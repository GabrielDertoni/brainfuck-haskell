module Brainfuck.ProtoOS where

import GHC.Base (ap, liftM)
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe
import Data.Char
import Data.Time
import Data.Fixed
import System.Timeout
import Text.Printf

import Brainfuck.Instructions
import Brainfuck.Compiler

toPico :: Int -> Pico
toPico num = MkFixed $ (10^12) * fromIntegral num

-- TODO: implement a more robust catch error system (MonadThrow, MonadCatch, etc.)

data System = System { start_time :: UTCTime -- time program started
                     , froze_time :: Maybe UTCTime -- time freeze started
                     , offset_time :: NominalDiffTime -- sum of frozen times
                     , input_buffer :: String
                     , output_buffer :: String
                     , mem_limit :: Int -- bytes
                     , time_limit :: Int -- seconds
                     , frozen_time_limit :: Int -- seconds
                     , read_fn :: IO String
                     , write_fn :: String -> IO ()
                     }

defaultSystem :: IO System
defaultSystem = do time <- getCurrentTime
                   return System { start_time = time
                                 , froze_time = Nothing
                                 , offset_time = secondsToNominalDiffTime 0
                                 , input_buffer = ""
                                 , output_buffer = ""
                                 , mem_limit = 30000
                                 , time_limit = 30
                                 , frozen_time_limit = 120
                                 , read_fn = (<> "\n") <$> getLine
                                 , write_fn = putStr
                                 }

executionTime :: System -> IO NominalDiffTime
executionTime sys
  = do time <- getCurrentTime
       off  <- offsetTime sys
       let execTime = diffUTCTime time (start_time sys)
       return $! (execTime - off)

frozenTime :: System -> IO (Maybe NominalDiffTime)
frozenTime System { froze_time = t }
  = do time <- getCurrentTime
       return (diffUTCTime time <$> t)

offsetTime :: System -> IO NominalDiffTime
offsetTime sys
  = do freez <- frozenTime sys
       case freez of
         Nothing -> return $ offset_time sys
         Just t  -> return (offset_time sys + t)

data Environment a
    = Environment { memory :: a
                  , system :: System
                  }

instance Show (Environment a) where
  show env = "Environment"

type Env = Environment IntMapMemory
-- type Env = Environment Memory

defaultEnv :: IO Env
defaultEnv = do sys <- defaultSystem
                return $ Environment emptyMemory sys

data ExecuteM a b
  = ExecuteM { execute :: Environment a -> IO (Either String (b, Environment a)) }

instance ProtoMemory a => Functor (ExecuteM a) where
  fmap f x = x >>= (pure . f)

instance ProtoMemory a => Applicative (ExecuteM a) where
  pure a = ExecuteM (\state -> return $ Right (a, state))
  (<*>) = ap

ioErrHandle :: IOException -> IO (Either String a)
ioErrHandle e = do print e
                   return $ Left "Internal error."

instance ProtoMemory a => Monad (ExecuteM a) where
  return = pure
  (>>=) (ExecuteM exec) f = ExecuteM $
    \state -> handle ioErrHandle $
      do res <- exec state
         case res of
           Right (a, new_state) -> execute (f a) new_state
           Left err             -> return $ Left err

instance ProtoMemory a => MonadIO (ExecuteM a) where
  liftIO action = ExecuteM $
    \state -> do a <- action
                 return $ Right (a, state)

instance ProtoMemory a => MonadFail (ExecuteM a) where
  fail err = ExecuteM $ \_ -> return $ Left err

{-
instance MonadThrow ExecuteM where
  throwM e = ExecuteM $ \_ -> return $ Left $ show e

instance MonadCatch ExecuteM where
  catch exec f = ExecuteM $
    \env -> do res <- exec env
               case res of
                 Right (a, new_state) -> return $ Right (a, new_state)
                 Left err             -> f err
-}


actionToExec :: ProtoMemory a => (a -> a) -> ExecuteM a ()
actionToExec action = ExecuteM $ \env -> return $ Right ((), env { memory = action $ memory env })

handleEnvErrors :: ProtoMemory a => IO (Either String (b, Environment a)) -> IO (Either String (b, Environment a))
handleEnvErrors inp
  = do state <- inp
       case state of
         Right (a, env) -> do checks <- checkErrors env
                              case checks of
                                []   -> return $ Right (a, env)
                                errs -> return $ Left $ init $ unlines errs

         Left errs      -> return $ Left errs

checkErrors :: ProtoMemory a => Environment a -> IO [String]
checkErrors env = catMaybes <$> sequence (checkList <*> pure env)

checkList :: ProtoMemory a => [Environment a -> IO (Maybe String)]
checkList = [ checkOutOfMemory
            , checkTimeout
            ]

checkOutOfMemory :: ProtoMemory a => Environment a -> IO (Maybe String)
checkOutOfMemory env
  | (memorySize $ memory env) > mem_limit (system env) = return $ Just "Memory limit exceded."
  | otherwise = return Nothing

checkTimeout :: ProtoMemory a => Environment a -> IO (Maybe String)
checkTimeout env = do time <- executionTime (system env)
                      if time > secondsToNominalDiffTime (toPico $ time_limit $ system env)
                        then return $ Just "Time limit exceded."
                        else return Nothing

{-
checkWaitTimeout :: Environment -> IO (Maybe String)
checkWaitTimeout Environment { system = sys }
  | froze_time sys == Nothing = return Nothing
  | otherwise = do time <- frozenTime sys
                   case time of
                     Nothing -> return Nothing
                     Just t  -> if t > secondsToNominalDiffTime frozenTimeLimit
                                  then return $ Just "Frozen time limit exceded."
                                  else return Nothing
-}

kill :: ProtoMemory a => ExecuteM a ()
kill = ExecuteM $ \_ -> return $ Left "Program terminated by user.\n"

execFreeze :: ProtoMemory a => ExecuteM a ()
execFreeze = ExecuteM $
  \env -> do time <- getCurrentTime
             return $ Right ((), env { system = (system env) { froze_time = Just time } } )

execUnFreeze :: ProtoMemory a => ExecuteM a ()
execUnFreeze = ExecuteM $
  \env -> do time <- frozenTime (system env)
             case time of
               Nothing -> execute (pure ()) env
               Just t  -> let off = offset_time $ system env in
                          let sys = (system env) { offset_time = off + t, froze_time = Nothing } in
                          return $ Right ((), env { system = sys } )

execGetFreezeTime :: ProtoMemory a => ExecuteM a (Maybe NominalDiffTime)
execGetFreezeTime = ExecuteM $
  \env -> do time <- frozenTime (system env)
             return $ Right (time, env)

freezerExecution :: ProtoMemory a => ExecuteM a b -> ExecuteM a b
freezerExecution exec = do execFreeze
                           v <- exec
                           execUnFreeze
                           return v

execGetChar :: ProtoMemory a => ExecuteM a Char
execGetChar = ExecuteM $
  \env -> let sys  = system env in
          case input_buffer $ system env of
            []     -> do v <- timeout (frozen_time_limit sys * (10^6)) (read_fn sys)
                         case v of
                           Nothing     -> return $ Left $ "Frozen time limit exceded."
                           Just (x:xs) -> return $ pure (x, env { system = sys { input_buffer = xs } })
            
            (x:xs) -> return $ pure (x, env { system = sys { input_buffer = xs }})

execPutChar :: ProtoMemory a => Char -> ExecuteM a ()
execPutChar c = ExecuteM $
  \env -> let sys = system env in
          let out = output_buffer sys in
          return $ pure ((), env { system = sys { output_buffer = c:out } })

execOutputBuffer :: ProtoMemory a => ExecuteM a ()
execOutputBuffer = ExecuteM $
  \env -> do let sys = system env
             let out = output_buffer sys
             if length out > 0
               then do write_fn sys $ reverse out
                       return $ pure ((), env { system = sys { output_buffer = [] } })
               else return $ pure ((), env)

execGetOutputBuffer :: ProtoMemory a => ExecuteM a String
execGetOutputBuffer = ExecuteM $
  \env -> do let sys = system env
             return $ pure (output_buffer sys, env)

execGetInputBuffer :: ProtoMemory a => ExecuteM a String
execGetInputBuffer = ExecuteM $
  \env -> do let sys = system env
             return $ pure (input_buffer sys, env)

execReadMemory :: ProtoMemory a => ExecuteM a a
execReadMemory = ExecuteM $ \env -> return $ Right (memory env, env)

execGetEnv :: ProtoMemory a => ExecuteM a (Environment a)
execGetEnv = ExecuteM $ \env -> return $ Right (env, env)