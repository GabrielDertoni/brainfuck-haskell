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

memLimit = 30000 -- bytes
timeLimit = 30 -- seconds
frozenTimeLimit = 120 -- seconds

-- TODO: implement a more robust catch error system (MonadThrow, MonadCatch, etc.)

data System = System { start_time :: UTCTime
                     , froze_time :: Maybe UTCTime
                     , offset_time :: NominalDiffTime
                     , input_buffer :: String
                     , output_buffer :: String
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
                                 , read_fn = (:[]) <$> getChar
                                 , write_fn = putStr
                                 }

executionTime :: System -> IO NominalDiffTime
executionTime sys
  = do time <- getCurrentTime
       off  <- offsetTime sys
       let execTime = diffUTCTime time (start_time sys)
       return (execTime - off)

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

data Environment
    = Environment { memory :: Memory
                  , system :: System
                  }

defaultEnv :: IO Environment
defaultEnv = do sys <- defaultSystem
                return $ Environment emptyMemory sys

data ExecuteM a
  = ExecuteM { execute :: Environment -> IO (Either String (a, Environment)) }

instance Functor ExecuteM where
  fmap f x = x >>= (pure . f)

instance Applicative ExecuteM where
  pure a = ExecuteM (\state -> return $ Right (a, state))
  (<*>) = ap

ioErrHandle :: SomeException -> IO (Either String a)
ioErrHandle e = return $ Left $ show e

instance Monad ExecuteM where
  return a = ExecuteM (\state -> handleEnvErrors $ return $ Right (a, state))
  (>>=) (ExecuteM exec) f = ExecuteM $
    \state -> handle ioErrHandle $
      do res <- exec state
         case res of
           Right (a, new_state) -> handleEnvErrors $ execute (f a) new_state
           Left err             -> return $ Left err

instance MonadIO ExecuteM where
  liftIO action = ExecuteM $
    \state -> do a <- action
                 return $ Right (a, state)

instance MonadFail ExecuteM where
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


actionToExec :: (Memory -> Memory) -> ExecuteM ()
actionToExec action = ExecuteM $ \env -> return $ Right ((), env { memory = action $ memory env })

handleEnvErrors :: IO (Either String (a, Environment)) -> IO (Either String (a, Environment))
handleEnvErrors inp
  = do state <- inp
       case state of
         Right (a, env) -> do checks <- checkErrors env
                              case checks of
                                []   -> return $ Right (a, env)
                                errs -> return $ Left $ init $ unlines errs

         Left errs      -> return $ Left errs

checkErrors :: Environment -> IO [String]
checkErrors env = catMaybes <$> sequence (checkList <*> pure env)

checkList :: [Environment -> IO (Maybe String)]
checkList = [ checkOutOfMemory
            , checkTimeout
            , checkSegFault
            -- , checkWaitTimeout
            ]

checkOutOfMemory :: Environment -> IO (Maybe String)
checkOutOfMemory env
  | (memorySize $ memory env) > memLimit = return $ Just "Memory limit exceded."
  | otherwise = return Nothing

checkTimeout :: Environment -> IO (Maybe String)
checkTimeout env = do time <- executionTime (system env)
                      if time > secondsToNominalDiffTime timeLimit
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

checkSegFault :: Environment -> IO (Maybe String)
checkSegFault env
  | (right $ memory env) == [] = return $ Just "Segmentation fault."
  | otherwise = return Nothing

kill :: ExecuteM ()
kill = ExecuteM $ \_ -> return $ Left "Program terminated by user.\n"

execFreeze :: ExecuteM ()
execFreeze = ExecuteM $
  \env -> do time <- getCurrentTime
             return $ Right ((), env { system = (system env) { froze_time = Just time } } )

execUnFreeze :: ExecuteM ()
execUnFreeze = ExecuteM $
  \env -> do time <- frozenTime (system env)
             case time of
               Nothing -> execute (pure ()) env
               Just t  -> let off = offset_time $ system env in
                          let sys = (system env) { offset_time = off + t, froze_time = Nothing } in
                          return $ Right ((), env { system = sys } )

execGetFreezeTime :: ExecuteM (Maybe NominalDiffTime)
execGetFreezeTime = ExecuteM $
  \env -> do time <- frozenTime (system env)
             return $ Right (time, env)

freezerExecution :: ExecuteM a -> ExecuteM a
freezerExecution exec = do execFreeze
                           v <- exec
                           execUnFreeze
                           return v

execGetChar :: ExecuteM Char
execGetChar = ExecuteM $
  \env -> let sys  = system env in
          case input_buffer $ system env of
            []     -> do v <- timeout (frozenTimeLimit*(10^6)) (read_fn sys)
                         case v of
                           Nothing     -> return $ Left $ "Frozen time limit exceded."
                           Just (x:xs) -> return $ pure (x, env { system = sys { input_buffer = xs } })
            
            (x:xs) -> return $ pure (x, env { system = sys { input_buffer = xs }})

execPutChar :: Char -> ExecuteM ()
execPutChar c = ExecuteM $
  \env -> let sys = system env in
          let out = output_buffer sys in
          return $ pure ((), env { system = sys { output_buffer = c:out } })

execOutputBuffer :: ExecuteM ()
execOutputBuffer = ExecuteM $
  \env -> do let sys = system env
             let out = output_buffer sys
             if length out > 0
               then do write_fn sys $ reverse out
                       return $ pure ((), env { system = sys { output_buffer = [] } })
               else return $ pure ((), env)

execGetOutputBuffer :: ExecuteM String
execGetOutputBuffer = ExecuteM $
  \env -> do let sys = system env
             return $ pure (output_buffer sys, env)


execReadMemory :: ExecuteM Memory
execReadMemory = ExecuteM $ \env -> return $ Right (memory env, env)