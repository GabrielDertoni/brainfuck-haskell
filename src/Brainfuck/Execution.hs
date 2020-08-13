module Brainfuck.Execution where

import GHC.Base (ap, liftM)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe
import Data.Char
import Data.Time
import Text.Printf

import Brainfuck.Instructions
import Brainfuck.Compiler

memLimit = 30000
timeLimit = secondsToNominalDiffTime 10
frozenTimeLimit = secondsToNominalDiffTime 120

data System = System { start_time :: UTCTime
                     , froze_time :: Maybe UTCTime
                     , offset_time :: NominalDiffTime
                     }

defaultSystem :: IO System
defaultSystem = do time <- getCurrentTime
                   return System { start_time = time
                                 , froze_time = Nothing
                                 , offset_time = secondsToNominalDiffTime 0
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

instance Monad ExecuteM where
  return a = ExecuteM (\state -> handleEnvErrors $ return $ Right (a, state))
  (>>=) (ExecuteM exec) f
    = ExecuteM (\state -> do res <- exec state
                             case res of
                              Right (a, new_state) -> handleEnvErrors $ execute (f a) new_state
                              Left err             -> return $ Left err)

instance MonadIO ExecuteM where
  liftIO action = ExecuteM $
    \state -> do a <- action
                 return $ Right (a, state)


actionToExec :: (Memory -> Memory) -> ExecuteM ()
actionToExec action = ExecuteM $ \env -> return $ Right ((), env { memory = action $ memory env })

handleEnvErrors :: IO (Either String (a, Environment)) -> IO (Either String (a, Environment))
handleEnvErrors inp
  = do state <- inp
       case state of
         Right (a, env) -> do checks <- checkErrors env
                              case checks of
                                []   -> return $ Right (a, env)
                                errs -> return $ Left $ unlines errs

         Left errs      -> return $ Left errs

checkErrors :: Environment -> IO [String]
checkErrors env = catMaybes <$> sequence (checkList <*> pure env)

checkList :: [Environment -> IO (Maybe String)]
checkList = [ checkOutOfMemory
            , checkTimeout
            , checkSegFault
            ]

checkOutOfMemory :: Environment -> IO (Maybe String)
checkOutOfMemory env
  | (memorySize $ memory env) > memLimit = return $ Just "Memory limit exceded."
  | otherwise = return Nothing

checkTimeout :: Environment -> IO (Maybe String)
checkTimeout env = do time <- executionTime (system env)
                      if time > timeLimit
                        then return $ Just "Time limit exceded."
                        else return Nothing

checkWaitTimeout :: Environment -> IO (Maybe String)
checkWaitTimeout Environment { system = sys }
  | froze_time sys == Nothing = return Nothing
  | otherwise = do time <- frozenTime sys
                   case time of
                     Nothing -> return Nothing
                     Just t  -> if t > frozenTimeLimit
                                  then return $ Just "Frozen time limit exceded."
                                  else return Nothing

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

freezerExecution :: ExecuteM a -> ExecuteM a
freezerExecution exec = do execFreeze
                           v <- exec
                           execUnFreeze
                           return v

execReadMemory :: ExecuteM Memory
execReadMemory = ExecuteM $ \env -> return $ Right (memory env, env)

execGoForwards :: ExecuteM ()
execGoForwards = actionToExec goForwards

execGoBackwards :: ExecuteM ()
execGoBackwards = actionToExec goBackwards

execIncrement :: ExecuteM ()
execIncrement = actionToExec increment

execDecrement :: ExecuteM ()
execDecrement = actionToExec decrement

execInput :: ExecuteM ()
execInput = do c <- liftIO $ getChar
               if ord c == 27
                 then kill
                 else actionToExec $ input c

execOutput :: ExecuteM ()
execOutput = do mem <- execReadMemory
                liftIO $ putChar $ output mem


interpret :: [Instruction] -> ExecuteM ()
interpret [] = return ()
interpret (x:xs)
  = do m <- execReadMemory
       case x of
        Increment -> execIncrement
        Decrement -> execDecrement
        Forward   -> execGoForwards
        Backward  -> execGoBackwards
        Input     -> execInput
        Output    -> execOutput
        Loop inst -> do mem <- execReadMemory
                        when (readMemoryHead mem /= 0) $ do
                          interpret inst
                          interpret [x]

       interpret xs

runProgram :: String -> IO ()
runProgram prog = do 
  case parseInstructions prog of
    Left err        -> putStrLn err
    Right instructs -> logRun instructs

  where logRun :: [Instruction] -> IO ()
        logRun instructs
          = do putStrLn "Execution started."
               env   <- defaultEnv
               sTime <- getCurrentTime
               res   <- execute (interpret instructs) env
               eTime <- getCurrentTime
               let time = nominalDiffTimeToSeconds $ diffUTCTime eTime sTime
               case res of
                 Left err       -> printf "Error(s):\n%sTook %s seconds\n" err (show time)
                 Right (_, env) -> do putStrLn $ printf "\nFinished execution. Took %s seconds" (show time)
                                      print $ memory env