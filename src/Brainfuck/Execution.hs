module Brainfuck.Execution where

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
import Brainfuck.ProtoOS


execGoForwards :: ProtoMemory a => ExecuteM a ()
execGoForwards = do
  actionToExec goForwards
  env <- execGetEnv
  when (memorySize (memory env) > mem_limit (system env)) $
    fail "Memory limit exceded."

execGoBackwards :: ProtoMemory a => ExecuteM a ()
execGoBackwards = do
  actionToExec goBackwards
  env <- execGetEnv
  when (memorySize (memory env) > mem_limit (system env)) $
    fail "Memory limit exceded."

execIncrement :: ProtoMemory a => ExecuteM a ()
execIncrement = actionToExec increment

execDecrement :: ProtoMemory a => ExecuteM a ()
execDecrement = actionToExec decrement

execInput :: ProtoMemory a => ExecuteM a ()
execInput = freezerExecution $ do
              buff <- execGetInputBuffer
              -- Only outputs when input needs to be received (or program ends).
              when (length buff == 0) execOutputBuffer
              c <- execGetChar
              if ord c == 27
                then kill
                else actionToExec $ input c

execOutput :: ProtoMemory a => ExecuteM a ()
execOutput = do mem <- execReadMemory
                execPutChar $ output mem

execTimeout :: ProtoMemory a => Int -> ExecuteM a () -> ExecuteM a ()
execTimeout limit exec = ExecuteM $
  \env -> do res <- timeout (limit * (10^6)) $ execute exec env
             case res of
               Nothing -> return $ Left "Time limit exceded."
               Just v  -> return v

interpret :: ProtoMemory a => [Instruction] -> ExecuteM a ()
interpret [] = return ()
interpret instructs = do env <- execGetEnv
                         execTimeout (time_limit $ system env) $ runInstructions instructs
                         buff <- execGetOutputBuffer
                         execOutputBuffer

runInstructions :: ProtoMemory a => [Instruction] -> ExecuteM a ()
runInstructions [] = return ()
runInstructions (!x:xs)
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
                          runInstructions inst
                          runInstructions [x]

       runInstructions xs

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
                 Left err       -> printf "Error(s):\n%s\nTook %s seconds\n" err (show time)
                 Right (_, env) -> do putStrLn $ printf "\nFinished execution. Took %s seconds" (show time)
                                      -- print $ memory env