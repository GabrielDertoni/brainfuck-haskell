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


execGoForwards :: ExecuteM ()
execGoForwards = actionToExec goForwards

execGoBackwards :: ExecuteM ()
execGoBackwards = actionToExec goBackwards

execIncrement :: ExecuteM ()
execIncrement = actionToExec increment

execDecrement :: ExecuteM ()
execDecrement = actionToExec decrement

execInput :: ExecuteM ()
execInput = freezerExecution $ do
              buff <- execGetInputBuffer
              -- Only outputs when input needs to be received (or program ends).
              when (length buff == 0) execOutputBuffer
              c <- execGetChar
              if ord c == 27
                then kill
                else actionToExec $ input c

execOutput :: ExecuteM ()
execOutput = do mem <- execReadMemory
                execPutChar $ output mem

interpret :: [Instruction] -> ExecuteM ()
interpret [] = return ()
interpret instructs = do runInstructions instructs
                         buff <- execGetOutputBuffer
                         execOutputBuffer

runInstructions :: [Instruction] -> ExecuteM ()
runInstructions [] = return ()
runInstructions (x:xs)
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
                                      print $ memory env