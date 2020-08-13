module Lib where

import System.Environment
import Text.ParserCombinators.ReadP
import Text.Printf
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Data.Time

import Compiler
import Instructions
import Execution

handleParseFailure :: IO ()
handleParseFailure = putStrLn "Failed to parse"

noMonad :: IO ()
noMonad = do contents <- getLine
             let s = readP_to_S instructions contents
             print (length s)
             maybe handleParseFailure (runProgram . fst) $ listToMaybe $ reverse s
  
  where runProgram :: [Instruction] -> IO ()
        runProgram instructions = do putStrLn "Execution started."
                                     sTime <- getCurrentTime
                                     mem   <- go emptyMemory instructions
                                     eTime <- getCurrentTime
                                     let time = nominalDiffTimeToSeconds $ diffUTCTime eTime sTime
                                     putStrLn $ printf "\nFinished execution. Took %s seconds" (show time)
                                     print mem

        go :: Memory -> [Instruction] -> IO Memory
        go mem [] = return mem
        go mem (x:xs)
          = case x of
              Increment -> go (increment mem) xs

              Decrement -> go (decrement mem) xs

              Forward   -> go (goForwards mem) xs

              Backward  -> go (goBackwards mem) xs

              Input     -> do c <- getChar
                              go (input c mem) xs

              Output    -> do let c = output mem
                              putChar c
                              go mem xs

              Loop inst -> if readMemoryHead mem == 0
                            then go mem xs
                            else do mem' <- go mem inst
                                    go mem' (x:xs)

run :: IO ()
run = do [progFile] <- getArgs
         contents   <- readFile progFile
         case parseInstructions contents of
           Left err        -> putStrLn err
           Right instructs -> runProgram instructs

  where runProgram :: [Instruction] -> IO ()
        runProgram instructs
          = do putStrLn "Execution started."
               env   <- defaultEnv
               sTime <- getCurrentTime
               res   <- execute (go instructs) env
               eTime <- getCurrentTime
               let time = nominalDiffTimeToSeconds $ diffUTCTime eTime sTime
               case res of
                 Left err       -> printf "Error(s):\n%sTook %s seconds\n" err (show time)
                 Right (_, env) -> do putStrLn $ printf "\nFinished execution. Took %s seconds" (show time)
                                      print $ memory env

        go :: [Instruction] -> ExecuteM ()
        go [] = return ()
        go (x:xs)
          = do m <- execReadMemory
               case x of
                Increment -> execIncrement
                Decrement -> execDecrement
                Forward   -> execGoForwards
                Backward  -> execGoBackwards
                Input     -> execInput
                Output    -> execOutput
                Loop inst -> do mem <- execReadMemory
                                if readMemoryHead mem /= 0
                                  then do go inst
                                          go [x]
                                  else return ()

               go xs

