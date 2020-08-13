-- module Spec where

import Data.Char
import Test.Hspec
import Test.QuickCheck

import Lib as L
import Compiler as Comp
import Execution as Exec
import Instructions as I

instance Arbitrary Memory where
  arbitrary = do
    left  <- listOf $ elements [0..255]
    right <- (listOf $ elements [0..255]) `suchThat` ((> 0) . length)
    ptr   <- arbitrary
    return $ Memory left right ptr

capToOneByte :: Integral a => a -> a
capToOneByte = abs . (`mod` 256)

instructionsTest :: IO ()
instructionsTest = hspec $ do
  describe "goForwards" $ do
  
    it "can be reversed by goBackwards (identity)" $ property $
      \mem -> (goForwards $ goBackwards mem) =:= mem

    it "has no prefered order with goBackwards" $ property $
      \mem -> (goBackwards $ goBackwards $ goForwards mem) =:= (goForwards $ goBackwards $ goBackwards mem)
    
    it "wont change the content of the memory" $ property $
      \(mem, iters) -> all (==mem) $ take (getPositive iters) $ iterate goForwards mem
    
    it "increments the memory pointer position by one" $ property $
      \mem -> memoryPointer (goForwards mem) == (memoryPointer mem + 1)
  
  describe "goBackwards" $ do

    it "can be reversed by goForwards (identity)" $ property $
      \mem -> (goBackwards $ goForwards mem) =:= mem
    
    it "wont change the content of the memory" $ property $
      \(mem, n) -> let iters = getPositive n in
                   all (==mem) $ take iters $ iterate goBackwards mem
    
    it "will increase the size of the memory as needed" $ property $
      \n -> let iters = getPositive n in
            let mem = listToMemory 0 [] in
            (memorySize $ iterate goBackwards mem !! iters) === iters

    it "decrements the memory pointer position by one" $ property $
      \mem -> memoryPointer (goBackwards mem) === (memoryPointer mem - 1)
  
  describe "increment" $ do

    it "can be reversed with decrement" $ property $
      \mem -> (increment $ decrement mem) =:= mem
    
    it "has no prefered order with decrement" $ property $
      \mem -> (increment $ decrement $ increment mem) =:= (decrement $ increment $ increment mem)
    
    it "will not change the position of the memory pointer" $ property $
      \(mem, n) -> let iters = getPositive n in
                   all ((== memoryPointer mem) . memoryPointer) $ take iters $ iterate increment mem
    
    it "increments by one the value at the memory pointer" $ property $
      \mem -> readMemoryHead (increment mem) === capToOneByte (readMemoryHead mem + 1)
    
    it "will overflow to 0 if it were to go above 255" $ property $
      (readMemoryHead $ increment $ Memory [] [255] 0) === 0
  
  describe "decrement" $ do

    it "can be reversed with increment" $ property $
      \mem -> (decrement $ increment mem) =:= mem
    
    it "will not change the position of the memory pointer" $ property $
      \(mem, n) -> let iters = getPositive n in
                       all ((== memoryPointer mem) . memoryPointer) $ take iters $ iterate decrement mem
    
    it "decrements by one the value at the memory pointer" $ property $
      \mem -> readMemoryHead (decrement mem) === capToOneByte (readMemoryHead mem - 1)
    
    it "will underflow to 255 if it were to go below 0" $ property $
      (readMemoryHead $ decrement $ Memory [] [0] 0) === 255
  
  describe "input" $ do

    it "will write a char to memory" $ property $
      \(mem, c) -> readMemoryHead (input c mem) === ord c
    
  describe "output" $ do

    it "will read a char from memory" $ property $
      \c -> (I.output $ listToMemory 0 [ord c]) === c
    
    it "will read a char that has been written" $ property $
      \(mem, c) -> (I.output $ input c mem) === c

main :: IO ()
main = do instructionsTest