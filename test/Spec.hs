-- module Spec where

import Data.Char
import Data.Word
import Data.ByteString.Internal (w2c)
import Test.Hspec
import Test.QuickCheck

import Brainfuck.Compiler as Comp
import Brainfuck.Execution as Exec
import Brainfuck.Instructions as I

-- Type alias for the current memory type beeing tested.
type MemoryImplementation = IntMapMemory

instance Arbitrary Memory where
  arbitrary = do
    left  <- listOf $ elements [0..255]
    right <- (listOf $ elements [0..255]) `suchThat` ((> 0) . length)
    ptr   <- arbitrary
    return $ Memory left right ptr

instance Arbitrary IntMapMemory where
  arbitrary = do
    lst <- (listOf $ elements [0..255]) `suchThat` ((> 0) . length)
    ptr <- arbitrary `suchThat` (\x -> x >= 0 && x < length lst)
    return $ listToMemory ptr lst

capToOneByte :: Integral a => a -> a
capToOneByte = abs . (`mod` 256)

protoMemoryTest :: Spec
protoMemoryTest = do
  describe "listToMemory" $ do

    it "can be reversed by memoryToList" $ property $
      \(lst :: [Word8]) -> let mem = listToMemory 0 lst :: MemoryImplementation in
                           memoryToList mem === lst
    
  describe "memoryToList" $ do

    it "can be reversed by listToMemory 0" $ property $
      \(mem :: MemoryImplementation) -> listToMemory 0 (memoryToList mem) === mem
  
  describe "emptyMemory" $ do

    it "has size 1" $ property $
      memorySize (emptyMemory :: MemoryImplementation) === 1
  
  describe "readMemoryHead" $ do

    it "should read the memory cell at the head pointer" $ property $
      \(lst :: [Word8]) -> length lst > 0 ==>
        let mem = listToMemory 0 lst :: MemoryImplementation in
        readMemoryHead mem == head lst
    
  describe "moveHead" $ do

    it "should move the memory head pointer by a certain offset" $ property $
      \(mem :: MemoryImplementation, off :: Int) -> memoryPointer (moveHead off mem) === (memoryPointer mem + off)
  
  describe "modifyMemoryHead" $ do

    it "should modify the memory head cell to any value (word8)" $ property $
      \(mem :: MemoryImplementation, change :: Word8) -> readMemoryHead (modifyMemoryHead mem (\_ -> change)) === change

instructionsTest :: Spec
instructionsTest = do
  describe "goForwards" $ do
  
    it "can be reversed by goBackwards (identity)" $ property $
      \(mem :: MemoryImplementation) -> (goForwards $ goBackwards mem) =:= mem

    it "has no prefered order with goBackwards" $ property $
      \(mem :: MemoryImplementation) -> (goBackwards $ goBackwards $ goForwards mem) =:= (goForwards $ goBackwards $ goBackwards mem)
    
    it "wont change the content of the memory" $ property $
      \(mem :: MemoryImplementation, iters) -> all (==mem) $ take (getPositive iters) $ iterate goForwards mem
    
    it "increments the memory pointer position by one" $ property $
      \(mem :: MemoryImplementation) -> memoryPointer (goForwards mem) === (memoryPointer mem + 1)
  
  describe "goBackwards" $ do

    it "can be reversed by goForwards (identity)" $ property $
      \(mem :: MemoryImplementation) -> (goBackwards $ goForwards mem) =:= mem
    
    it "wont change the content of the memory" $ property $
      \(mem :: MemoryImplementation, n) -> let iters = getPositive n in
                   all (==mem) $ take iters $ iterate goBackwards mem
    
    it "will increase the size of the memory as needed" $ property $
      \n -> let iters = getPositive n in
            let mem = listToMemory 0 [] :: MemoryImplementation in
            -- The increment is used in order to make sure the memory at the pointer is changed
            -- therefore forcing the utilization (allocation) of that memory cell.
            (memorySize $ iterate (increment . goBackwards) mem !! iters) === iters

    it "decrements the memory pointer position by one" $ property $
      \(mem :: MemoryImplementation) -> memoryPointer (goBackwards mem) === (memoryPointer mem - 1)
  
  describe "increment" $ do

    it "can be reversed with decrement" $ property $
      \(mem :: MemoryImplementation) -> (increment $ decrement mem) =:= mem
    
    it "has no prefered order with decrement" $ property $
      \(mem :: MemoryImplementation) -> (increment $ decrement $ increment mem) =:= (decrement $ increment $ increment mem)
    
    it "will not change the position of the memory pointer" $ property $
      \(mem :: MemoryImplementation, n) -> let iters = getPositive n in
                   all ((== memoryPointer mem) . memoryPointer) $ take iters $ iterate increment mem
    
    it "increments by one the value at the memory pointer" $ property $
      \(mem :: MemoryImplementation) -> readMemoryHead (increment mem) === capToOneByte (readMemoryHead mem + 1)
    
    it "will overflow to 0 if it were to go above 255" $ property $
      let mem = listToMemory 0 [255] :: MemoryImplementation in
      (readMemoryHead $ increment mem) === 0
  
  describe "decrement" $ do

    it "can be reversed with increment" $ property $
      \(mem :: MemoryImplementation) -> (decrement $ increment mem) =:= mem
    
    it "will not change the position of the memory pointer" $ property $
      \(mem :: MemoryImplementation, n) -> let iters = getPositive n in
                       all ((== memoryPointer mem) . memoryPointer) $ take iters $ iterate decrement mem
    
    it "decrements by one the value at the memory pointer" $ property $
      \(mem :: MemoryImplementation) -> readMemoryHead (decrement mem) === capToOneByte (readMemoryHead mem - 1)
    
    it "will underflow to 255 if it were to go below 0" $ property $
      let mem = listToMemory 0 [0] :: MemoryImplementation in
      (readMemoryHead $ decrement $ mem) === 255
  
  describe "input" $ do

    it "will write a char to memory" $ property $
      \(mem :: MemoryImplementation, w :: Word8) -> readMemoryHead (input (w2c w) mem) === ord (w2c w)
    
  describe "output" $ do

    it "will read a char from memory" $ property $
      \(w :: Word8) -> (I.output $ (listToMemory 0 [ord $ w2c w] :: MemoryImplementation)) === w2c w
    
    it "will read a char that has been written" $ property $
      \(mem :: MemoryImplementation, w :: Word8) -> I.output (input (w2c w) mem) === w2c w

main :: IO ()
main = hspec $ do
  protoMemoryTest
  instructionsTest