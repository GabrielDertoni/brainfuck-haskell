module Brainfuck.Instructions where

import Data.Char
import Text.Printf

import Brainfuck.Compiler

class Identical a where
  (=:=) :: a -> a -> Bool

data Memory = Memory { left :: [Int]
                     , right :: [Int]
                     , pointerPosition :: Int
                     }

instance Show Memory where
  show mem = printf "%d | %s|%s" ptr (show $ reverse left) (show right)
    where (Memory left right ptr) = mem

instance Eq Memory where
  (==) a b
    = (reverse lefta <> righta) == (reverse leftb <> rightb)
    where (Memory lefta righta _) = trimMemory a
          (Memory leftb rightb _) = trimMemory b

instance Identical Memory where
  (=:=) a b = trimMemory a == trimMemory b

memoryPointer :: Memory -> Int
memoryPointer = pointerPosition

trimMemory :: Memory -> Memory
trimMemory mem = listToMemory (memoryPointer mem) $ trimList $ memoryToList mem

trimList :: [Int] -> [Int]
trimList = dropWhile (==0) . reverse . dropWhile (==0) . reverse

listToMemory :: Int -> [Int] -> Memory
listToMemory off lst = Memory (reverse $ take off lst) (drop off lst) off

memoryToList :: Memory -> [Int]
memoryToList (Memory left right _) = reverse left <> right

memorySize :: Memory -> Int
memorySize (Memory left right _) = length left + length right

emptyMemory :: Memory
emptyMemory = Memory [] [0] 0

readMemoryHead :: Memory -> Int
readMemoryHead = head . right

goForwards :: Memory -> Memory
goForwards (Memory left []     ptr) = Memory (0:left) [0] (ptr + 1)
goForwards (Memory left (x:[]) ptr) = Memory (x:left) [0] (ptr + 1)
goForwards (Memory left (x:xs) ptr) = Memory (x:left) xs (ptr + 1)

goBackwards :: Memory -> Memory
goBackwards (Memory []     right ptr) = Memory [] (0:right) (ptr - 1)
goBackwards (Memory (x:xs) right ptr) = Memory xs (x:right) (ptr - 1)

increment :: Memory -> Memory
increment mem = if x + 1 < 256
                  then mem { right = x + 1 : xs}
                  else mem { right = 0 : xs }
  where (x:xs) = right mem

decrement :: Memory -> Memory
decrement mem = if x - 1 >= 0
                  then mem { right = x - 1 : xs }
                  else mem { right = 255 : xs}
  where (x:xs) = right mem

input :: Char -> Memory -> Memory
input inp mem = mem { right = ord inp : xs }
  where (x:xs) = right mem

output :: Memory -> Char
output = chr . head . right