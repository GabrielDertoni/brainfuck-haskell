module Brainfuck.Instructions where

import Data.Char
import Text.Printf
import GHC.Exts (IsList)

import qualified Data.IntMap.Lazy as Map
import           Data.Word

import Brainfuck.Compiler

class Identical a where
  (=:=) :: a -> a -> Bool

class ProtoMemory a where
  memoryToList   :: Integral b => a -> [b]
  
  listToMemory   :: Integral b => Int -> [b] -> a
  
  memorySize     :: a -> Int
  memorySize m = length $ memoryToList m
  
  emptyMemory    :: a
  emptyMemory = listToMemory 0 [0]

  -- Reads the memory at the head position.
  readMemoryHead :: Integral b => a -> b
  
  -- Gets the position of the head pointer.
  memoryPointer  :: a -> Int

  -- Moves the pointer position by a certain offset
  moveHead :: Int -> a -> a
  
  modifyMemoryHead :: Integral b => a -> (b -> b) -> a

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

instance ProtoMemory Memory where
  memoryPointer = pointerPosition
  
  listToMemory off ilst = Memory (reverse $ take off lst) (drop off lst) off
    where lst = fromIntegral <$> ilst
  
  memoryToList (Memory left right _) = fromIntegral <$> (reverse left <> right)
  
  readMemoryHead mem
    | null (right mem) = 0
    | otherwise = fromIntegral $ head $ right mem

  modifyMemoryHead (Memory left [] ptr) f
    = Memory left (fromIntegral (f 0) : []) ptr
  modifyMemoryHead (Memory left (x:xs) ptr) f
    = Memory left (fromIntegral (f $ fromIntegral x) : xs) ptr
  
  moveHead off (Memory left right ptr)
    | off > 0 && null right = moveHead (off - 1) $ Memory (0:left) [0] (ptr + 1)
    | off > 0               = moveHead (off - 1) $ Memory (head right : left) (tail right) (ptr + 1)
    | off < 0 && null left  = moveHead (off + 1) $ Memory [] (0:right) (ptr - 1)
    | off < 0               = moveHead (off + 1) $ Memory (tail left) (head left : right) (ptr - 1)
    | otherwise = Memory left right ptr

data IntMapMemory = IntMapMemory { memMap :: Map.IntMap Word8
                                 , headPosition :: Int
                                 , memSize :: Integer
                                 }

instance Show IntMapMemory where
  show mem = printf "%d | %s" (headPosition mem) (show $ memMap mem)

instance Eq IntMapMemory where
  (==) a b = memMap a == memMap b

instance Identical IntMapMemory where
  (=:=) a b = (headPosition a == headPosition b) && (memMap a == memMap b)

instance ProtoMemory IntMapMemory where
  memoryPointer = headPosition

  listToMemory off lst
    = IntMapMemory { memMap = Map.fromList $ zip [0..] $ fromIntegral <$> lst
                   , headPosition = off
                   , memSize = fromIntegral $ length lst
                   }
  
  memoryToList = map fromIntegral . snd . unzip . Map.toList . memMap

  -- memorySize = Map.size . memMap
  memorySize = fromIntegral . memSize

  readMemoryHead mem = maybe 0 id lk
    where lk = fromIntegral <$> Map.lookup (headPosition mem) (memMap mem)
  
  moveHead off mem = mem { headPosition = headPosition mem + off }

  modifyMemoryHead mem f = case lk of
    Nothing -> mem { memMap = Map.insert hp (fromIntegral (f 0)) mp
                    , memSize = memSize mem + 1 }
    Just x  -> mem { memMap = Map.insert hp (fromIntegral (f x)) mp }
    where mp = memMap mem
          hp = headPosition mem
          -- fx = f $ readMemoryHead mem
          lk = fromIntegral <$> Map.lookup (headPosition mem) (memMap mem)

trimMemory :: ProtoMemory a => a -> a
trimMemory mem = listToMemory (memoryPointer mem) $ trimList $ memoryToList mem

trimList :: [Int] -> [Int]
trimList = dropWhile (==0) . reverse . dropWhile (==0) . reverse

goForwards :: ProtoMemory a => a -> a
goForwards = moveHead 1

goBackwards :: ProtoMemory a => a -> a
goBackwards = moveHead (-1)

increment :: ProtoMemory a => a -> a
increment mem = modifyMemoryHead mem $ \x -> if x + 1 < 256 then x + 1 else 0

decrement :: ProtoMemory a => a -> a
decrement mem = modifyMemoryHead mem $ \x -> if x - 1 >= 0 then x - 1 else 255

input :: ProtoMemory a => Char -> a -> a
input inp mem = modifyMemoryHead mem $ \_ -> ord inp

output :: ProtoMemory a => a -> Char
output = chr . readMemoryHead