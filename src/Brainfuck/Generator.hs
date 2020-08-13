module Brainfuck.Generator where

import Text.Printf

-- lowestBit :: Int -> Int
-- lowestBit n
--   | n == 0 = 0
--   | odd n = 1
--   | otherwise = 1 + lowestBit (n `div` 2)

compNum :: Int -> String
compNum 0 = ""
compNum n = go n 1
  where go 1 d = inner 1 d
        go n depth = printf "%s>++[%s-]<" (inner n depth) $ go (n `div` 2) (depth + 1)
        inner n d = if odd n then printf "%s+%s" (open (d-1)) (close (d-1)) else ""
        open  d = replicate d '<'
        close d = replicate d '>'

