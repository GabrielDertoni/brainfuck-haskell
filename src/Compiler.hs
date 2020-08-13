module Compiler where

import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative ((<|>), some)
import Text.Printf

data Instruction
  = Increment
  | Decrement
  | Forward
  | Backward
  | Input
  | Output
  | Loop [Instruction]
  deriving (Eq, Show)

reservedChars :: [Char]
reservedChars = [ '+'
                , '-'
                , '>'
                , '<'
                , ','
                , '.'
                , '['
                , ']'
                ]

space :: ReadP String
space = many $ satisfy isSpace

ignore :: ReadP String
-- ignore = many $ satisfy (`notElem` reservedChars)
ignore = readS_to_P $ (\inp -> [span (`notElem` reservedChars) inp])

token :: ReadP a -> ReadP a
token pars = do ignore
                v <- pars
                ignore
                return v

incrementInstruction :: ReadP Instruction
incrementInstruction = token (char '+') *> return Increment

decrementInstruction :: ReadP Instruction
decrementInstruction = token (char '-') *> return Decrement

forwardInstruction :: ReadP Instruction
forwardInstruction = token (char '>') *> return Forward

backwardInstruction :: ReadP Instruction
backwardInstruction = token (char '<') *> return Backward

inputInstruction :: ReadP Instruction
inputInstruction = token (char ',') *> return Input

outputInstruction :: ReadP Instruction
outputInstruction = token (char '.') *> return Output

loopInstruction :: ReadP Instruction
loopInstruction = token $ do char '['
                             inst <- instructions
                             char ']'
                             return $ Loop inst

instruction :: ReadP Instruction
instruction = incrementInstruction
          <|> decrementInstruction
          <|> forwardInstruction
          <|> backwardInstruction
          <|> inputInstruction
          <|> outputInstruction
          <|> loopInstruction

instructions :: ReadP [Instruction]
instructions = many instruction

getParseError :: String -> String -> String
getParseError contents unparsed
  = printf "Parse error on %sline %d col %d." c nline ncol
  where c = if unparsed == [] then "" else printf "'%c', " $ head unparsed
        ncol = length line + 1
        line = if lines parsed == [] then "" else last $ lines parsed
        nline = (length $ lines parsed) + 1
        parsed = take (length contents - length unparsed) contents

parseInstructions :: String -> Either String [Instruction]
parseInstructions contents
  | parsed == [] = Left "Parse error on line 0 col 0."
  | (snd $ last parsed) /= "" = Left $ getParseError contents $ snd $ last parsed
  | otherwise = Right $ fst $ last parsed
  where parsed = readP_to_S instructions contents