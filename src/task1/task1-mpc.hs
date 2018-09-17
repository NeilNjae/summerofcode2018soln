{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')        -- import the strict fold

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec -- hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA


-- number of steps
type Distance = Int
type Position = (Int, Int)

-- the directions. See below for functions for turning
data Direction = North | East | South | West 
    deriving (Enum, Show, Bounded, Eq)

-- direction, easting, northing
data Mowmaster = Mowmaster { direction :: Direction 
                           , position :: Position
                           } deriving (Show, Eq)

-- one instruction for the mowmaster
data Instruction =   Forward Distance
                   | Clockwise 
                   | Anticlockwise
                   | Comment String
    deriving (Show, Eq)


main :: IO ()
main = do 
        instruction_text <- TIO.readFile "data/01-mowmaster.txt"
        let instructions = successfulParse instruction_text
        print $ part1 instructions
        print $ part2 instructions

part1 :: [Instruction] -> Int
part1 = length 

part2 :: [Instruction] -> Int
part2 instructions = finalDistance $ executeAll instructions
    where executeAll = foldl' execute initialMowmaster

initialMowmaster = Mowmaster North (0, 0)


-- Calculate manhattan distance from start to this state
finalDistance :: Mowmaster -> Int
finalDistance m = (abs e) + (abs n)
    where (e, n) = position m


-- Make one move
execute :: Mowmaster -> Instruction -> Mowmaster
execute m (Forward s)    = m {position  = forward s (direction m) (position m)}
execute m  Clockwise     = m {direction = turnCW (direction m)}
execute m  Anticlockwise = m {direction = turnACW (direction m)}
execute m  _ = m

-- Move in the current direction
forward :: Int -> Direction -> Position -> Position
forward s North (e, n) = (e, n+s)
forward s South (e, n) = (e, n-s)
forward s West  (e, n) = (e-s, n)
forward s East  (e, n) = (e+s, n)


-- | a `succ` that wraps 
turnCW :: (Bounded a, Enum a, Eq a) => a -> a 
turnCW dir | dir == maxBound = minBound
           | otherwise       = succ dir

-- | a `pred` that wraps
turnACW :: (Bounded a, Enum a, Eq a) => a -> a
turnACW dir | dir == minBound = maxBound
            | otherwise       = pred dir


-- Parse the input file

type Parser = Parsec Void Text

-- treat comment lines as whitespace
sc :: Parser ()
sc = L.space space1 lineComment CA.empty
    where lineComment = L.skipLineComment "#"

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

-- instructions is some optional space followed by many instructions
instrsP = optional sc *> many instrP
instrP = forwardP <|> cwP <|> acwP

-- parse each instruction
forwardP = Forward <$> (symb "F" *> integer)
cwP = Clockwise <$ symb "C"
acwP = Anticlockwise <$ symb "A"

successfulParse :: Text -> [Instruction]
successfulParse input = 
        case parse instrsP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right instrs  -> instrs