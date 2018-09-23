{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')        -- import the strict fold

import qualified Data.Map.Strict as M

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec -- hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA


-- number of steps
type Distance = Int

-- easting, northing
type Position = (Int, Int)

-- the directions. See below for functions for turning
data Direction = North | East | South | West 
    deriving (Enum, Show, Bounded, Eq)

type Grass = M.Map Position Bool


-- The currenct state of a World
data World = World { direction :: Direction 
                   , position :: Position
                   , pen :: Bool
                   , grass :: Grass
                   } deriving (Show, Eq)

-- one instruction for the World
data Instruction =   Forward Distance
                   | Clockwise 
                   | Anticlockwise
                   | Up
                   | Down
    deriving (Show, Eq)


main :: IO ()
main = do 
        instruction_text <- TIO.readFile "data/03-graffiti.txt"
        let instructions = successfulParse instruction_text
        let mownWorld = foldl' execute initialWorld instructions
        print $ part1 mownWorld
        TIO.putStr $ part2 mownWorld


part1 :: World -> Int
part1 = M.size . grass 

part2 = showWorld

initialWorld = World { direction = North
                     , position = (0, 0)
                     , pen = False
                     , grass = M.empty
                     }


-- Make one move
execute :: World -> Instruction -> World
execute w (Forward s)    = iterate forward w !! s
execute w  Clockwise     = w {direction = turnCW (direction w)}
execute w  Anticlockwise = w {direction = turnACW (direction w)}
execute w  Up            = w {pen = False}
execute w  Down          = mow $ w {pen = True}


mow :: World -> World
mow w | pen w     = w {grass = M.insert (position w) True (grass w)}
      | otherwise = w


forward :: World -> World
forward w = mow $ w {position = newPosition (direction w) (position w)}

-- Move in the current direction
newPosition :: Direction -> Position -> Position
newPosition North (e, n) = (e, n+1)
newPosition South (e, n) = (e, n-1)
newPosition West  (e, n) = (e-1, n)
newPosition East  (e, n) = (e+1, n)


-- | a `succ` that wraps 
turnCW :: (Bounded a, Enum a, Eq a) => a -> a 
turnCW dir | dir == maxBound = minBound
           | otherwise       = succ dir

-- | a `pred` that wraps
turnACW :: (Bounded a, Enum a, Eq a) => a -> a
turnACW dir | dir == minBound = maxBound
            | otherwise       = pred dir



showWorld :: World -> Text
showWorld w = showGrass mine maxn minn mine maxn maxe g 
    where 
      g = grass w
      mine = minimum(map fst $ M.keys g)
      maxe = maximum(map fst $ M.keys g)
      minn = minimum(map snd $ M.keys g)
      maxn = maximum(map snd $ M.keys g)

showGrass :: Int -> Int -> Int -> Int -> Int -> Int -> Grass -> Text
showGrass e n minn mine maxn maxe g 
  | n == minn && e == maxe = T.singleton '\n'
  | e == maxe              = T.cons '\n' $ showGrass mine  (n-1) minn mine maxn maxe g
  | otherwise              = T.cons cell $ showGrass (e+1) n     minn mine maxn maxe g
      where cell = if M.member (e, n) g then '⌷' else ' '

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

-- an instruction is either F, C, or A
instrP = forwardP <|> cwP <|> acwP <|> upP <|> downP

-- parse each instruction
forwardP = Forward <$> (symb "F" *> integer)
cwP = Clockwise <$ symb "C"
acwP = Anticlockwise <$ symb "A"
upP = Up <$ symb "U"
downP = Down <$ symb "D"

successfulParse :: Text -> [Instruction]
successfulParse input = 
        case parse instrsP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right instrs  -> instrs