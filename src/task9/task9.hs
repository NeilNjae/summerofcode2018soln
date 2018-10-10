{-# LANGUAGE OverloadedStrings #-}

import Data.List (inits, sortBy, foldl')
import Data.Function (on)

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))


type Weight = Int
type Value = Int
data Bag = Bag { bagWeight :: Weight, bagValue :: Value} deriving (Show, Eq)
data TableEntry = TableEntry {value :: Value, backpointer :: (Int, Weight)} deriving (Show, Eq)
type DPTable = M.HashMap (Int, Weight) TableEntry

main :: IO ()
main = do 
        bagsT <- TIO.readFile "data/09-bags.txt"
        let bags = successfulParse bagsT
        let limit = 5000
        -- print bags
        print $ part1 limit bags
        print $ part2 limit bags

part1 :: Weight -> [Bag] -> Int
part1 limit bags = length $ last $ takeWhile (willFit limit) $ inits sortedBags
    where sortedBags = sortBy (compare `on` bagWeight) bags
          willFit limit' bags' = (sum $ map bagWeight bags') < limit'

part2 :: Weight -> [Bag] -> Value
part2 limit bags = value $ table!(length bags, limit)
    where initialTable = M.fromList [((0, w), TableEntry {value = 0, backpointer = (0, w)}) | w <- [0..limit]]
          table = foldl' includeBag initialTable 
                          [ (thisBag, bagNumber, allowedWeight) 
                          | (thisBag, bagNumber) <- zip bags [1..]
                          , allowedWeight <- [0..limit]
                          ]


includeBag :: DPTable -> (Bag, Int, Weight) -> DPTable
includeBag table (bag, bagNumber, weight) = 
    if bagWeight bag > weight 
    then M.insert here withoutBag table 
    else if value withBagEntry + bagValue bag > value withoutBagEntry
         then M.insert here withBag table
         else M.insert here withoutBag table
    where here = (bagNumber, weight)
          withoutBagKey = (bagNumber - 1, weight)
          withBagKey = (bagNumber - 1, weight - bagWeight bag)
          withoutBagEntry = table!withoutBagKey
          withBagEntry = table!withBagKey
          withoutBag = TableEntry {value = value withoutBagEntry, backpointer = withoutBagKey}
          withBag = TableEntry {value = value withBagEntry + bagValue bag, backpointer = withBagKey}


-- Parse the input file

type Parser = Parsec Void Text

-- don't treat newlines as automatically-consumed whitespace
sc :: Parser ()
sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
-- symb = L.symbol sc

bagsFileP = bagP `sepBy` newline

bagP = bagify <$> integer <*> integer
    where bagify w v = Bag { bagWeight = w , bagValue = v}

successfulParse :: Text -> [Bag]
successfulParse input = 
        case parse bagsFileP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right bags -> bags