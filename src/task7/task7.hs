{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Function (on)

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

type Burgers = [Int]
type Flips = [Int]
type Flipper = (Int, Flips)

main :: IO ()
main = do 
        flipsT <- TIO.readFile "data/07-flips.txt"
        let (burgers, flippers) = successfulParse flipsT
        print $ part1 burgers flippers
        print $ part2 burgers flippers


part1 :: Burgers -> [Flipper] -> Int
part1 burgers = length . filter isSorted . map (enflip burgers) . map snd
    where isSorted items = items == unburntSort items

part2 :: Burgers -> [Flipper] -> Int   
part2 burgers = fst . head . filter isSorted . map (\(n, fs) -> (n, enflip burgers fs)) 
    where isSorted (_, items) = (null $ filter (<= 0) items) && (items == sort items)


enflip :: Burgers -> Flips -> Burgers
enflip = foldl' oneFlip
    where oneFlip items pos = [-1 * i | i <- (reverse $ take pos items)] ++ (drop pos items)

unburntSort :: Burgers -> Burgers
unburntSort = sortBy (compare `on` abs)


-- Parse the input file

type Parser = Parsec Void Text

-- don't treat newlines as automatically-consumed whitespace
sc :: Parser ()
sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

flipsFileP = (,) <$> burgersP <* newline <*> (flipperP `endBy` newline)

burgersP = (:) <$> symb "burgers:" *> many integer

flipperP = (,) <$> integer <* symb ":" <*> many integer

successfulParse :: Text -> (Burgers, [Flipper])
successfulParse input = 
        case parse flipsFileP "input" input of
                Left  _error -> ([], []) -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right flips -> flips