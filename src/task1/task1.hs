import Data.List (foldl')        -- import the strict fold

-- number of steps
type Distance = Int

-- easting, northing
type Position = (Int, Int)

-- the directions. See below for functions for turning
data Direction = North | East | South | West 
    deriving (Enum, Show, Bounded, Eq)

-- the currenct state of a Mowmaster
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
        instruction_text <- readFile "data/01-mowmaster.txt"
        print $ part1 instruction_text
        print $ part2 instruction_text

part1 :: String -> Int
part1 = length . filter (not . isComment) . lines

part2 :: String -> Int
part2 instruction_text = finalDistance $ executeAll instructions
    where instructions = readInstructions instruction_text
          executeAll = foldl' execute initialMowmaster
          

-- Is this line a comment?
isComment :: String -> Bool
isComment ('#':_) = True
isComment _       = False


-- Extract the steps from the input string.
readInstructions :: String -> [Instruction]
readInstructions = (map readInstruction) . lines

readInstruction :: String -> Instruction
readInstruction i = readInstruction' (head i) (tail i)
    where readInstruction' 'F' s = Forward (read s)
          readInstruction' 'C' _ = Clockwise
          readInstruction' 'A' _ = Anticlockwise
          readInstruction'  _  t = Comment t


initialMowmaster = Mowmaster East (0, 0)


-- Calculate manhattan distance from start to this state
finalDistance :: Mowmaster -> Int
finalDistance m = (abs e) + (abs n)
    where (e, n) = position m


-- Make one move
execute :: Mowmaster -> Instruction -> Mowmaster
execute m (Forward s)    = m {position  = forward s (direction m) (position m)}
execute m  Clockwise     = m {direction = turnCW (direction m)}
execute m  Anticlockwise = m {direction = turnACW (direction m)}
execute m (Comment _)    = m


-- Move in the current direction
forward :: Distance -> Direction -> Position -> Position
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
