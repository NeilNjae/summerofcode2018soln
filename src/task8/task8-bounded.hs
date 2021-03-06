import Data.Maybe (fromMaybe)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.HashSet as S
import qualified Data.Sequence as Q
import qualified Data.HashMap.Strict as M
import Data.Sequence ((<|))
import Data.Foldable (foldl')
import Data.HashMap.Strict ((!))


data Grass = Short | Long deriving (Show, Eq) 
type Location = (Int, Int)
data Neighbour = Neighbour {stepLocation :: Location, stepCost :: Int} deriving (Show, Eq)
type Lawn = M.HashMap Location Grass
data BoundedLawn = BoundedLawn { lawnMap :: Lawn
                               , minRow :: Int , minCol :: Int
                               , maxRow :: Int , maxCol :: Int
                               }
type Closed = S.HashSet Location
data Agendum = Agendum {current :: Location, trail :: Q.Seq Location, cost :: Int} deriving (Show, Eq)
type Agenda = P.MinPQueue Int Agendum 


main :: IO ()
main = do 
    lawnT <- readFile "data/08-maze.txt"
    let lawn = parseLawn lawnT
    let boundedLawn = boundify lawn
    let goal = goalLocation boundedLawn
    let start = startLocation boundedLawn
    print $ part1 boundedLawn start goal
    print $ part2 boundedLawn start goal

part1 :: BoundedLawn -> Location -> Location -> Int
part1 lawn start goal = cost $ fromMaybe (snd $ P.findMin $ initAgenda start goal) $ aStar 999 goal lawn (initAgenda start goal) S.empty

part2 :: BoundedLawn -> Location -> Location -> Int
part2 lawn start goal = cost $ fromMaybe (snd $ P.findMin $ initAgenda start goal) $ aStar 3 goal lawn (initAgenda start goal) S.empty


parseLawn :: String -> Lawn
parseLawn lawnT = M.fromList lawnCells
    where lawnLines = zip [1..] $ lines lawnT
          lawnCells = concatMap (\(r, row) -> zipWith (cellify r) [1..] row) lawnLines
          cellify r c cell
            | cell == '#' = ((r, c), Long)
            | otherwise   = ((r, c), Short)

boundify lawn = BoundedLawn { lawnMap = lawn 
                            , minRow = minR, minCol = minC
                            , maxRow = maxR, maxCol = maxC
                            }
    where minR = minimum $ map fst $ M.keys lawn
          maxR = maximum $ map fst $ M.keys lawn
          minC = minimum $ map snd $ M.keys lawn
          maxC = maximum $ map snd $ M.keys lawn

goalLocation :: BoundedLawn -> Location
goalLocation lawn = ( maxRow lawn, maxCol lawn )

startLocation :: BoundedLawn -> Location
startLocation lawn = ( minRow lawn, minCol lawn )

initAgenda :: Location -> Location -> Agenda
initAgenda start goal = P.singleton (estimateCost start goal) Agendum {current = start, trail = Q.empty, cost = 0}

aStar :: Int -> Location -> BoundedLawn -> Agenda -> Closed -> Maybe Agendum
aStar longCost goal lawn agenda closed 
    -- | trace ("Peeping " ++ (show $ fst $ P.findMin agenda) ++ ": " ++ (show reached) ++ " <- " ++ (show $ toList $ Q.take 1 $ trail $ currentAgendum) ++ " :: " ++ (show newAgenda)) False = undefined
    | P.null agenda = Nothing
    | otherwise = 
        if reached == goal then Just currentAgendum
        else if reached `S.member` closed 
            then aStar longCost goal lawn (P.deleteMin agenda) closed
            else aStar longCost goal lawn newAgenda (S.insert reached closed)
        where 
            (_, currentAgendum) = P.findMin agenda
            reached = current currentAgendum
            newAgenda = foldl' (\q a -> P.insert (estimatedCost a) a q) (P.deleteMin agenda) $ candidates longCost lawn currentAgendum closed
            estimatedCost agendum = estimateCost (current agendum) goal + cost agendum



candidates :: Int -> BoundedLawn -> Agendum -> Closed -> Q.Seq Agendum
candidates longCost lawn agendum closed = newCandidates
    where
        candidate = current agendum
        previous = trail agendum
        succs = successors longCost lawn candidate
        nonloops = Q.filter (\s -> not $ (stepLocation s) `S.member` closed) succs
        newCandidates = fmap (\n -> makeAgendum n) nonloops
        makeAgendum new = Agendum {current = stepLocation new, 
                                    trail = candidate <| previous, 
                                    cost = cost agendum + stepCost new}


successors :: Int -> BoundedLawn -> Location -> (Q.Seq Neighbour)
successors longCost boundedLawn (row, column) = Q.fromList $ map neighbourify neighbours
    where lawn = lawnMap boundedLawn
          neighbours = filter (\l -> l `M.member` lawn) 
                                [ (r, c) | r <- [(row - 1)..(row + 1)]
                                         , c <- [(column - 1)..(column + 1)]
                                         , r >= minR
                                         , r <= maxR
                                         , c >= minC
                                         , c <= maxC
                                         , ((r == row && c /= column) || (r /= row && c == column))
                                ]
          neighbourify neighbour = Neighbour {stepLocation = neighbour, stepCost = scCalc neighbour}
          minR = minRow boundedLawn
          maxR = maxRow boundedLawn
          minC = minCol boundedLawn
          maxC = maxCol boundedLawn
          scCalc location = if lawn!location == Long
                            then longCost
                            else 1


estimateCost :: Location -> Location -> Int
estimateCost (r, c) (gr, gc) = abs (r - gr) + abs(c - gc)

