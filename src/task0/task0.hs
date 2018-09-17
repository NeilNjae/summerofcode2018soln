import qualified Data.HashMap.Strict as M
import Data.List (foldl')        -- import the strict fold
import Data.List.Split (splitOn)

-- a key-value store for recording how often each name is seen
type NameCount = M.HashMap String Int

main :: IO ()
main = do 
        -- text <- readFile "data/small_invites.txt"
        text <- readFile "data/00-invites.txt"
        print $ part1 text
        print $ part2 text

-- use `words` here as I don't need to worry about the comma delimiters
part1 :: String -> Int
part1  = maximum . map (length . words) . lines 

part2 :: String -> Int
part2 inviteText = M.size popular
    where -- split names on the delimieter, use `concatMap` to give just one long list
          invites = concatMap (splitOn ", ") $ lines inviteText
          -- count the names
          invited = nameCounts invites
          -- popular are those mentioned more than once
          popular = M.filter (> 1) invited


-- count how many times each name appears.
-- It's a fold over the list of names, creating the name count HashMap.
-- As each name is taken from the list, we find how many times we've seen it
-- before, add one to it, then update the HashMap with the new count.
nameCounts :: [String] -> NameCount
nameCounts names = foldl' includeName M.empty names
    where includeName nc name = M.insert name (1 + count name nc) nc
          -- a default count of 0 for each name
          count name = M.lookupDefault 0 name