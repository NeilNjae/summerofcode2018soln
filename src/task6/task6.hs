{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')        -- import the strict fold
-- import Data.List

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))

type Name = Text

data Group = Group { parent :: Name
                   , size :: Int
                   } deriving (Show, Eq)


type FriendGroups = M.HashMap Name Group

main :: IO ()
main = do 
        friendship_text <- TIO.readFile "data/06-friendships.txt"
        let friendships = map enpair $ T.lines friendship_text
        let groups = mergeFriendships friendships
        print $ part1 groups
        print $ part2 groups


part1 :: FriendGroups -> Int
part1 = M.size . M.filterWithKey (\k a -> k == parent a)

part2 :: FriendGroups -> Int
part2 = maximum . (map size) . M.elems


enpair friendText = (this, that)
    where (this:that:[]) = T.words friendText


mergeFriendships :: [(Name, Name)] -> FriendGroups
mergeFriendships = foldl' includeFriendship M.empty

exemplar :: FriendGroups -> Name -> Name
exemplar groups person
    | person' == person = person
    | otherwise         = exemplar groups person'
    where person' = parent (groups!person)


include :: FriendGroups -> Name -> FriendGroups
include groups person = 
    if person `M.member` groups
    then groups
    else M.insert person (Group {parent = person, size = 1}) groups


includeFriendship :: FriendGroups -> (Name, Name) -> FriendGroups
includeFriendship groups0 (thisPerson, thatPerson) =
    if thisExemplar == thatExemplar
    then groups
    else groups''
    where groups1 = include groups0 thisPerson
          groups = include groups1 thatPerson
          thisExemplar = exemplar groups thisPerson
          thatExemplar = exemplar groups thatPerson
          thisSize = size $ groups!thisExemplar
          thatSize = size $ groups!thatExemplar
          (absorber, absorbed) = if thisSize > thatSize 
                                 then (thisExemplar, thatExemplar)
                                 else (thatExemplar, thisExemplar)
          groups'  = M.insert absorbed ((groups!absorbed) {parent = absorber}) groups 
          groups'' = M.insert absorber ((groups!absorber) {size = thisSize + thatSize}) groups'

