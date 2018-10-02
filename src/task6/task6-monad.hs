{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM_, ap, liftM)

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

newtype State a = State (FriendGroups -> (FriendGroups, a))

instance Monad State where 
    return x = State (\groups -> (groups, x))
    (State st) >>= f 
        = State (\groups -> let 
                            (newGroups, y) = st groups
                            (State trans)  = f y
                            in
                            trans newGroups 
                )

instance Applicative State where
    pure = return
    (<*>) = ap

instance Functor State where
    fmap = liftM




main :: IO ()
main = do 
        friendship_text <- TIO.readFile "data/06-friendships.txt"
        let friendships = map enpair $ T.lines friendship_text
        let groups = execGroups $ mergeFriendships friendships
        print $ part1 groups
        print $ part2 groups


part1 :: FriendGroups -> Int
part1 = M.size . M.filterWithKey (\k a -> k == parent a)

part2 :: FriendGroups -> Int
part2 = maximum . (map size) . M.elems


enpair friendText = (this, that)
    where (this:that:[]) = T.words friendText


-- run a state monad, extract the groups
execGroups :: State a -> FriendGroups
execGroups (State st) = fst $ st M.empty

-- including all the friendships is just a monadic fold
mergeFriendships :: [(Name, Name)] -> State ()
mergeFriendships pairs = foldM_ includeFriendshipM () pairs

includeFriendshipM :: () -> (Name, Name) -> State ()
includeFriendshipM _ (thisPerson, thatPerson) = do
    include thisPerson
    include thatPerson
    thisExemplar <- exemplar thisPerson
    thatExemplar <- exemplar thatPerson
    if thisExemplar /= thatExemplar
    then do
          let thisSize = size thisExemplar
          let thatSize = size thatExemplar
          let (absorber, absorbed) = if thisSize > thatSize 
                                     then (thisExemplar, thatExemplar)
                                     else (thatExemplar, thisExemplar)
          absorb absorber absorbed
          updateSize absorber (thisSize + thatSize)
    else
          return ()


exemplar :: Name -> State Group 
exemplar name = State (exemplar' name)
    where exemplar' person groups = 
              let person' = parent (groups!person)
              in if person' == person
                 then (groups, groups!person)
                 else exemplar' person' groups

include :: Name -> State ()
include name = State (\groups -> 
                  if name `M.member` groups
                  then (groups, () )
                  else (M.insert name (Group {parent = name, size = 1}) groups, ())
                  )

absorb :: Group -> Group -> State ()
absorb absorberG absorbedG = 
    let absorber = parent absorberG
        absorbed = parent absorbedG
    in State (\groups ->
      ( M.insert absorbed ((groups!absorbed) {parent = absorber}) groups
      , ()
      ) 
    )

updateSize :: Group -> Int -> State ()
updateSize exemplarG newSize = 
    let name = parent exemplarG
    in State (\groups -> 
      ( M.insert name ((groups!name) {size = newSize}) groups,
        ()
      )
    )