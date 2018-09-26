{-# LANGUAGE OverloadedStrings #-}

-- import Data.List (foldl')        -- import the strict fold
import Data.List

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))

import Data.Void (Void)

import Text.Megaparsec -- hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA


type TaskName = String
type Preconditions = [TaskName]

data Task = Task { name :: TaskName
                 , duration :: Int
                 , preconditions :: Preconditions
                 } deriving (Show, Eq)


type CompletedTasks = M.HashMap TaskName Int

main :: IO ()
main = do 
        task_text <- TIO.readFile "data/04-preparation.txt"
        let tasks = successfulParse task_text
        print $ part1 tasks
        print $ part2 tasks


part1 :: [Task] -> Int
part1 = sum . (map duration)

part2 :: [Task] -> Int
part2 tasks = maximum $ M.elems $ timeAllTasks tasks M.empty

timeAllTasks :: [Task] -> CompletedTasks -> CompletedTasks
timeAllTasks tasks completed 
    | null tasks = completed
    | otherwise  = timeAllTasks notDoable completed'
        where (doable, notDoable) = doableTasks completed tasks
              completed' = foldl' completeTask completed doable

doableTasks :: CompletedTasks -> [Task] -> ([Task], [Task])
doableTasks completed tasks = partition (allSatisfied completed) tasks

allSatisfied :: CompletedTasks -> Task -> Bool
allSatisfied completed task =
    all (\n -> n `M.member` completed) (preconditions task)


completeTask :: CompletedTasks -> Task -> CompletedTasks
completeTask completed task = M.insert (name task) (duration task + startTime) completed
    where startTime = if null $ preconditions task
                      then 0
                      else maximum $ map (\p -> completed!p) $ preconditions task



-- Parse the input file

type Parser = Parsec Void Text

-- don't treat newlines as automatically-consumed whitespace
sc :: Parser ()
sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

-- tasks is just a sequence of many individual tasks
tasksP = many taskP

-- a task is name, duration, preconditions, followed by newline
taskP = taskify <$> nameP <*> integer <*> (many nameP) <* newline
  where taskify n d ns = Task {name = n, duration = d, preconditions = ns}

nameP = lexeme (some letterChar)

successfulParse :: Text -> [Task]
successfulParse input = 
        case parse tasksP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right tasks  -> tasks