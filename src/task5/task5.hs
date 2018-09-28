{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isSpace)


main :: IO ()
main = do 
        text <- TIO.readFile "data/05-instructions.txt"
        let uncommented = deComment text
        let expanded = expand uncommented
        print $ countNonSpace uncommented
        print $ countNonSpace expanded


countNonSpace :: Text -> Int
countNonSpace = T.length . T.filter (\c -> not (isSpace c))


-- Remove comments
deComment :: Text -> Text
deComment text = deComment' (extractFirstComment text) text

-- If there are no comments, return the text as-is
-- If there is a first comment, decomment the rest
deComment' :: Maybe (Text, Text) -> Text -> Text
deComment' Nothing text = text
deComment' (Just (prefix, suffix)) _ = prefix `T.append` (deComment suffix)

-- Monad notation to string togeether all the Maybes
extractFirstComment :: Text -> Maybe (Text, Text)
extractFirstComment text = 
    do (prefix, commentSuffix) <- maybeBreakOn "<" text
       (_, suffix) <- maybeBreakOn ">" commentSuffix
       return (prefix, suffix)


-- Expand compression
expand :: Text -> Text
expand text = expand' (extractFirstExpander text) text

-- If there are no compression markers, return the text
-- If there's one, expand it, and try again.
expand' :: Maybe (Text, Int, Int, Text) -> Text -> Text
expand' Nothing text = text
expand' (Just (prefix, expandLen, expandCount, suffix)) _ =
    expand (expandedPrefix `T.append` suffix)
    where unexpandedPrefix = T.dropEnd expandLen prefix
          group = T.takeEnd expandLen prefix
          expanded  = T.replicate expandCount group
          expandedPrefix = unexpandedPrefix `T.append` expanded

extractFirstExpander :: Text -> Maybe (Text, Int, Int, Text)
extractFirstExpander text = 
    do (prefix, suffix) <- maybeBreakOn ":" text
       (expanderLenT, suffix') <- maybeBreakOn ":" suffix
       (expanderCountT, suffix'') <- maybeBreakOn ":" suffix'
       let expanderLen = read $ T.unpack expanderLenT
       let expanderCount = read $ T.unpack expanderCountT
       return (prefix, expanderLen, expanderCount, suffix'')


-- Uses Text.breakOn, but puts the result in a Maybe
-- Also drops the needle from the returned suffix.
maybeBreakOn :: Text -> Text -> Maybe (Text, Text)
maybeBreakOn needle haystack =
    if T.null suffix
    then Nothing
    else Just (prefix, T.drop (T.length needle) suffix)
    where (prefix, suffix) = T.breakOn needle haystack
