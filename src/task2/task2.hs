import Data.List (foldl')

main :: IO ()
main = do 
        record_text <- readFile "data/02-rainfall.txt"
        let records = readRecords record_text
        print $ part1 records
        print $ part2 records
        print $ part2a records

readRecords :: String -> [Int]
readRecords = takeWhile (< 9999) . map read . lines

part1 :: [Int] -> Int
part1 = length 

part2 :: [Int] -> Int
part2 records = round $ ((fromIntegral $ sum records) :: Double)
                      / (fromIntegral $ length records)
                      

-- Doing part 2 as a fold, so only traverse the list once
part2a :: [Int] -> Int
part2a records = round $ ((fromIntegral sumR) :: Double) / (fromIntegral count)
    where 
        (sumR, count) = foldl' (\(s, n) r -> (s+r, n+1)) (0, 0) records
