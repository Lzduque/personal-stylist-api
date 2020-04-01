module Lib where

import qualified Data.ByteString.Lazy as B

-- ------------ HELPERS
-- get the json and transform in byte string
getJSON :: FilePath -> IO B.ByteString
getJSON filePath = B.readFile filePath

inRange :: Int -> (Int,Int) -> Bool
inRange x (a,b)
  | a <= x && x <= b = True
  | otherwise = False

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (x==)
