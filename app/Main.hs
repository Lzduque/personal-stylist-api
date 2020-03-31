module Main where

import Lib (getJSON, setUpBaseWardrobe, countOutfits, fillUpWardrobe, groupByClothing)
import CapsuleWardrobe
import Data.Aeson (decode)
import Text.Pretty.Simple (pPrint)

jsonFile :: FilePath
jsonFile = "CW-1.json"

-- main function that is running everything
main :: IO ()
main = do
  str <- getJSON jsonFile
  let decodedStr = decode str :: Maybe CapsuleWardrobe
  case decodedStr of
    Just capsule -> do
      pPrint . groupByClothing . fillUpWardrobe $ setUpBaseWardrobe capsule
      let count = countOutfits . wardrobe . fillUpWardrobe $ setUpBaseWardrobe capsule
      putStrLn $ "Num Of Outfits: " ++ show count
    Nothing -> print "nothing"
