module Main where

import Data.Aeson (decode)
import Text.Pretty.Simple (pPrint)

import Lib (getJSON)
import CapsuleWardrobe 
  ( CapsuleWardrobe(..)
  , groupByClothing
  , fillUpWardrobe
  , countOutfits
  )



jsonFile :: FilePath
jsonFile = "CW-1.json"

-- main function that is running everything
main :: IO ()
main = do
  str <- getJSON jsonFile
  let decodedStr = decode str :: Maybe CapsuleWardrobe
  case decodedStr of
    Just capsule -> do
      pPrint . groupByClothing . groupByClothing $ fillUpWardrobe capsule
      let count = countOutfits . wardrobe $ fillUpWardrobe capsule
      putStrLn $ "Num Of Outfits: " ++ show count
    Nothing -> print "nothing"
