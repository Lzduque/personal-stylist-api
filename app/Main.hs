module Main where

import Lib (getJSON, chooseCapsule, countOutfits, makeCapsule)
import Wish (Wish(..))
import CapsuleWardrobe ()
import Data.Aeson (decode)
import Text.Pretty.Simple (pPrint)

jsonFile :: FilePath
jsonFile = "CW-1.json"

-- main function that is running everything
main :: IO ()
main = do
  str <- getJSON jsonFile
  let decodedStr = decode str :: Maybe Wish
  case decodedStr of
    Just wish -> do
      pPrint . makeCapsule wish $ chooseCapsule wish
      let count = countOutfits . makeCapsule wish $ chooseCapsule wish
      putStrLn $ "Num Of Outfits: " ++ show count
    Nothing -> print "nothing"
