module Main where

import Lib (getJSON)
import Wish
  (Wish(..)
  , Style(..)
  , Season(..)
  )
import CapsuleWardrobe
  (CapsuleWardrobe(..)
  , autumnWinterCasualCW
  , springSummerCasualCW
  , autumnWinterOfficeCW
  , springSummerOfficeCW
  )
import Data.Aeson (decode)

jsonFile = "test-1.json"

-- main function that is running everything
main :: IO ()
main = do
  str <- getJSON jsonFile
  let decodedStr = decode str :: Maybe Wish
  case decodedStr of
    Just wish -> print wish
    Nothing -> print "nothing"


