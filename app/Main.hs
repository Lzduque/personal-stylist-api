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

-- LOGIC --
chooseCapsule :: Wish -> CapsuleWardrobe
chooseCapsule wish
  | wish == Wish {season = AutumnWinter, style = Casual} = autumnWinterCasualCW
  | wish == Wish {season = SpringSummer, style = Casual} = springSummerCasualCW
  | wish == Wish {season = AutumnWinter, style = Office} = autumnWinterOfficeCW
  | wish == Wish {season = SpringSummer, style = Office} = springSummerOfficeCW

-- main function that is running everything
main :: IO ()
main = do
  str <- getJSON jsonFile
  let decodedStr = decode str :: Maybe Wish
  case decodedStr of
    Just wish -> print wish
    Nothing -> print "nothing"


