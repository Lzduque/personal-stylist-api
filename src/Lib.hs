module Lib
    ( getJSON
    , chooseCapsule
    , numOfOutfits
    ) where
import Wish
  ( Wish(..)
  , Style(..)
  , Season(..)
  , NumberOfOutfits(..)
  )
import CapsuleWardrobe
  (CapsuleWardrobe(..)
  , autumnWinterCasualCW
  , springSummerCasualCW
  , autumnWinterOfficeCW
  , springSummerOfficeCW
  )
import qualified Data.ByteString.Lazy as B

-- get the json and transform in byte string
getJSON :: FilePath -> IO B.ByteString
getJSON filePath = B.readFile filePath

chooseCapsule :: Wish -> CapsuleWardrobe
chooseCapsule wish
  | wish == Wish {season = AutumnWinter, style = Casual, numberOfOutfits = From10to20} = autumnWinterCasualCW
  | wish == Wish {season = SpringSummer, style = Casual, numberOfOutfits = From10to20} = springSummerCasualCW
  | wish == Wish {season = AutumnWinter, style = Office, numberOfOutfits = From10to20} = autumnWinterOfficeCW
  | wish == Wish {season = SpringSummer, style = Office, numberOfOutfits = From10to20} = springSummerOfficeCW

numOfOutfits :: CapsuleWardrobe -> Int
numOfOutfits capsule = (numOfTops * numOfPants * numOfOveralls) + (numOfTops * numOfSkirts * numOfOveralls) + (numOfDresses * numOfOveralls) where
    numOfTops = length . tops $ capsule
    numOfPants = length . pants $ capsule
    numOfSkirts = length . skirts $ capsule
    numOfDresses = length . dresses $ capsule
    numOfOveralls = length . overalls $ capsule
