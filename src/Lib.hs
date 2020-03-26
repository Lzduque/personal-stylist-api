module Lib
    ( getJSON
    , chooseCapsule
    , countOutfits
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
chooseCapsule Wish {season = AutumnWinter, style = Casual} = autumnWinterCasualCW
chooseCapsule Wish {season = SpringSummer, style = Casual} = springSummerCasualCW
chooseCapsule Wish {season = AutumnWinter, style = Office} = autumnWinterOfficeCW
chooseCapsule Wish {season = SpringSummer, style = Office} = springSummerOfficeCW

countOutfits :: CapsuleWardrobe -> Int
countOutfits capsule = (numOfTops * numOfPants * numOfOveralls) + (numOfTops * numOfSkirts * numOfOveralls) + (numOfDresses * numOfOveralls) where
    numOfTops = length . tops $ capsule
    numOfPants = length . pants $ capsule
    numOfSkirts = length . skirts $ capsule
    numOfDresses = length . dresses $ capsule
    numOfOveralls = length . overalls $ capsule

toRange :: NumberOfOutfits -> [Int]
toRange From10to20 = [10..20]
toRange From21to30 = [21..30]
toRange From31to40 = [31..40]
