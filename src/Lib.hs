module Lib
    ( getJSON
    , chooseCapsule
    ) where
import Wish
  ( Wish(..)
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
import qualified Data.ByteString.Lazy as B

-- get the json and transform in byte string
getJSON :: FilePath -> IO B.ByteString
getJSON filePath = B.readFile filePath

chooseCapsule :: Wish -> CapsuleWardrobe
chooseCapsule wish
  | wish == Wish {season = AutumnWinter, style = Casual} = autumnWinterCasualCW
  | wish == Wish {season = SpringSummer, style = Casual} = springSummerCasualCW
  | wish == Wish {season = AutumnWinter, style = Office} = autumnWinterOfficeCW
  | wish == Wish {season = SpringSummer, style = Office} = springSummerOfficeCW

