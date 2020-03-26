module Wish
  (Wish(..)
  , Style(..)
  , Season(..)
  , NumberOfOutfits(..)
  ) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)

data Wish =
  Wish { season           :: Season
        , style           :: Style
        , numberOfOutfits :: NumberOfOutfits
        } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Season = SpringSummer | AutumnWinter
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Style = Casual | Office
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NumberOfOutfits = From10to20 | From21to30 | From31to40
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- data BodyType = Rectangle | Apple | InvertedTriangle | Hourglass | Triangle
--   deriving Show
