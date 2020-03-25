module Wish
  (Wish(..)
  , Style(..)
  ,Season(..)
  ) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)

data Wish =
  Wish { season       :: Season
        , style       :: Style
        } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Season = SpringSummer | AutumnWinter
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Style = Casual | Office
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- data BodyType = Rectangle | Apple | InvertedTriangle | Hourglass | Triangle
--   deriving Show
