module Wish where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)

data Wish =
  Wish { season           :: Season
        , style           :: Style
        , numberOfOutfits :: NumberOfOutfits
        , colors          :: [Colors]
        } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Season = SpringSummer | AutumnWinter
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Style = Casual | Office
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NumberOfOutfits = From10to20 | From21to30 | From31to40 | From41to50 | From51to60 | From61to70 | From71to80 | From81to90 | From91to100
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Colors = White | OffWhite | Beige | Brown | Black | Navy | Blue | LightBlue | DarkGreen | LightGreen | DarkYellow | LightYellow | DarkPink | LightPink | DarkRed | LightRed | DarkOrgange | LightOrange | DarkPurple | LightPurple
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- data BodyType = Rectangle | Circle | InvertedTriangle | Hourglass | Triangle
--   deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Hourglass
--   General 
--     "Try to choose light fabrics that don't add too much volume."
--     "The best fabric for your body type is soft but firm and elastic, such as viscose, polyester, jersey, silk, chiffon, soft wool crepe, fine knits and fabrics with lycra."
--   Dresses
--     "Try acentuating your curves, drawing attention to your waist."
--     "The best dresses are wrap, bodycon, monocrome, with V-neckline, but never too tight."
--   Tops
--     ""