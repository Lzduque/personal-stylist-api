{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib (someFunc)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON,ToJSON,Value,decode)
import GHC.Generics (Generic)

-- main function that is running everything
main :: IO ()
main = do
  str <- getJSON
  let decodedStr = decode str :: Maybe WishJSON
  case decodedStr of
    Just wish -> print wish
    Nothing -> print "nothing"

-- importing a json file
jsonFile :: FilePath
jsonFile = "test-1.json"

-- get the sjson nd trnsform in byte string
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

-- instance FromJSON WishJSON where
--   parseJSON = withObject "WishJSON" $

-- -------------- // TYPES // --------------
-- type of the input that it's being parsed
data WishJSON =
  WishJSON { season       :: String
            , style       :: String
            , bodyType    :: String
            , numOutfits  :: Int
            } deriving (Show,Generic)

-- Aeson methods to parse
instance FromJSON WishJSON
instance ToJSON WishJSON

-- data Wish =
--   Wish { season       :: Season
--         , style       :: Style
--         , bodyType    :: BodyType
--         , numOutfits  :: Int
--         } deriving (Show,Generic)

data Season = Spring | Summer | Autumn | Winter
  deriving Show

data Style = Casual | Office
  deriving Show

data BodyType = Rectangle | Apple | InvertedTriangle | Hourglass | Triangle
  deriving Show
