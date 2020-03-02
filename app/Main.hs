{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib (someFunc)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)

main :: IO ()
main = do
  str <- getJSON
  B.putStrLn str

data Requirements =
  Requirements { season       :: String
                , style       :: String
                , bodyType    :: String
                , numOutfits  :: Int
                } deriving (Show,Generic)

instance FromJSON Requirements
instance ToJSON Requirements


jsonFile :: FilePath
jsonFile = "test-1.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


-- instance FromJSON Requirements where
--  parseJSON (Object v) =
--     Person <$> v .: "season"
--            <*> v .: "style"
--            <*> v .: "bodyType"
--            <*> v .: "numOutfits"
--  parseJSON _ = mzero

-- instance ToJSON Requirements where
--  toJSON (Person season style bodyType numOutfits) =
--     object [ "season"     .= season
--            , "style"      .= style
--            , "bodyType"   .= bodyType
--            , "numOutfits" .= numOutfits
--              ]

