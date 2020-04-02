module Main where

import Data.Aeson (decode)
import Text.Pretty.Simple (pPrint)
import Web.Scotty
-- import Network.HTTP.Types
import Control.Monad.IO.Class (liftIO)

import Lib (getJSON)
import CapsuleWardrobe 
  ( CapsuleWardrobe(..)
  , groupByClothing
  , fillUpWardrobe
  , countOutfits
  )



jsonFile :: FilePath
jsonFile = "CW-1.json"



main = scotty 3000 $ do
  get "/" $ do                         -- handle GET request on "/" URL
    str <- liftIO $ getJSON jsonFile
    let decodedStr = decode str :: Maybe CapsuleWardrobe
    case decodedStr of
      Just capsule -> do
        json $ groupByClothing $ fillUpWardrobe capsule
      Nothing -> text "nothing"
  delete "/" $ do
    html "This was a DELETE request!"  -- send 'text/html' response
  post "/" $ do
    text "This was a POST request!"
  put "/" $ do
    text "This was a PUT request!"



-- main function that is running everything
mainF :: IO ()
mainF = do
  str <- getJSON jsonFile
  let decodedStr = decode str :: Maybe CapsuleWardrobe
  case decodedStr of
    Just capsule -> do
      pPrint $ fillUpWardrobe capsule
      let count = countOutfits . wardrobe $ fillUpWardrobe capsule
      putStrLn $ "Num Of Outfits: " ++ show count
    Nothing -> print "nothing"
