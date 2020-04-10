module Main where

import Data.Aeson (decode)
import Text.Pretty.Simple (pPrint)
import Web.Scotty
-- import Network.HTTP.Types
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as BS

import Lib ()
import CapsuleWardrobe 
  ( CapsuleWardrobe(..)
  , groupByClothing
  , fillUpWardrobe
  )



main :: IO ()
main = scotty 3000 $ do
  get "/capsule/:capsule" $ do                         -- handle GET request on "/" URL
    capsule <- param "capsule"
    -- text $ "Capsule: " <> capsule
    liftIO $ putStrLn "capsule"
    case Base64.decode . UTF8.fromString . T.unpack $ capsule of
      Left error -> do
        text $ T.pack error
        liftIO $ putStrLn "Error in the left"
      Right capsuleJSON -> do
        let decodedStr = decode (BS.pack (UTF8.toString capsuleJSON)) :: Maybe CapsuleWardrobe
        liftIO $ putStrLn "decodedStr"
        case decodedStr of
          Just capsule -> do
            liftIO $ putStrLn "capsule inside decodedstr"
            setHeader "Access-Control-Allow-Origin" "http://localhost:2000"
            case fillUpWardrobe capsule of
              Left error -> do
                json $ error
              Right wardrobe -> do
                let displayWardrobe = groupByClothing wardrobe
                liftIO $ pPrint displayWardrobe
                json $ displayWardrobe
                liftIO $ putStrLn "answer with json"
          Nothing -> json ("error in form input" :: String)
  delete "/" $ do
    html "This was a DELETE request!"  -- send 'text/html' response
  post "/" $ do
    text "This was a POST request!"
  put "/" $ do
    text "This was a PUT request!"



-- jsonFile :: FilePath
-- jsonFile = "CW-1.json"

-- main function that is running everything
-- mainF :: IO ()
-- mainF = do
--   str <- getJSON jsonFile
--   let decodedStr = decode str :: Maybe CapsuleWardrobe
--   case decodedStr of
--     Just capsule -> do
--       pPrint $ fillUpWardrobe capsule
--       let count = countOutfits . wardrobe $ fillUpWardrobe capsule
--       putStrLn $ "Num Of Outfits: " ++ show count
--     Nothing -> print "nothing"
