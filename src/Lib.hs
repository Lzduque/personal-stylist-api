module Lib
    ( getJSON
    ) where

import qualified Data.ByteString.Lazy as B

-- get the json and transform in byte string
getJSON :: FilePath -> IO B.ByteString
getJSON filePath = B.readFile filePath

