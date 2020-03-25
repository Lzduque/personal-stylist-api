import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
import Lib (getJSON)
import Wish
  ( Wish(..)
  , Style(..)
  , Season(..)
  )
import Data.Aeson (decode)

main :: IO ()
main = hspec $ do
  describe "decode" $ do
    it "returns a Wish given a JSON file with extra content" $ do
      str <- getJSON "test-1.json"
      let decodedStr = decode str :: Maybe Wish
      decodedStr `shouldBe` Just (Wish {season = AutumnWinter, style = Casual})

  describe "decode" $ do
    it "returns nothing given a JSON file with wrong content - style" $ do
      str <- getJSON "test-2.json"
      let decodedStr = decode str :: Maybe Wish
      decodedStr `shouldBe` Nothing

  describe "decode" $ do
    it "returns nothing given a JSON file with wrong content - season" $ do
      str <- getJSON "test-3.json"
      let decodedStr = decode str :: Maybe Wish
      decodedStr `shouldBe` Nothing

  describe "decode" $ do
    it "returns nothing given a JSON file with no content" $ do
      str <- getJSON "test-4.json"
      let decodedStr = decode str :: Maybe Wish
      decodedStr `shouldBe` Nothing

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
