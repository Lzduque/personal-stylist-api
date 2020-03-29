import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
import Lib 
  (getJSON
  , chooseCapsule
  , countOutfits
  , toRange
  , inRange
  , countOccurrences
  )
import Wish
  ( Wish(..)
  , Style(..)
  , Season(..)
  , NumberOfOutfits(..)
  )
import CapsuleWardrobe
  (CapsuleWardrobe(..)
  , Top(..)
  , Pants(..)
  , Skirt(..)
  , Dress(..)
  , Overall(..)
  , Shoe(..)
  , Purse(..)
  , autumnWinterCasualCW
  , springSummerCasualCW
  , autumnWinterOfficeCW
  , springSummerOfficeCW
  )
import Data.Aeson (decode)

autumnWinterCasualWish = Wish {season = AutumnWinter, style = Casual, numberOfOutfits = From10to20}

testCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, ShortSleeveShirt, LongSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = [ShortSkirt, LongSkirt]
                  , dresses = [LongSleeveDress]
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [Backpack]
                  }


decodeJSON :: FilePath -> IO (Maybe Wish)
decodeJSON filePath = do
  str <- getJSON filePath
  return $ decode str

main :: IO ()
main = hspec $ do
  describe "decode" $ do
    it "returns a Wish given a JSON file with extra content" $ do
      wish <- decodeJSON "test-1.json"
      wish `shouldBe` Just (Wish {season = AutumnWinter, style = Casual, numberOfOutfits = From10to20})

  describe "decode" $ do
    it "returns nothing given a JSON file with wrong content - style" $ do
      wish <- decodeJSON "test-2.json"
      wish `shouldBe` Nothing

  describe "decode" $ do
    it "returns nothing given a JSON file with wrong content - season" $ do
      wish <- decodeJSON "test-3.json"
      wish `shouldBe` Nothing

  describe "decode" $ do
    it "returns nothing given a JSON file with no content" $ do
      wish <- decodeJSON "test-4.json"
      wish `shouldBe` Nothing

  describe "chooseCapsule" $ do
    it "returns a CapsuleWardrobe if given a Wish" $ do
      chooseCapsule autumnWinterCasualWish `shouldBe` autumnWinterCasualCW

  describe "countOutfits" $ do
    it "returns the number of possible outfits if given a CapsuleWardrobe" $ do
      countOutfits autumnWinterCasualCW `shouldBe` 12
      countOutfits springSummerCasualCW `shouldBe` 12
      countOutfits autumnWinterOfficeCW `shouldBe` 12
      countOutfits springSummerOfficeCW `shouldBe` 12
      countOutfits testCW `shouldBe` 34

  describe "toRange" $ do
    it "returns a the minimum e maximum from the range of outfits, given the desired Number Of Outfits" $ do
      toRange From10to20 `shouldBe` (10,20)
      toRange From21to30 `shouldBe` (21,30)
      toRange From31to40 `shouldBe` (31,40)
      toRange From31to40 `shouldBe` (41,50)
      toRange From31to40 `shouldBe` (51,60)
      toRange From31to40 `shouldBe` (61,70)

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
