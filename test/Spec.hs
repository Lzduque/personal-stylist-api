import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
import Lib 
  (getJSON
  , chooseCapsule
  , numOfOutfits
  )
import Wish
  ( Wish(..)
  , Style(..)
  , Season(..)
  )
import CapsuleWardrobe
  (CapsuleWardrobe(..)
  , Top(..)
  , Pants(..)
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

autumnWinterCasualWish = Wish {season = AutumnWinter, style = Casual}

aWCCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse]
                  , pants = [SkinnyJeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
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
      wish `shouldBe` Just (Wish {season = AutumnWinter, style = Casual})

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

  describe "numOfOutfits" $ do
    it "returns the number of possible outfits if given a CapsuleWardrobe" $ do
      numOfOutfits aWCCW `shouldBe` 9

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
