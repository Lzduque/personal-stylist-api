import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
import Lib
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
  , Shoes(..)
  , Purse(..)
  , autumnWinterCasualCW
  , springSummerCasualCW
  , autumnWinterOfficeCW
  , springSummerOfficeCW
  )
import Data.Aeson (decode)

autumnWinterCasualWish = Wish {season = AutumnWinter, style = Casual, numberOfOutfits = From10to20}
autumnWinterCasualWish1 = Wish {season = AutumnWinter, style = Casual, numberOfOutfits = From21to30}

testCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, ShortSleeveShirt, LongSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = [ShortSkirt, LongSkirt]
                  , dresses = [LongSleeveDress]
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [RelaxedBag]
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
    it "returns the minimum and the maximum from the range of outfits, given the desired Number Of Outfits" $ do
      toRange From10to20 `shouldBe` (10,20)
      toRange From21to30 `shouldBe` (21,30)
      toRange From31to40 `shouldBe` (31,40)
      toRange From41to50 `shouldBe` (41,50)
      toRange From51to60 `shouldBe` (51,60)
      toRange From61to70 `shouldBe` (61,70)
      toRange From71to80 `shouldBe` (71,80)
      toRange From81to90 `shouldBe` (81,90)
      toRange From91to100 `shouldBe` (91,100)

  describe "inRange" $ do
    it "returns True or False whether the number is within the range" $ do
      inRange 1 (10,20) `shouldBe` False
      inRange 11 (10,20) `shouldBe` True
      inRange 20 (10,20) `shouldBe` True
      inRange 21 (10,20) `shouldBe` False

  describe "countOccurrences" $ do
    it "counts the number of times the element appears in the list" $ do
      countOccurrences 1 [1,2,3,4,1] `shouldBe` 2
      countOccurrences LongSleeveShirt [LongSleeveBlouse, LongSleeveBlouse] `shouldBe` 0



-- Logic for Pants
  describe "springSummerCasualPants" $ do
    it "chooses a specifc type of pants given the Capsule Wardrobe" $ do
      springSummerCasualPants testCW `shouldBe` JeansShorts

  describe "autumnWinterCasualPants" $ do
    it "chooses a specifc type of pants given the Capsule Wardrobe" $ do
      autumnWinterCasualPants testCW `shouldBe` Leggings

  describe "springSummerOfficePants" $ do
    it "chooses a specifc type of pants given the Capsule Wardrobe" $ do
      springSummerOfficePants testCW `shouldBe` SocialShorts

  describe "autumnWinterOfficePants" $ do
    it "chooses a specifc type of pants given the Capsule Wardrobe" $ do
      autumnWinterOfficePants testCW `shouldBe` DressTrousers



-- Logic for Skirt
  describe "springSummerCasualSkirt" $ do
    it "chooses a specifc type of skirt given the Capsule Wardrobe" $ do
      springSummerCasualSkirt testCW `shouldBe` ShortSkirt

  describe "autumnWinterCasualSkirt" $ do
    it "chooses a specifc type of skirt given the Capsule Wardrobe" $ do
      autumnWinterCasualSkirt testCW `shouldBe` ShortSkirt

  describe "springSummerOfficeSkirt" $ do
    it "chooses a specifc type of skirt given the Capsule Wardrobe" $ do
      springSummerOfficeSkirt testCW `shouldBe` ShortSkirt

  describe "autumnWinterOfficeSkirt" $ do
    it "chooses a specifc type of skirt given the Capsule Wardrobe" $ do
      autumnWinterOfficeSkirt testCW `shouldBe` ShortSkirt



-- Logic for Overalls
  describe "springSummerCasualOverall" $ do
    it "chooses a specifc type of overall given the Capsule Wardrobe" $ do
      springSummerCasualOverall testCW `shouldBe` Sweatshirt

  describe "autumnWinterCasualOverall" $ do
    it "chooses a specifc type of overall given the Capsule Wardrobe" $ do
      autumnWinterCasualOverall testCW `shouldBe` Jacket

  describe "springSummerOfficeOverall" $ do
    it "chooses a specifc type of overall given the Capsule Wardrobe" $ do
      springSummerOfficeOverall testCW `shouldBe` Vest

  describe "autumnWinterOfficeOverall" $ do
    it "chooses a specifc type of overall given the Capsule Wardrobe" $ do
      autumnWinterOfficeOverall testCW `shouldBe` Sweater



-- Logic for Dress
  describe "springSummerCasualDress" $ do
    it "chooses a specifc type of dress given the Capsule Wardrobe" $ do
      springSummerCasualDress testCW `shouldBe` ShortSleeveDress

  describe "autumnWinterCasualDress" $ do
    it "chooses a specifc type of dress given the Capsule Wardrobe" $ do
      autumnWinterCasualDress testCW `shouldBe` LongSleeveDress

  describe "springSummerOfficeDress" $ do
    it "chooses a specifc type of dress given the Capsule Wardrobe" $ do
      springSummerOfficeDress testCW `shouldBe` ShortSleeveDress

  describe "autumnWinterOfficeDress" $ do
    it "chooses a specifc type of dress given the Capsule Wardrobe" $ do
      autumnWinterOfficeDress testCW `shouldBe` LongSleeveDress



-- Logic for Tops
  describe "springSummerCasualTop" $ do
    it "chooses a specifc type of top given the Capsule Wardrobe" $ do
      springSummerCasualTop testCW `shouldBe` ShortSleeveShirt

  describe "autumnWinterCasualTop" $ do
    it "chooses a specifc type of top given the Capsule Wardrobe" $ do
      autumnWinterCasualTop testCW `shouldBe` LongSleeveShirt

  describe "springSummerOfficeTop" $ do
    it "chooses a specifc type of top given the Capsule Wardrobe" $ do
      springSummerOfficeTop testCW `shouldBe` ShortSleeveBlouse

  describe "autumnWinterOfficeTop" $ do
    it "chooses a specifc type of top given the Capsule Wardrobe" $ do
      autumnWinterOfficeTop testCW `shouldBe` LongSleeveBlouse



-- ------------ Add Clothes function
  describe "addTop" $ do
    it "returns a capsule Wardrobe with a new top given one Capsule Wardrobe" $ do
      addTop autumnWinterCasualWish1 testCW `shouldBe` 
        CapsuleWardrobe 
          {tops = [LongSleeveShirt,LongSleeveShirt,ShortSleeveShirt,LongSleeveBlouse,LongSleeveShirt]
          , pants = [Jeans,Jeans]
          , skirts = [ShortSkirt,LongSkirt]
          , dresses = [LongSleeveDress]
          , overalls = [Cardigan,TrenchCoat]
          , shoes = [Boots,Flats]
          , purses = [RelaxedBag]}

  describe "addDress" $ do
    it "returns a capsule Wardrobe with a new dress given one Capsule Wardrobe" $ do
      addDress autumnWinterCasualWish1 testCW `shouldBe` 
        CapsuleWardrobe 
          {tops = [LongSleeveShirt,LongSleeveShirt,ShortSleeveShirt,LongSleeveBlouse]
          , pants = [Jeans,Jeans]
          , skirts = [ShortSkirt,LongSkirt]
          , dresses = [LongSleeveDress,LongSleeveDress]
          , overalls = [Cardigan,TrenchCoat]
          , shoes = [Boots,Flats]
          , purses = [RelaxedBag]}

  describe "addOverall" $ do
    it "returns a capsule Wardrobe with a new overall given one Capsule Wardrobe" $ do
      addOverall autumnWinterCasualWish1 testCW `shouldBe` 
        CapsuleWardrobe 
          {tops = [LongSleeveShirt,LongSleeveShirt,ShortSleeveShirt,LongSleeveBlouse]
          , pants = [Jeans,Jeans]
          , skirts = [ShortSkirt,LongSkirt]
          , dresses = [LongSleeveDress]
          , overalls = [Cardigan,TrenchCoat,Jacket]
          , shoes = [Boots,Flats]
          , purses = [RelaxedBag]}

  describe "addBottom" $ do
    it "returns a capsule Wardrobe with a new bottom given one Capsule Wardrobe" $ do
      addBottom autumnWinterCasualWish1 testCW `shouldBe` 
        CapsuleWardrobe 
          {tops = [LongSleeveShirt,LongSleeveShirt,ShortSleeveShirt,LongSleeveBlouse]
          , pants = [Jeans,Jeans,Leggings]
          , skirts = [ShortSkirt,LongSkirt]
          , dresses = [LongSleeveDress]
          , overalls = [Cardigan,TrenchCoat]
          , shoes = [Boots,Flats]
          , purses = [RelaxedBag]}

  describe "addSkirt" $ do
    it "returns a capsule Wardrobe with a new skirt given one Capsule Wardrobe" $ do
      addSkirt autumnWinterCasualWish1 testCW `shouldBe` 
        CapsuleWardrobe 
          {tops = [LongSleeveShirt,LongSleeveShirt,ShortSleeveShirt,LongSleeveBlouse]
          , pants = [Jeans,Jeans]
          , skirts = [ShortSkirt,LongSkirt,ShortSkirt]
          , dresses = [LongSleeveDress]
          , overalls = [Cardigan,TrenchCoat]
          , shoes = [Boots,Flats]
          , purses = [RelaxedBag]}

  describe "addPants" $ do
    it "returns a capsule Wardrobe with a new pants given one Capsule Wardrobe" $ do
      addPants autumnWinterCasualWish1 testCW `shouldBe` 
        CapsuleWardrobe 
          {tops = [LongSleeveShirt,LongSleeveShirt,ShortSleeveShirt,LongSleeveBlouse]
          , pants = [Jeans,Jeans,Leggings]
          , skirts = [ShortSkirt,LongSkirt]
          , dresses = [LongSleeveDress]
          , overalls = [Cardigan,TrenchCoat]
          , shoes = [Boots,Flats]
          , purses = [RelaxedBag]}



-- ------------ MAIN FUNCTIONS
  describe "makeCapsule" $ do
    it "returns a final capsule Wardrobe with a new item(s) of clothing given one Capsule Wardrobe" $ do
      makeCapsule autumnWinterCasualWish1 autumnWinterCasualCW `shouldBe` 
        CapsuleWardrobe 
          {tops = [LongSleeveShirt,LongSleeveShirt,LongSleeveBlouse,LongSleeveBlouse]
          , pants = [Jeans,Jeans]
          , skirts = [ShortSkirt]
          , dresses = [LongSleeveDress]
          , overalls = [Sweater,TrenchCoat]
          , shoes = [Flats,Boots]
          , purses = [RelaxedBag]}
      countOutfits (makeCapsule autumnWinterCasualWish1 autumnWinterCasualCW) `shouldBe` 26

  describe "addMoreClothes" $ do
    it "returns a capsule Wardrobe with a new item of clothing given one Capsule Wardrobe" $ do
      addMoreClothes autumnWinterCasualWish1 autumnWinterCasualCW `shouldBe` 
        CapsuleWardrobe 
          {tops = [LongSleeveShirt,LongSleeveShirt,LongSleeveBlouse]
          , pants = [Jeans,Jeans]
          , skirts = []
          , dresses = [LongSleeveDress]
          , overalls = [Sweater,TrenchCoat]
          , shoes = [Boots,Flats]
          , purses = [RelaxedBag]}



    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException


