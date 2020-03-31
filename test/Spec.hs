import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
import Lib
import CapsuleWardrobe
import Data.Aeson (decode)



baseCW = 
  CapsuleWardrobe 
    { season = undefined
    , style = undefined
    , numberOfOutfits = undefined
    , colors = []
    , wardrobe = 
      Wardrobe
        { tops = []
        , pants = []
        , skirts = []
        , dresses = []
        , overalls = []
        , shoes = []
        , purses = []
        }
    }

testWardrobe =
  Wardrobe { tops = [LongSleeveShirt, LongSleeveShirt, ShortSleeveShirt, LongSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = [ShortSkirt, LongSkirt]
                  , dresses = [LongSleeveDress]
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [RelaxedBag]
                  }



decodeJSON :: FilePath -> IO (Maybe CapsuleWardrobe)
decodeJSON filePath = do
  str <- getJSON filePath
  return $ decode str

main :: IO ()
main = hspec $ do
  describe "decode" $ do
    it "returns a CapsuleWardrobe given a JSON file with extra content" $ do
      capsule <- decodeJSON "test-1.json"
      capsule `shouldBe` Just (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From10to20, colors = [Navy,OffWhite,LightYellow,Beige,Brown,LightBlue,LightPink,LightPurple,LightGreen]})

  describe "decode" $ do
    it "returns nothing given a JSON file with wrong content - style" $ do
      capsule <- decodeJSON "test-2.json"
      capsule `shouldBe` Nothing

  describe "decode" $ do
    it "returns nothing given a JSON file with wrong content - season" $ do
      capsule <- decodeJSON "test-3.json"
      capsule `shouldBe` Nothing

  describe "decode" $ do
    it "returns nothing given a JSON file with no content" $ do
      capsule <- decodeJSON "test-4.json"
      capsule `shouldBe` Nothing

  describe "setUpBaseWardrobe" $ do
    it "returns a CapsuleWardrobe if given a CapsuleWardrobe" $ do
      let cw = setUpBaseWardrobe (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops =
                  [LongSleeveShirt,LongSleeveShirt,LongSleeveBlouse]
              , pants =
                  [Jeans, Jeans]
              , skirts = []
              , dresses =
                  []
              , overalls =
                  [Sweater, TrenchCoat]
              , shoes =
                  [Boots,Flats]
              , purses =
                  [RelaxedBag]
              }

  describe "countOutfits" $ do
    it "returns the number of possible outfits if given a CapsuleWardrobe" $ do
      countOutfits autumnWinterCasual `shouldBe` 12
      countOutfits springSummerCasual `shouldBe` 12
      countOutfits autumnWinterOffice `shouldBe` 12
      countOutfits springSummerOffice `shouldBe` 12
      countOutfits testWardrobe `shouldBe` 34

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



-- ------------ Add Clothes function
  describe "addTop" $ do
    it "returns a capsule Wardrobe with a new top given one Capsule Wardrobe" $ do
      let cw = addTop (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = [LongSleeveShirt]
              , pants = []
              , skirts = []
              , dresses = []
              , overalls = []
              , shoes = []
              , purses = []
              }

  describe "addDress" $ do
    it "returns a capsule Wardrobe with a new dress given one Capsule Wardrobe" $ do
      let cw = addDress (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = []
              , skirts = []
              , dresses = [LongSleeveDress]
              , overalls = []
              , shoes = []
              , purses = []
              }

  describe "addOverall" $ do
    it "returns a capsule Wardrobe with a new overall given one Capsule Wardrobe" $ do
      let cw = addOverall (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = []
              , skirts = []
              , dresses = []
              , overalls = [Cardigan]
              , shoes = []
              , purses = []
              }

  describe "addBottom" $ do
    it "returns a capsule Wardrobe with a new bottom given one Capsule Wardrobe" $ do
      let cw = addBottom (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = [Leggings]
              , skirts = []
              , dresses = []
              , overalls = []
              , shoes = []
              , purses = []
              }

  describe "addSkirt" $ do
    it "returns a capsule Wardrobe with a new skirt given one Capsule Wardrobe" $ do
      let cw = addSkirt (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = []
              , skirts = [ShortSkirt]
              , dresses = []
              , overalls = []
              , shoes = []
              , purses = []
              }

  describe "addPants" $ do
    it "returns a capsule Wardrobe with new pants given one Capsule Wardrobe" $ do
      let cw = addPants (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = [Leggings]
              , skirts = []
              , dresses = []
              , overalls = []
              , shoes = []
              , purses = []
              }

  describe "addAccessories" $ do
    it "returns a capsule Wardrobe with new accessories given one Capsule Wardrobe and the actual number of Outfits" $ do
      let cw = addAccessories (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, wardrobe = Wardrobe
                { tops =
                    [LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, LongSleeveBlouse]
                , pants =
                    [Jeans, Jeans, Leggings]
                , skirts = [ ShortSkirt ]
                , dresses =
                    [LongSleeveDress, LongSleeveDress]
                , overalls =
                    [Sweater, Cardigan, TrenchCoat]
                , shoes =
                    []
                , purses =
                    []
                }
            }) 
      wardrobe cw `shouldBe` 
            Wardrobe
                { tops =
                    [LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, LongSleeveBlouse]
                , pants =
                    [Jeans, Jeans, Leggings]
                , skirts = [ ShortSkirt ]
                , dresses =
                    [LongSleeveDress, LongSleeveDress]
                , overalls =
                    [Sweater, Cardigan, TrenchCoat]
                , shoes =
                    [Sneakers]
                , purses =
                    []
                }

  describe "addShoes" $ do
    it "returns a capsule Wardrobe with new shoes given one Capsule Wardrobe" $ do
      let cw = addShoes (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, wardrobe = Wardrobe
                { tops =
                    [LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, LongSleeveBlouse]
                , pants =
                    [Jeans, Jeans, Leggings]
                , skirts = [ ShortSkirt ]
                , dresses =
                    [LongSleeveDress, LongSleeveDress]
                , overalls =
                    [Sweater, Cardigan, TrenchCoat]
                , shoes =
                    []
                , purses =
                    []
                }
            }) 
      wardrobe cw `shouldBe` 
            Wardrobe
                { tops =
                    [LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, LongSleeveBlouse]
                , pants =
                    [Jeans, Jeans, Leggings]
                , skirts = [ ShortSkirt ]
                , dresses =
                    [LongSleeveDress, LongSleeveDress]
                , overalls =
                    [Sweater, Cardigan, TrenchCoat]
                , shoes =
                    [Sneakers]
                , purses =
                    []
                }

  describe "addPurse" $ do
    it "returns a capsule Wardrobe with a new purse given one Capsule Wardrobe" $ do
      let cw = addPurse (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, wardrobe = Wardrobe
                { tops =
                    [LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, LongSleeveBlouse]
                , pants =
                    [Jeans, Jeans, Leggings]
                , skirts = [ ShortSkirt ]
                , dresses =
                    [LongSleeveDress, LongSleeveDress, LongSleeveDress]
                , overalls =
                    [Sweater, Cardigan, Cardigan, TrenchCoat]
                , shoes =
                    []
                , purses =
                    []
                }
            }) 
      wardrobe cw `shouldBe` 
            Wardrobe
                { tops =
                    [LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, LongSleeveBlouse]
                , pants =
                    [Jeans, Jeans, Leggings]
                , skirts = [ ShortSkirt ]
                , dresses =
                    [LongSleeveDress, LongSleeveDress, LongSleeveDress]
                , overalls =
                    [Sweater, Cardigan, Cardigan, TrenchCoat]
                , shoes =
                    []
                , purses =
                    [RelaxedBag]
                }



-- ------------ MAIN FUNCTIONS
  describe "fillUpWardrobe" $ do
    it "returns a final capsule Wardrobe with a new item(s) of clothing given one Capsule Wardrobe" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From61to70}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = [ShortSleeveShirt,ShortSleeveShirt,ShortSleeveShirt,ShortSleeveBlouse,ShortSleeveBlouse,TankTop,TankTop]
              , pants = [JeansShorts,Leggings]
              , skirts = [ShortSkirt]
              , dresses = [ShortSleeveDress,ShortSleeveDress]
              , overalls = [Cardigan,Blazer,Sweatshirt]
              , shoes = [Sandals,Loafers,Sneakers,Wedges]
              , purses = [RelaxedBag,RelaxedBag,RelaxedBag]
              }
      countOutfits (wardrobe cw) `shouldBe` 69

      let cw = fillUpWardrobe (baseCW {season = AutumnWinter, style = Office, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = [LongSleeveShirt,LongSleeveShirt,LongSleeveBlouse,LongSleeveBlouse,LongSleeveBlouse,LongSleeveBlouse,LongSleeveBlouse]
              , pants = [DressTrousers,DressTrousers,DressTrousers]
              , skirts = [ShortSkirt]
              , dresses = [LongSleeveDress,LongSleeveDress]
              , overalls = [Sweater,Vest,Blazer]
              , shoes = [Flats,Heels,Heels,AnkleBoots,Boots]
              , purses = [StructuredBag,StructuredBag,StructuredBag]
              }
      countOutfits (wardrobe cw) `shouldBe` 90

  describe "addMoreClothes" $ do
    it "returns a capsule Wardrobe with a new item of clothing given one Capsule Wardrobe" $ do
      let cw = fillUpWardrobe (baseCW {season = AutumnWinter, style = Office, numberOfOutfits = From81to90}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = [LongSleeveShirt,LongSleeveShirt,LongSleeveBlouse,LongSleeveBlouse,LongSleeveBlouse,LongSleeveBlouse,LongSleeveBlouse]
              , pants = [DressTrousers,DressTrousers,DressTrousers]
              , skirts = [ShortSkirt]
              , dresses = [LongSleeveDress,LongSleeveDress]
              , overalls = [Sweater,Vest,Blazer]
              , shoes = [Flats,Heels,Heels,AnkleBoots,Boots]
              , purses = [StructuredBag,StructuredBag,StructuredBag]
              }

  describe "groupByClothing" $ do
    it "returns a displayable capsule Wardrobe with the clothes, numbers and colors given a wish and a Capsule Wardrobe" $ do
      groupByClothing (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = [Navy,OffWhite,LightYellow,Beige,Brown,LightBlue,LightPink,LightPurple,LightGreen], wardrobe = Wardrobe
                { tops =
                    [LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, LongSleeveBlouse]
                , pants =
                    [Jeans, Jeans, Leggings]
                , skirts = [ ShortSkirt ]
                , dresses =
                    [LongSleeveDress, LongSleeveDress, LongSleeveDress]
                , overalls =
                    [Sweater, Cardigan, Cardigan, TrenchCoat]
                , shoes =
                    []
                , purses =
                    []
                }
            }) `shouldBe` 
        [
          ("LongSleeveShirt",4,[Navy,OffWhite,LightYellow,Beige])
          ,("LongSleeveBlouse",3,[Navy,OffWhite,LightYellow])
          ,("Jeans",2,[Navy,OffWhite])
          ,("Leggings",1,[Navy])
          ,("ShortSkirt",1,[Navy])
          ,("LongSleeveDress",3,[LightPink,LightPurple,LightGreen])
          ,("Sweater",1,[Navy])
          ,("Cardigan",2,[Navy,OffWhite])
          ,("TrenchCoat",1,[Navy])
          ]



    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException


