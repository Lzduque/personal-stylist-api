import Test.Hspec
import Data.Aeson (decode)
import Data.Either (isRight)

import Lib
import CapsuleWardrobe



baseCW = 
  CapsuleWardrobe 
    { season = undefined
    , style = undefined
    , numberOfOutfits = undefined
    , colors = 
      ColorsTypes
        { mains = [],
          neutrals = [],
          accents = []
        }
    , preferences = []
    , wardrobe = 
      Wardrobe
        { tops = []
        , pants = []
        , skirts = []
        , dresses = []
        , coats = []
        , shoes = []
        , purses = []
        }
    }

testWardrobe =
    Wardrobe
      { tops = [ShirtTop,ShirtTop,ShirtTop,TShirtTankTop,TShirtTankTop]
      , pants = [JeansPants,Shorts]
      , skirts = []
      , dresses = [DayDress,DayDress]
      , coats = [Sweater,Jacket]
      , shoes = [Shoes]
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
      capsule `shouldBe` Just (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From10to20, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Skirts,Dresses,Pants]})

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

  describe "countOutfits" $ do
    it "returns the number of possible outfits if given a CapsuleWardrobe" $ do
      countOutfits testWardrobe `shouldBe` 24

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
      toRange From101to110 `shouldBe` (101,110)
      toRange From111to120 `shouldBe` (111,120)
      toRange From121to130 `shouldBe` (121,130)
      toRange From131to140 `shouldBe` (131,140)
      toRange From141to150 `shouldBe` (141,150)
      toRange From151to160 `shouldBe` (151,160)
      toRange From161to170 `shouldBe` (161,170)
      toRange From171to180 `shouldBe` (171,180)
      toRange From181to190 `shouldBe` (181,190)
      toRange From191to200 `shouldBe` (191,200)
      toRange From201to210 `shouldBe` (201,210)
      toRange From211to220 `shouldBe` (211,220)
      toRange From221to230 `shouldBe` (221,230)
      toRange From231to240 `shouldBe` (231,240)
      toRange From241to250 `shouldBe` (241,250)
      toRange From251to260 `shouldBe` (251,260)
      toRange From261to270 `shouldBe` (261,270)
      toRange From271to280 `shouldBe` (271,280)
      toRange From281to290 `shouldBe` (281,290)
      toRange From291to300 `shouldBe` (291,300)

  describe "inRange" $ do
    it "returns True or False whether the number is within the range" $ do
      inRange 1 (10,20) `shouldBe` False
      inRange 11 (10,20) `shouldBe` True
      inRange 20 (10,20) `shouldBe` True
      inRange 21 (10,20) `shouldBe` False

  describe "countOccurrences" $ do
    it "counts the number of times the element appears in the list" $ do
      countOccurrences 1 [1,2,3,4,1] `shouldBe` 2
      countOccurrences Sweater [Blazer] `shouldBe` 0



-- ------------ Add Clothes function
  describe "addTop" $ do
    it "returns a capsule Wardrobe with a new top given one Capsule Wardrobe" $ do
      let cw = addTop (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts]})
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = [ShirtTop]
              , pants = []
              , skirts = []
              , dresses = []
              , coats = []
              , shoes = []
              , purses = []
              }

  describe "addDress" $ do
    it "returns a capsule Wardrobe with a new dress given one Capsule Wardrobe" $ do
      let cw = addDress (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts]}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = []
              , skirts = []
              , dresses = [DayDress]
              , coats = []
              , shoes = []
              , purses = []
              }

  describe "addCoat" $ do
    it "returns a capsule Wardrobe with a new coat given one Capsule Wardrobe" $ do
      let cw = addCoat (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts]}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = []
              , skirts = []
              , dresses = []
              , coats = [Sweater]
              , shoes = []
              , purses = []
              }

  describe "addBottom" $ do
    it "returns a capsule Wardrobe with a new bottom given one Capsule Wardrobe" $ do
      let cw = addBottom (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts]}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = [JeansPants]
              , skirts = []
              , dresses = []
              , coats = []
              , shoes = []
              , purses = []
              }

  describe "addSkirt" $ do
    it "returns a capsule Wardrobe with a new skirt given one Capsule Wardrobe" $ do
      let cw = addSkirt (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts]}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = []
              , skirts = [DaySkirt]
              , dresses = []
              , coats = []
              , shoes = []
              , purses = []
              }

  describe "addPants" $ do
    it "returns a capsule Wardrobe with new pants given one Capsule Wardrobe" $ do
      let cw = addPants (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts]}) 
      wardrobe cw `shouldBe` 
            Wardrobe
              { tops = []
              , pants = [JeansPants]
              , skirts = []
              , dresses = []
              , coats = []
              , shoes = []
              , purses = []
              }

  describe "addAccessories" $ do
    it "returns a capsule Wardrobe with new accessories given one Capsule Wardrobe and the actual number of Outfits" $ do
      let cw = addAccessories (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts], wardrobe = Wardrobe
                { tops =
                    [ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop]
                , pants =
                    [JeansPants, JeansPants, Leggings]
                , skirts = [ DaySkirt ]
                , dresses =
                    [DayDress, DayDress]
                , coats =
                    [Sweater]
                , shoes =
                    []
                , purses =
                    []
                }
            }) 
      wardrobe cw `shouldBe` 
            Wardrobe
                { tops =
                    [ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop]
                , pants =
                    [JeansPants, JeansPants, Leggings]
                , skirts = [ DaySkirt ]
                , dresses =
                    [DayDress, DayDress]
                , coats =
                    [Sweater]
                , shoes =
                    [Shoes]
                , purses =
                    [RelaxedBag]
                }

  describe "addShoes" $ do
    it "returns a capsule Wardrobe with new shoes given one Capsule Wardrobe" $ do
      let cw = addShoes (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts], wardrobe = Wardrobe
                { tops =
                    [ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop]
                , pants =
                    [JeansPants, JeansPants, Leggings]
                , skirts = [ DaySkirt ]
                , dresses =
                    [DayDress, DayDress]
                , coats =
                    [Sweater]
                , shoes =
                    []
                , purses =
                    []
                }
            }) 
      wardrobe cw `shouldBe` 
            Wardrobe
                { tops =
                    [ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop]
                , pants =
                    [JeansPants, JeansPants, Leggings]
                , skirts = [ DaySkirt ]
                , dresses =
                    [DayDress, DayDress]
                , coats =
                    [Sweater]
                , shoes =
                    [Shoes]
                , purses =
                    []
                }

  describe "addPurse" $ do
    it "returns a capsule Wardrobe with a new purse given one Capsule Wardrobe" $ do
      let cw = addPurse (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts], wardrobe = Wardrobe
                { tops =
                    [ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop]
                , pants =
                    [JeansPants, JeansPants, Leggings]
                , skirts = [ DaySkirt ]
                , dresses =
                    [DayDress, DayDress, DayDress]
                , coats =
                    [Sweater]
                , shoes =
                    []
                , purses =
                    []
                }
            }) 
      wardrobe cw `shouldBe` 
            Wardrobe
                { tops =
                    [ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop]
                , pants =
                    [JeansPants, JeansPants, Leggings]
                , skirts = [ DaySkirt ]
                , dresses =
                    [DayDress, DayDress, DayDress]
                , coats =
                    [Sweater]
                , shoes =
                    []
                , purses =
                    [RelaxedBag]
                }



-- ------------ MAIN FUNCTIONS
  describe "fillUpWardrobe" $ do
    it "returns a final capsule Wardrobe with a new item(s) of clothing given one Capsule Wardrobe with dresses and pants as preferences" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From61to70, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,ShortsPants]})
      cw `shouldSatisfy` isRight
      let (Right capsule) = cw
      wardrobe capsule `shouldBe` 
            Wardrobe
              { tops = [ShirtTop,ShirtTop,ShirtTop,ShirtTop,TShirtTankTop,TShirtTankTop,TShirtTankTop]
              , pants = [JeansPants,JeansPants,Shorts]
              , skirts = []
              , dresses = [DayDress,DayDress]
              , coats = [Sweater,Sweater,Jacket]
              , shoes = [Shoes,Shoes,Shoes]
              , purses = [RelaxedBag,RelaxedBag,RelaxedBag]
              }
      countOutfits (wardrobe capsule) `shouldBe` 69

    it "returns a final capsule Wardrobe with a new item(s) of clothing given one Capsule Wardrobe with skirts and pants as preferences" $ do
      let cw = fillUpWardrobe (baseCW {season = AutumnWinter, style = Office, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Pants,Skirts]}) 
      cw `shouldSatisfy` isRight
      let (Right capsule) = cw
      wardrobe capsule `shouldBe` 
            Wardrobe
              { tops = [ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop]
              , pants = [DressPants,DressPants,DressPants]
              , skirts = [BusinessSkirt]
              , dresses = []
              , coats = [Sweater,Sweater,Blazer]
              , shoes = [Shoes,Shoes,Shoes,Shoes]
              , purses = [StructuredBag,StructuredBag,StructuredBag,StructuredBag]
              }
      countOutfits (wardrobe capsule) `shouldBe` 84

    it "returns a error message given one Capsule Wardrobe without preferences and colors" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From61to70, colors = ColorsTypes { mains = [], neutrals = [], accents = []}, preferences = []})
      cw `shouldBe` Left Error { error = True, message = "Please, select at least one preference and one color."} 

    it "returns a error message given one Capsule Wardrobe without preferences" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From61to70, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = []})
      cw `shouldBe` Left Error { error = True, message = "Please, select at least one preference."}

    it "returns a error message given one Capsule Wardrobe without colors" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From61to70, colors = ColorsTypes { mains = [], neutrals = [], accents = []}, preferences = [Dresses]})
      cw `shouldBe` Left Error { error = True, message = "Please, select at least one color."}

    it "returns a error message given one Capsule Wardrobe without any bottom preference" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From61to70, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [LeggingsPants]})
      cw `shouldBe` Left Error { error = True, message = "Please, select at least one of these preferences: Skirts, Dresses or Pants."}

    it "returns a error message given one Capsule Wardrobe with more outfits than it was asked for" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From61to70, colors = ColorsTypes { mains = [], neutrals = [], accents = []}, preferences = [Dresses], wardrobe = 
        Wardrobe
          { tops = []
          , pants = []
          , skirts = []
          , dresses = [BusinessDress,BusinessDress,BusinessDress,BusinessDress,BusinessDress,BusinessDress,BusinessDress,BusinessDress,BusinessDress,BusinessDress,BusinessDress,BusinessDress]
          , coats = [Blazer,Blazer,Blazer,Blazer,Blazer,Blazer]
          , shoes = [Shoes,Shoes,Shoes]
          , purses = [StructuredBag,StructuredBag,StructuredBag]
          }
      })
      cw `shouldBe` Left Error { error = True, message = "No capsule can be generated within this range, for these parameters. Please, change the number of outfits." }

  describe "addMoreClothes" $ do
    it "returns a capsule Wardrobe with a new item of clothing given one Capsule Wardrobe - From31to40" $ do
      let cw = fillUpWardrobe (baseCW {season = AutumnWinter, style = Office, numberOfOutfits = From31to40, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Skirts]}) 
      cw `shouldSatisfy` isRight
      let (Right capsule) = cw
      wardrobe capsule `shouldBe` 
            Wardrobe
              { tops = [ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop]
              , pants = []
              , skirts = [BusinessSkirt,BusinessSkirt,BusinessSkirt]
              , dresses = [BusinessDress]
              , coats = [Sweater,Blazer]
              , shoes = [Shoes,Shoes]
              , purses = [StructuredBag,StructuredBag]
              }
    it "returns a capsule Wardrobe with a new item of clothing given one Capsule Wardrobe - From81to90" $ do
      let cw = fillUpWardrobe (baseCW {season = AutumnWinter, style = Office, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts]}) 
      cw `shouldSatisfy` isRight
      let (Right capsule) = cw
      wardrobe capsule `shouldBe` 
            Wardrobe
              { tops = [ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop]
              , pants = [DressPants,DressPants,DressPants]
              , skirts = [BusinessSkirt]
              , dresses = [BusinessDress,BusinessDress]
              , coats = [Sweater,Sweater,Blazer]
              , shoes = [Shoes,Shoes,Shoes,Shoes]
              , purses = [StructuredBag,StructuredBag, StructuredBag,StructuredBag]
              }
    it "returns a capsule Wardrobe with a new item of clothing given one Capsule Wardrobe - From141to150" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From141to150, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts,ShortsPants]}) 
      cw `shouldSatisfy` isRight
      let (Right capsule) = cw
      wardrobe capsule `shouldBe` 
            Wardrobe
              { tops = [ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop,TShirtTankTop,TShirtTankTop,TShirtTankTop,TShirtTankTop]
              , pants = [JeansPants,JeansPants,Shorts]
              , skirts = [DaySkirt,DaySkirt]
              , dresses = [DayDress,DayDress]
              , coats = [Sweater,Sweater,Jacket]
              , shoes = [Shoes,Shoes,Shoes,Shoes]
              , purses = [RelaxedBag,RelaxedBag,RelaxedBag,RelaxedBag]
              }
    it "returns a capsule Wardrobe with a new item of clothing given one Capsule Wardrobe - From181to190" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From181to190, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts,ShortsPants]}) 
      cw `shouldSatisfy` isRight
      let (Right capsule) = cw
      wardrobe capsule `shouldBe` 
            Wardrobe
              { tops = [ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop,TShirtTankTop,TShirtTankTop,TShirtTankTop,TShirtTankTop]
              , pants = [JeansPants,JeansPants,Shorts]
              , skirts = [DaySkirt,DaySkirt]
              , dresses = [DayDress,DayDress]
              , coats = [Sweater,Sweater,Jacket,Jacket]
              , shoes = [Shoes,Shoes,Shoes,Shoes]
              , purses = [RelaxedBag,RelaxedBag,RelaxedBag,RelaxedBag]
              }
    it "returns a capsule Wardrobe with a new item of clothing given one Capsule Wardrobe - From271to280" $ do
      let cw = fillUpWardrobe (baseCW {season = SpringSummer, style = Casual, numberOfOutfits = From271to280, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts,ShortsPants]}) 
      cw `shouldSatisfy` isRight
      let (Right capsule) = cw
      wardrobe capsule `shouldBe` 
            Wardrobe
              { tops = [ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop,ShirtTop,TShirtTankTop,TShirtTankTop,TShirtTankTop,TShirtTankTop,TShirtTankTop]
              , pants = [JeansPants,JeansPants,Shorts,Shorts]
              , skirts = [DaySkirt,DaySkirt]
              , dresses = [DayDress,DayDress]
              , coats = [Sweater,Sweater,Jacket,Jacket]
              , shoes = [Shoes,Shoes,Shoes,Shoes]
              , purses = [RelaxedBag,RelaxedBag,RelaxedBag,RelaxedBag]
              }

  describe "groupByClothing" $ do
    it "returns a displayable capsule Wardrobe with the clothes, numbers and colors given a wish and a Capsule Wardrobe" $ do
      groupByClothing (baseCW {season = AutumnWinter, style = Casual, numberOfOutfits = From81to90, colors = ColorsTypes { mains = [Navy,OffWhite], neutrals = [LightYellow,Beige], accents = [Brown,LightBlue]}, preferences = [Dresses,Pants,Skirts], wardrobe = Wardrobe
                { tops =
                    [ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop, ShirtTop]
                , pants =
                    [JeansPants, JeansPants, Leggings]
                , skirts = [ DaySkirt ]
                , dresses =
                    [DayDress, DayDress, DayDress]
                , coats =
                    [Sweater]
                , shoes =
                    [Shoes]
                , purses =
                    [RelaxedBag]
                }
            }) `shouldBe` 
        [
         ("ShirtTop",7,[Navy,OffWhite,LightYellow,Beige,Brown,LightBlue])
        ,("JeansPants",2,[Navy,OffWhite])
        ,("Leggings",1,[Navy])
        ,("DaySkirt",1,[Navy])
        ,("DayDress",3,[Brown,LightBlue,LightYellow])
        ,("Sweater",1,[Navy])
        ,("Shoes",1,[LightYellow])
        ,("RelaxedBag",1,[LightYellow])
        ]




    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException


