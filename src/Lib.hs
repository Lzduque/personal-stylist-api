module Lib where
import Wish
  ( Wish(..)
  , Style(..)
  , Season(..)
  , NumberOfOutfits(..)
  )
import CapsuleWardrobe
  ( CapsuleWardrobe(..)
  , Clothing(..)
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
import qualified Data.ByteString.Lazy as B

-- ------------ HELPERS
-- get the json and transform in byte string
getJSON :: FilePath -> IO B.ByteString
getJSON filePath = B.readFile filePath

chooseCapsule :: Wish -> CapsuleWardrobe
chooseCapsule Wish {season = AutumnWinter, style = Casual} = autumnWinterCasualCW
chooseCapsule Wish {season = SpringSummer, style = Casual} = springSummerCasualCW
chooseCapsule Wish {season = AutumnWinter, style = Office} = autumnWinterOfficeCW
chooseCapsule Wish {season = SpringSummer, style = Office} = springSummerOfficeCW

countOutfits :: CapsuleWardrobe -> Int
countOutfits capsule = (numOfTops * numOfPants * numOfOveralls) + (numOfTops * numOfSkirts * numOfOveralls) + (numOfDresses * numOfOveralls) where
    numOfTops = length . tops $ capsule
    numOfPants = length . pants $ capsule
    numOfSkirts = length . skirts $ capsule
    numOfDresses = length . dresses $ capsule
    numOfOveralls = length . overalls $ capsule

toRange :: NumberOfOutfits -> (Int,Int)
toRange From10to20 = (10,20)
toRange From21to30 = (21,30)
toRange From31to40 = (31,40)
toRange From41to50 = (41,50)
toRange From51to60 = (51,60)
toRange From61to70 = (61,70)

inRange :: Int -> (Int,Int) -> Bool
inRange x (a,b)
  | a <= x && x <= b = True
  | otherwise = False

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (x==)



-- ------------ MAIN FUNCTIONS
makeCapsule :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
makeCapsule wish capsule
    | totalOutfits `inRange` rangeOfOutfits = capsule
    | totalOutfits > snd rangeOfOutfits = error "Total Outfits larger than the Range of Outfits wished"
    | otherwise = makeCapsule wish newCapsule
    where
      totalOutfits = countOutfits capsule
      rangeOfOutfits = toRange . numberOfOutfits $ wish
      newCapsule = addMoreClothes wish capsule

addMoreClothes :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
addMoreClothes wish capsule
    | topBottom > 2 = addBottom wish capsule
    | topBottom < 3/2 = addTop wish capsule
    | dressBottom < 1/3 = addDress wish capsule
    | topOverall >= 3 = addOverall wish capsule
    | topOverall < 2 = addBottom wish capsule
    | dressTop  <= 1/6 = addDress wish capsule
    | otherwise = addTop wish capsule
    where 
      numBottoms = fromIntegral $ (length . pants $ capsule) + (length . skirts $ capsule)
      topBottom = fromIntegral (length . tops $ capsule) / numBottoms
      dressBottom = fromIntegral (length . dresses $ capsule) / numBottoms
      topOverall = fromIntegral (length . tops $ capsule) / fromIntegral (length . overalls $ capsule)
      dressTop = fromIntegral (length . dresses $ capsule) / fromIntegral (length . tops $ capsule)



-- ------------ Add Clothes function
addTop :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
addTop wish capsule
  | wishedSeason == SpringSummer && wishedStyle == Casual = addToCapsule (springSummerCasualTop capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Casual = addToCapsule (autumnWinterCasualTop capsule) capsule
  | wishedSeason == SpringSummer && wishedStyle == Office = addToCapsule (springSummerOfficeTop capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Office = addToCapsule (autumnWinterOfficeTop capsule) capsule
  | otherwise = error "Wrong capsule selected - Top"
  where
    wishedSeason = season $ wish
    wishedStyle = style $ wish

addDress :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
addDress wish capsule
  | wishedSeason == SpringSummer && wishedStyle == Casual = addToCapsule (springSummerCasualDress capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Casual = addToCapsule (autumnWinterCasualDress capsule) capsule
  | wishedSeason == SpringSummer && wishedStyle == Office = addToCapsule (springSummerOfficeDress capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Office = addToCapsule (autumnWinterOfficeDress capsule) capsule
  | otherwise = error "Wrong capsule selected - Dress"
  where
    wishedSeason = season $ wish
    wishedStyle = style $ wish

addOverall :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
addOverall wish capsule
  | wishedSeason == SpringSummer && wishedStyle == Casual = addToCapsule (springSummerCasualOverall capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Casual = addToCapsule (autumnWinterCasualOverall capsule) capsule
  | wishedSeason == SpringSummer && wishedStyle == Office = addToCapsule (springSummerOfficeOverall capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Office = addToCapsule (autumnWinterOfficeOverall capsule) capsule
  | otherwise = error "Wrong capsule selected - Overall"
  where
    wishedSeason = season $ wish
    wishedStyle = style $ wish

addBottom :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
addBottom wish capsule
  | pantSkirt > 3 = addSkirt wish capsule
  | otherwise = addPants wish capsule
  where
    pantSkirt = fromIntegral (length . pants $ capsule) / fromIntegral (length . skirts $ capsule)

addSkirt :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
addSkirt wish capsule
  | wishedSeason == SpringSummer && wishedStyle == Casual = addToCapsule (springSummerCasualSkirt capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Casual = addToCapsule (autumnWinterCasualSkirt capsule) capsule
  | wishedSeason == SpringSummer && wishedStyle == Office = addToCapsule (springSummerOfficeSkirt capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Office = addToCapsule (autumnWinterOfficeSkirt capsule) capsule
  | otherwise = error "Wrong capsule selected - Skirt"
  where
    wishedSeason = season $ wish
    wishedStyle = style $ wish

addPants :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
addPants wish capsule
  | wishedSeason == SpringSummer && wishedStyle == Casual = addToCapsule (springSummerCasualPants capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Casual = addToCapsule (autumnWinterCasualPants capsule) capsule
  | wishedSeason == SpringSummer && wishedStyle == Office = addToCapsule (springSummerOfficePants capsule) capsule
  | wishedSeason == AutumnWinter && wishedStyle == Office = addToCapsule (autumnWinterOfficePants capsule) capsule
  | otherwise = error "Wrong capsule selected - Skirt"
  where
    wishedSeason = season $ wish
    wishedStyle = style $ wish



-- Logic for Tops
springSummerCasualTop :: CapsuleWardrobe -> Top
springSummerCasualTop capsule
  | numOfShortSleeveShirt < numOfTops / 3 = ShortSleeveShirt 
  | numOfShortSleeveBlouse < numOfTops / 3 = ShortSleeveBlouse 
  | otherwise = TankTop
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops capsule
    numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops capsule

autumnWinterCasualTop :: CapsuleWardrobe -> Top
autumnWinterCasualTop capsule
  | numOfLongSleeveShirt < numOfTops / 2 = LongSleeveShirt 
  | otherwise = LongSleeveBlouse
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfLongSleeveShirt = fromIntegral . countOccurrences LongSleeveShirt $ tops capsule

springSummerOfficeTop :: CapsuleWardrobe -> Top
springSummerOfficeTop capsule
  | numOfShortSleeveBlouse < numOfTops / 2 = ShortSleeveBlouse 
  | numOfShortSleeveShirt < numOfTops / 4 = ShortSleeveShirt 
  | otherwise = TankTop
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops capsule
    numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops capsule

autumnWinterOfficeTop :: CapsuleWardrobe -> Top
autumnWinterOfficeTop capsule
  | numOfLongSleeveBlouse < numOfTops / (3/2) = LongSleeveBlouse 
  | otherwise = LongSleeveShirt
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfLongSleeveBlouse = fromIntegral . countOccurrences LongSleeveBlouse $ tops capsule

-- Logic for Dress
springSummerCasualDress :: CapsuleWardrobe -> Dress
springSummerCasualDress capsule
  | numOfShortSleeveDress < numOfTops / 2 = ShortSleeveDress 
  | otherwise = NoSleeveDress
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses capsule

autumnWinterCasualDress :: CapsuleWardrobe -> Dress
autumnWinterCasualDress capsule
  | numOfLongSleeveDress < numOfTops / 2 = LongSleeveDress 
  | otherwise = NoSleeveDress
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses capsule

springSummerOfficeDress :: CapsuleWardrobe -> Dress
springSummerOfficeDress capsule
  | numOfShortSleeveDress < numOfTops / 2 = ShortSleeveDress 
  | otherwise = NoSleeveDress
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses capsule

autumnWinterOfficeDress :: CapsuleWardrobe -> Dress
autumnWinterOfficeDress capsule
  | numOfLongSleeveDress < numOfTops / 2 = LongSleeveDress 
  | otherwise = NoSleeveDress
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses capsule

-- Logic for Overalls
springSummerCasualOverall :: CapsuleWardrobe -> Overall
springSummerCasualOverall capsule
  | numOfCardigan < numOfOveralls / 6 = Cardigan 
  | numOfSweatshirt < numOfOveralls / 6 = Sweatshirt 
  | numOfBlazer < numOfOveralls / 6 = Blazer 
  | numOfTrenchCoat < numOfOveralls / 6 = TrenchCoat 
  | numOfJacket < numOfOveralls / 6 = Jacket 
  | otherwise = Vest
  where
    numOfOveralls = fromIntegral . length . overalls $ capsule
    numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls capsule
    numOfSweatshirt = fromIntegral . countOccurrences Sweatshirt $ overalls capsule
    numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls capsule
    numOfTrenchCoat = fromIntegral . countOccurrences TrenchCoat $ overalls capsule
    numOfJacket = fromIntegral . countOccurrences Jacket $ overalls capsule

autumnWinterCasualOverall :: CapsuleWardrobe -> Overall
autumnWinterCasualOverall capsule
  | numOfCardigan < numOfOveralls / 8 = Cardigan 
  | numOfJacket < numOfOveralls / 8 = Jacket 
  | numOfSweatshirt < numOfOveralls / 8 = Sweatshirt 
  | numOfBlazer < numOfOveralls / 8 = Blazer 
  | numOfVest < numOfOveralls / 8 = Vest 
  | numOfWoolCoat < numOfOveralls / 8 = WoolCoat 
  | numOfSweater < numOfOveralls / 8 = Sweater 
  | otherwise = TrenchCoat
  where
    numOfOveralls = fromIntegral . length . overalls $ capsule
    numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls capsule
    numOfJacket = fromIntegral . countOccurrences Jacket $ overalls capsule
    numOfSweatshirt = fromIntegral . countOccurrences Sweatshirt $ overalls capsule
    numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls capsule
    numOfVest = fromIntegral . countOccurrences Vest $ overalls capsule
    numOfWoolCoat = fromIntegral . countOccurrences WoolCoat $ overalls capsule
    numOfSweater = fromIntegral . countOccurrences Sweater $ overalls capsule

springSummerOfficeOverall :: CapsuleWardrobe -> Overall
springSummerOfficeOverall capsule
  | numOfTrenchCoat < numOfOveralls / 4 = TrenchCoat 
  | numOfVest < numOfOveralls / 4 = Vest 
  | numOfCardigan < numOfOveralls / 4 = Cardigan 
  | otherwise = Blazer
  where
    numOfOveralls = fromIntegral . length . overalls $ capsule
    numOfTrenchCoat = fromIntegral . countOccurrences TrenchCoat $ overalls capsule
    numOfVest = fromIntegral . countOccurrences Vest $ overalls capsule
    numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls capsule

autumnWinterOfficeOverall :: CapsuleWardrobe -> Overall
autumnWinterOfficeOverall capsule
  | numOfSweater < numOfOveralls / 6 = Sweater 
  | numOfBlazer < numOfOveralls / 6 = Blazer 
  | numOfVest < numOfOveralls / 6 = Vest 
  | numOfWoolCoat < numOfOveralls / 6 = WoolCoat 
  | numOfCardigan < numOfOveralls / 6 = Cardigan 
  | otherwise = TrenchCoat
  where
    numOfOveralls = fromIntegral . length . overalls $ capsule
    numOfSweater = fromIntegral . countOccurrences Sweater $ overalls capsule
    numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls capsule
    numOfVest = fromIntegral . countOccurrences Vest $ overalls capsule
    numOfWoolCoat = fromIntegral . countOccurrences WoolCoat $ overalls capsule
    numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls capsule

-- Logic for Skirt
springSummerCasualSkirt :: CapsuleWardrobe -> Skirt
springSummerCasualSkirt capsule
  | numOfShortSkirt < numOfSkirts / 2 = ShortSkirt 
  | otherwise = LongSkirt
  where
    numOfSkirts = fromIntegral . length . skirts $ capsule
    numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts capsule

autumnWinterCasualSkirt :: CapsuleWardrobe -> Skirt
autumnWinterCasualSkirt capsule
  | numOfShortSkirt < numOfSkirts / 2 = ShortSkirt 
  | otherwise = LongSkirt
  where
    numOfSkirts = fromIntegral . length . skirts $ capsule
    numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts capsule

springSummerOfficeSkirt :: CapsuleWardrobe -> Skirt
springSummerOfficeSkirt capsule
  | numOfShortSkirt < numOfSkirts / 2 = ShortSkirt 
  | otherwise = LongSkirt
  where
    numOfSkirts = fromIntegral . length . skirts $ capsule
    numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts capsule

autumnWinterOfficeSkirt :: CapsuleWardrobe -> Skirt
autumnWinterOfficeSkirt capsule
  | numOfShortSkirt < numOfSkirts / 2 = ShortSkirt 
  | otherwise = LongSkirt
  where
    numOfSkirts = fromIntegral . length . skirts $ capsule
    numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts capsule

-- Logic for Pants
springSummerCasualPants :: CapsuleWardrobe -> Pants
springSummerCasualPants capsule
  | numOfJeansShorts < numOfPants / 3 = JeansShorts 
  | numOfLeggings < numOfPants / 3 = Leggings
  | otherwise = Jeans
  where
    numOfPants = fromIntegral . length . pants $ capsule
    numOfJeansShorts = fromIntegral . countOccurrences JeansShorts $ pants capsule
    numOfLeggings = fromIntegral . countOccurrences Leggings $ pants capsule

autumnWinterCasualPants :: CapsuleWardrobe -> Pants
autumnWinterCasualPants capsule
  | numOfLeggings < numOfPants / 2 = Leggings
  | otherwise = Jeans
  where
    numOfPants = fromIntegral . length . pants $ capsule
    numOfLeggings = fromIntegral . countOccurrences Leggings $ pants capsule

springSummerOfficePants :: CapsuleWardrobe -> Pants
springSummerOfficePants capsule
  | numOfSocialShorts < numOfPants / 2 = SocialShorts 
  | otherwise = DressTrousers
  where
    numOfPants = fromIntegral . length . pants $ capsule
    numOfSocialShorts = fromIntegral . countOccurrences SocialShorts $ pants capsule

autumnWinterOfficePants :: CapsuleWardrobe -> Pants
autumnWinterOfficePants capsule = DressTrousers
