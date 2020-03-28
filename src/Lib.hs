module Lib
    ( getJSON
    , chooseCapsule
    , countOutfits
    ) where
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

makeCapsule :: Wish -> CapsuleWardrobe -> CapsuleWardrobe
makeCapsule wish capsule
    | totalOutfits `inRange` rangeOfOutfits = capsule
    | totalOutfits > snd rangeOfOutfits = error "Total Outfits larger than the Range of Outfits wished"
    | otherwise = makeCapsule wish newCapsule
    where
      totalOutfits = countOutfits capsule
      rangeOfOutfits = toRange . numberOfOutfits $ wish
      newCapsule = addMoreClothes capsule

addMoreClothes :: CapsuleWardrobe -> CapsuleWardrobe
addMoreClothes capsule
    | topBottom > 2 = addBottom capsule
    | topBottom < 3/2 = addTop capsule
    | dressBottom < 1/3 = addDress capsule
    | topOverall >= 3 = addOverall capsule
    | topOverall < 2 = addBottom capsule
    | dressTop  <= 1/6 = addDress capsule
    | otherwise = addTop capsule
    where 
      numBottoms = fromIntegral $ (length . pants $ capsule) + (length . skirts $ capsule)
      topBottom = fromIntegral (length . tops $ capsule) / numBottoms
      dressBottom = fromIntegral (length . dresses $ capsule) / numBottoms
      topOverall = fromIntegral (length . tops $ capsule) / fromIntegral (length . overalls $ capsule)
      dressTop = fromIntegral (length . dresses $ capsule) / fromIntegral (length . tops $ capsule)

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

inRange :: Int -> (Int,Int) -> Bool
inRange x (a,b)
  | a <= x && x <= b = True
  | otherwise = False

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (x==)

-- Add Clothes function
addTop :: CapsuleWardrobe -> CapsuleWardrobe
addTop capsule
  | capsule == springSummerCasualCW = addToCapsule (springSummerCasualTop capsule) capsule
  | capsule == autumnWinterCasualCW = addToCapsule (autumnWinterCasualTop capsule) capsule
  | capsule == springSummerOfficeCW = addToCapsule (springSummerOfficeTop capsule) capsule
  | capsule == autumnWinterOfficeCW = addToCapsule (autumnWinterOfficeTop capsule) capsule
  | otherwise = error "Wrong capsule selected"

addDress :: CapsuleWardrobe -> CapsuleWardrobe
addDress capsule
  | capsule == springSummerCasualCW = addToCapsule (springSummerCasualDress capsule) capsule
  | capsule == autumnWinterCasualCW = addToCapsule (autumnWinterCasualDress capsule) capsule
  | capsule == springSummerOfficeCW = addToCapsule (springSummerOfficeDress capsule) capsule
  | capsule == autumnWinterOfficeCW = addToCapsule (autumnWinterOfficeDress capsule) capsule
  | otherwise = error "Wrong capsule selected"

addOverall :: CapsuleWardrobe -> CapsuleWardrobe
addOverall = undefined

addBottom :: CapsuleWardrobe -> CapsuleWardrobe
addBottom capsule
  | pantSkirt > 3 = addSkirt capsule
  | otherwise = addPant capsule
  where
    pantSkirt = fromIntegral (length . pants $ capsule) / fromIntegral (length . skirts $ capsule)

addSkirt :: CapsuleWardrobe -> CapsuleWardrobe
addSkirt = undefined

addPant :: CapsuleWardrobe -> CapsuleWardrobe
addPant = undefined

-- Logic for Tops
springSummerCasualTop :: CapsuleWardrobe -> Top
springSummerCasualTop capsule
  | numOfShortSleeveShirt < numOfTops / 3 = ShortSleeveShirt 
  | numOfShortSleeveBlouse < numOfTops / 3 = ShortSleeveBlouse 
  | numOfTankTop < numOfTops / 3 = TankTop 
  | otherwise = ShortSleeveShirt
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops capsule
    numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops capsule
    numOfTankTop = fromIntegral . countOccurrences TankTop $ tops capsule

autumnWinterCasualTop :: CapsuleWardrobe -> Top
autumnWinterCasualTop capsule
  | numOfLongSleeveShirt < numOfTops / 2 = LongSleeveShirt 
  | numOfLongSleeveBlouse < numOfTops / 2 = LongSleeveBlouse 
  | otherwise = LongSleeveShirt
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfLongSleeveShirt = fromIntegral . countOccurrences LongSleeveShirt $ tops capsule
    numOfLongSleeveBlouse = fromIntegral . countOccurrences LongSleeveBlouse $ tops capsule

springSummerOfficeTop :: CapsuleWardrobe -> Top
springSummerOfficeTop capsule
  | numOfShortSleeveShirt < numOfTops / 4 = ShortSleeveShirt 
  | numOfShortSleeveBlouse < numOfTops / 2 = ShortSleeveBlouse 
  | numOfTankTop < numOfTops / 4 = TankTop 
  | otherwise = ShortSleeveShirt
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops capsule
    numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops capsule
    numOfTankTop = fromIntegral . countOccurrences TankTop $ tops capsule

autumnWinterOfficeTop :: CapsuleWardrobe -> Top
autumnWinterOfficeTop capsule
  | numOfLongSleeveShirt < numOfTops / 3 = LongSleeveShirt 
  | numOfLongSleeveBlouse < numOfTops / (3/2) = LongSleeveBlouse 
  | otherwise = LongSleeveShirt
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfLongSleeveShirt = fromIntegral . countOccurrences LongSleeveShirt $ tops capsule
    numOfLongSleeveBlouse = fromIntegral . countOccurrences LongSleeveBlouse $ tops capsule

-- Logic for Dress
springSummerCasualDress :: CapsuleWardrobe -> Dress
springSummerCasualDress capsule
  | numOfShortSleeveDress < numOfTops / 2 = ShortSleeveDress 
  | numOfNoSleeveDress < numOfTops / 2 = NoSleeveDress 
  | otherwise = ShortSleeveDress
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses capsule
    numOfNoSleeveDress = fromIntegral . countOccurrences NoSleeveDress $ dresses capsule

autumnWinterCasualDress :: CapsuleWardrobe -> Dress
autumnWinterCasualDress capsule
  | numOfLongSleeveDress < numOfTops / 2 = LongSleeveDress 
  | numOfNoSleeveDress < numOfTops / 2 = NoSleeveDress 
  | otherwise = LongSleeveDress
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses capsule
    numOfNoSleeveDress = fromIntegral . countOccurrences NoSleeveDress $ dresses capsule

springSummerOfficeDress :: CapsuleWardrobe -> Dress
springSummerOfficeDress capsule
  | numOfShortSleeveDress < numOfTops / 2 = ShortSleeveDress 
  | numOfNoSleeveDress < numOfTops / 2 = NoSleeveDress 
  | otherwise = ShortSleeveDress
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses capsule
    numOfNoSleeveDress = fromIntegral . countOccurrences NoSleeveDress $ dresses capsule

autumnWinterOfficeDress :: CapsuleWardrobe -> Dress
autumnWinterOfficeDress capsule
  | numOfLongSleeveDress < numOfTops / 2 = LongSleeveDress 
  | numOfNoSleeveDress < numOfTops / 2 = NoSleeveDress 
  | otherwise = LongSleeveDress
  where
    numOfTops = fromIntegral . length . tops $ capsule
    numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses capsule
    numOfNoSleeveDress = fromIntegral . countOccurrences NoSleeveDress $ dresses capsule

