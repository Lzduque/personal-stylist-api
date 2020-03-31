module Lib where

import CapsuleWardrobe
import qualified Data.ByteString.Lazy as B
import Data.List

-- ------------ HELPERS
-- get the json and transform in byte string
getJSON :: FilePath -> IO B.ByteString
getJSON filePath = B.readFile filePath

setUpBaseWardrobe :: CapsuleWardrobe -> CapsuleWardrobe
setUpBaseWardrobe capsule@(CapsuleWardrobe {season = AutumnWinter, style = Casual}) = capsule { wardrobe = autumnWinterCasual}
setUpBaseWardrobe capsule@(CapsuleWardrobe {season = SpringSummer, style = Casual}) = capsule { wardrobe = springSummerCasual}
setUpBaseWardrobe capsule@(CapsuleWardrobe {season = AutumnWinter, style = Office}) = capsule { wardrobe = autumnWinterOffice}
setUpBaseWardrobe capsule@(CapsuleWardrobe {season = SpringSummer, style = Office}) = capsule { wardrobe = springSummerOffice}

countOutfits :: Wardrobe -> Int
countOutfits wardrobe = (numOfTops * numOfPants * numOfOveralls) + (numOfTops * numOfSkirts * numOfOveralls) + (numOfDresses * numOfOveralls) where
    numOfTops = length . tops $ wardrobe
    numOfPants = length . pants $ wardrobe
    numOfSkirts = length . skirts $ wardrobe
    numOfDresses = length . dresses $ wardrobe
    numOfOveralls = length . overalls $ wardrobe

toRange :: NumberOfOutfits -> (Int,Int)
toRange From10to20 = (10,20)
toRange From21to30 = (21,30)
toRange From31to40 = (31,40)
toRange From41to50 = (41,50)
toRange From51to60 = (51,60)
toRange From61to70 = (61,70)
toRange From71to80 = (71,80)
toRange From81to90 = (81,90)
toRange From91to100 = (91,100)

inRange :: Int -> (Int,Int) -> Bool
inRange x (a,b)
  | a <= x && x <= b = True
  | otherwise = False

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (x==)



-- ------------ MAIN FUNCTIONS
fillUpWardrobe :: CapsuleWardrobe -> CapsuleWardrobe
fillUpWardrobe capsule
    | totalOutfits `inRange` rangeOfOutfits = capsule { wardrobe = sortWardrobe $ wardrobe capsule }
    | totalOutfits > snd rangeOfOutfits = error "Total Outfits larger than the Range of Outfits wished"
    | otherwise = fillUpWardrobe newCapsule
    where
      totalOutfits = countOutfits . wardrobe $ capsule
      rangeOfOutfits = toRange . numberOfOutfits $ capsule
      newCapsule = addAccessories $ addMoreClothes capsule

groupByClothing :: CapsuleWardrobe -> [(String,Int,[Colors])]
groupByClothing capsule = concat [ftops, fpants, fskirts, fdresses, foveralls, fshoes, fpurses]
  where
    f :: (Clothing a, Eq a, Show a) => [a] -> [(String,Int,[Colors])]
    f = map (\cs -> (show . head $ cs, length cs, takeColors cs (colors capsule))) . group
    ftops = f . tops . wardrobe $ capsule
    fpants = f . pants . wardrobe $ capsule
    fskirts = f . skirts . wardrobe $ capsule
    fdresses = f . dresses . wardrobe $ capsule
    foveralls = f . overalls . wardrobe $ capsule
    fshoes = f . shoes . wardrobe $ capsule
    fpurses = f . purses . wardrobe $ capsule

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
      numBottoms = fromIntegral $ (length . pants $ wardrobe capsule) + (length . skirts $ wardrobe capsule)
      topBottom = fromIntegral (length . tops $ wardrobe capsule) / numBottoms
      dressBottom = fromIntegral (length . dresses $ wardrobe capsule) / numBottoms
      topOverall = fromIntegral (length . tops $ wardrobe capsule) / fromIntegral (length . overalls $ wardrobe capsule)
      dressTop = fromIntegral (length . dresses $ wardrobe capsule) / fromIntegral (length . tops $ wardrobe capsule)



-- ------------ Add Clothes functions
-- addClothing :: CapsuleWardrobe -> CapsuleWardrobe
-- addClothing Top (CapsuleWardrobe {season = SpringSummer, style = Casual, wardrobe}) = addToWardrobe newTop wardrobe
--   where
--     newTop
--       | numOfShortSleeveShirt <= numOfTops / 3 = ShortSleeveShirt
--       | numOfShortSleeveBlouse <= numOfTops / 3 = ShortSleeveBlouse
--       | otherwise = TankTop
--     numOfTops = fromIntegral . length . tops $ wardrobe
--     numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops wardrobe
--     numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops wardrobe

addTop :: CapsuleWardrobe -> CapsuleWardrobe
addTop capsule@(CapsuleWardrobe {season = SpringSummer, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newTop wardrobe}
  where 
    newTop
      | numOfShortSleeveShirt <= numOfTops / 3 = ShortSleeveShirt 
      | numOfShortSleeveBlouse <= numOfTops / 3 = ShortSleeveBlouse 
      | otherwise = TankTop
    numOfTops = fromIntegral . length . tops $ wardrobe
    numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops wardrobe
    numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops wardrobe
addTop capsule@(CapsuleWardrobe {season = AutumnWinter, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newTop wardrobe}
  where 
    newTop
      | numOfLongSleeveShirt <= numOfTops / 2 = LongSleeveShirt 
      | otherwise = LongSleeveBlouse
    numOfTops = fromIntegral . length . tops $ wardrobe
    numOfLongSleeveShirt = fromIntegral . countOccurrences LongSleeveShirt $ tops wardrobe
addTop capsule@(CapsuleWardrobe {season = SpringSummer, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newTop wardrobe}
  where 
    newTop
      | numOfShortSleeveBlouse <= numOfTops / 2 = ShortSleeveBlouse 
      | numOfShortSleeveShirt <= numOfTops / 4 = ShortSleeveShirt 
      | otherwise = TankTop
    numOfTops = fromIntegral . length . tops $ wardrobe
    numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops wardrobe
    numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops wardrobe
addTop capsule@(CapsuleWardrobe {season = AutumnWinter, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newTop wardrobe}
  where 
    newTop
      | numOfLongSleeveBlouse <= numOfTops / (3/2) = LongSleeveBlouse 
      | otherwise = LongSleeveShirt
    numOfTops = fromIntegral . length . tops $ wardrobe
    numOfLongSleeveBlouse = fromIntegral . countOccurrences LongSleeveBlouse $ tops wardrobe


-- addTop capsule
--   | wishedSeason == SpringSummer && wishedStyle == Casual = addToWardrobe (springSummerCasualTop capsule) $ wardrobe capsule
--   | wishedSeason == AutumnWinter && wishedStyle == Casual = addToWardrobe (autumnWinterCasualTop capsule) $ wardrobe capsule
--   | wishedSeason == SpringSummer && wishedStyle == Office = addToWardrobe (springSummerOfficeTop capsule) $ wardrobe capsule
--   | wishedSeason == AutumnWinter && wishedStyle == Office = addToWardrobe (autumnWinterOfficeTop capsule) $ wardrobe capsule
--   | otherwise = error "Wrong capsule selected - Top"
--   where
--     wishedSeason = season $ capsule
--     wishedStyle = style $ capsule

addDress :: CapsuleWardrobe -> CapsuleWardrobe
addDress capsule@(CapsuleWardrobe {season = SpringSummer, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newDress wardrobe}
  where 
    newDress
      | numOfShortSleeveDress <= numOfTops / 2 = ShortSleeveDress 
      | otherwise = NoSleeveDress
    numOfTops = fromIntegral . length . tops $ wardrobe
    numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses wardrobe

addDress capsule@(CapsuleWardrobe {season = AutumnWinter, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newDress wardrobe}
  where 
    newDress
      | numOfLongSleeveDress <= numOfTops / 2 = LongSleeveDress 
      | otherwise = NoSleeveDress
    numOfTops = fromIntegral . length . tops $ wardrobe
    numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses wardrobe

addDress capsule@(CapsuleWardrobe {season = SpringSummer, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newDress wardrobe}
  where 
    newDress
      | numOfShortSleeveDress <= numOfTops / 2 = ShortSleeveDress 
      | otherwise = NoSleeveDress
    numOfTops = fromIntegral . length . tops $ wardrobe
    numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses wardrobe

addDress capsule@(CapsuleWardrobe {season = AutumnWinter, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newDress wardrobe}
  where 
    newDress
      | numOfLongSleeveDress <= numOfTops / 2 = LongSleeveDress 
      | otherwise = NoSleeveDress
    numOfTops = fromIntegral . length . tops $ wardrobe
    numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses wardrobe

addOverall :: CapsuleWardrobe -> CapsuleWardrobe
addOverall capsule@(CapsuleWardrobe {season = SpringSummer, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newOverall wardrobe}
  where 
    newOverall
      | numOfCardigan <= numOfOveralls / 6 = Cardigan 
      | numOfSweatshirt <= numOfOveralls / 6 = Sweatshirt 
      | numOfBlazer <= numOfOveralls / 6 = Blazer 
      | numOfTrenchCoat <= numOfOveralls / 6 = TrenchCoat 
      | numOfJacket <= numOfOveralls / 6 = Jacket 
      | otherwise = Vest
    numOfOveralls = fromIntegral . length . overalls $ wardrobe
    numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls wardrobe
    numOfSweatshirt = fromIntegral . countOccurrences Sweatshirt $ overalls wardrobe
    numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls wardrobe
    numOfTrenchCoat = fromIntegral . countOccurrences TrenchCoat $ overalls wardrobe
    numOfJacket = fromIntegral . countOccurrences Jacket $ overalls wardrobe

addOverall capsule@(CapsuleWardrobe {season = AutumnWinter, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newOverall wardrobe}
  where 
    newOverall
      | numOfCardigan <= numOfOveralls / 8 = Cardigan 
      | numOfJacket <= numOfOveralls / 8 = Jacket 
      | numOfSweatshirt <= numOfOveralls / 8 = Sweatshirt 
      | numOfBlazer <= numOfOveralls / 8 = Blazer 
      | numOfVest <= numOfOveralls / 8 = Vest 
      | numOfWoolCoat <= numOfOveralls / 8 = WoolCoat 
      | numOfSweater <= numOfOveralls / 8 = Sweater 
      | otherwise = TrenchCoat
    numOfOveralls = fromIntegral . length . overalls $ wardrobe
    numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls wardrobe
    numOfJacket = fromIntegral . countOccurrences Jacket $ overalls wardrobe
    numOfSweatshirt = fromIntegral . countOccurrences Sweatshirt $ overalls wardrobe
    numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls wardrobe
    numOfVest = fromIntegral . countOccurrences Vest $ overalls wardrobe
    numOfWoolCoat = fromIntegral . countOccurrences WoolCoat $ overalls wardrobe
    numOfSweater = fromIntegral . countOccurrences Sweater $ overalls wardrobe

addOverall capsule@(CapsuleWardrobe {season = SpringSummer, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newOverall wardrobe}
  where 
    newOverall
      | numOfTrenchCoat <= numOfOveralls / 4 = TrenchCoat 
      | numOfVest <= numOfOveralls / 4 = Vest 
      | numOfCardigan <= numOfOveralls / 4 = Cardigan 
      | otherwise = Blazer
    numOfOveralls = fromIntegral . length . overalls $ wardrobe
    numOfTrenchCoat = fromIntegral . countOccurrences TrenchCoat $ overalls wardrobe
    numOfVest = fromIntegral . countOccurrences Vest $ overalls wardrobe
    numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls wardrobe

addOverall capsule@(CapsuleWardrobe {season = AutumnWinter, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newOverall wardrobe}
  where 
    newOverall
      | numOfSweater <= numOfOveralls / 6 = Sweater 
      | numOfBlazer <= numOfOveralls / 6 = Blazer 
      | numOfVest <= numOfOveralls / 6 = Vest 
      | numOfWoolCoat <= numOfOveralls / 6 = WoolCoat 
      | numOfCardigan <= numOfOveralls / 6 = Cardigan 
      | otherwise = TrenchCoat
    numOfOveralls = fromIntegral . length . overalls $ wardrobe
    numOfSweater = fromIntegral . countOccurrences Sweater $ overalls wardrobe
    numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls wardrobe
    numOfVest = fromIntegral . countOccurrences Vest $ overalls wardrobe
    numOfWoolCoat = fromIntegral . countOccurrences WoolCoat $ overalls wardrobe
    numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls wardrobe

addBottom :: CapsuleWardrobe -> CapsuleWardrobe
addBottom capsule
  | pantSkirt > 3 = addSkirt capsule
  | otherwise = addPants capsule
  where
    pantSkirt = fromIntegral (length . pants $ wardrobe capsule) / fromIntegral (length . skirts $ wardrobe capsule)

addSkirt :: CapsuleWardrobe -> CapsuleWardrobe
addSkirt capsule@(CapsuleWardrobe {season = SpringSummer, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newSkirt wardrobe}
  where 
    newSkirt
      | numOfShortSkirt <= numOfSkirts / 2 = ShortSkirt 
      | otherwise = LongSkirt
    numOfSkirts = fromIntegral . length . skirts $ wardrobe
    numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts wardrobe

addSkirt capsule@(CapsuleWardrobe {season = AutumnWinter, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newSkirt wardrobe}
  where 
    newSkirt
      | numOfShortSkirt <= numOfSkirts / 2 = ShortSkirt 
      | otherwise = LongSkirt
    numOfSkirts = fromIntegral . length . skirts $ wardrobe
    numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts wardrobe

addSkirt capsule@(CapsuleWardrobe {season = SpringSummer, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newSkirt wardrobe}
  where 
    newSkirt
      | numOfShortSkirt <= numOfSkirts / 2 = ShortSkirt 
      | otherwise = LongSkirt
    numOfSkirts = fromIntegral . length . skirts $ wardrobe
    numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts wardrobe

addSkirt capsule@(CapsuleWardrobe {season = AutumnWinter, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newSkirt wardrobe}
  where 
    newSkirt
      | numOfShortSkirt <= numOfSkirts / 2 = ShortSkirt 
      | otherwise = LongSkirt
    numOfSkirts = fromIntegral . length . skirts $ wardrobe
    numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts wardrobe

addPants :: CapsuleWardrobe -> CapsuleWardrobe
addPants capsule@(CapsuleWardrobe {season = SpringSummer, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newPants wardrobe}
  where 
    newPants
      | numOfJeansShorts <= numOfPants / 3 = JeansShorts 
      | numOfLeggings <= numOfPants / 3 = Leggings
      | otherwise = Jeans
    numOfPants = fromIntegral . length . pants $ wardrobe
    numOfJeansShorts = fromIntegral . countOccurrences JeansShorts $ pants wardrobe
    numOfLeggings = fromIntegral . countOccurrences Leggings $ pants wardrobe

addPants capsule@(CapsuleWardrobe {season = AutumnWinter, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newPants wardrobe}
  where 
    newPants
      | numOfLeggings <= numOfPants / 2 = Leggings
      | otherwise = Jeans
    numOfPants = fromIntegral . length . pants $ wardrobe
    numOfLeggings = fromIntegral . countOccurrences Leggings $ pants wardrobe

addPants capsule@(CapsuleWardrobe {season = SpringSummer, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newPants wardrobe}
  where 
    newPants
      | numOfSocialShorts <= numOfPants / 2 = SocialShorts 
      | otherwise = DressTrousers
    numOfPants = fromIntegral . length . pants $ wardrobe
    numOfSocialShorts = fromIntegral . countOccurrences SocialShorts $ pants wardrobe

addPants capsule@(CapsuleWardrobe {season = AutumnWinter, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newPants wardrobe}
  where 
    newPants = DressTrousers

addAccessories :: CapsuleWardrobe -> CapsuleWardrobe
addAccessories capsule
  | shoesNumOfOutfits <= 1/20 = addShoes capsule
  | purseNumOfOutfits <= 1/30 = addPurse capsule
  | otherwise = capsule
  where
    numOfOutfits = fromIntegral . countOutfits $ wardrobe capsule
    shoesNumOfOutfits = fromIntegral (length . shoes $ wardrobe capsule) /  numOfOutfits
    purseNumOfOutfits = fromIntegral (length . purses $ wardrobe capsule) / numOfOutfits

addShoes :: CapsuleWardrobe -> CapsuleWardrobe
addShoes capsule@(CapsuleWardrobe {season = SpringSummer, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newShoes wardrobe}
  where 
    newShoes
      | numOfSneakers <= numOfShoes / 5 = Sneakers 
      | numOfWedges <= numOfShoes / 5 = Wedges 
      | numOfLoafers <= numOfShoes / 5 = Loafers 
      | numOfSandals <= numOfShoes / 5 = Sandals
      | otherwise = Flats
    numOfShoes = fromIntegral . length . shoes $ wardrobe
    numOfSneakers = fromIntegral . countOccurrences Sneakers $ shoes wardrobe
    numOfWedges = fromIntegral . countOccurrences Wedges $ shoes wardrobe
    numOfLoafers = fromIntegral . countOccurrences Loafers $ shoes wardrobe
    numOfSandals = fromIntegral . countOccurrences Sandals $ shoes wardrobe

addShoes capsule@(CapsuleWardrobe {season = AutumnWinter, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newShoes wardrobe}
  where 
    newShoes
      | numOfSneakers <= numOfShoes / 5 = Sneakers 
      | numOfAnkleBoots <= numOfShoes / 5 = AnkleBoots 
      | numOfLoafers <= numOfShoes / 5 = Loafers 
      | numOfBoots <= numOfShoes / 5 = Boots 
      | otherwise = Flats
    numOfShoes = fromIntegral . length . shoes $ wardrobe
    numOfSneakers = fromIntegral . countOccurrences Sneakers $ shoes wardrobe
    numOfAnkleBoots = fromIntegral . countOccurrences AnkleBoots $ shoes wardrobe
    numOfLoafers = fromIntegral . countOccurrences Loafers $ shoes wardrobe
    numOfBoots = fromIntegral . countOccurrences Boots $ shoes wardrobe

addShoes capsule@(CapsuleWardrobe {season = SpringSummer, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newShoes wardrobe}
  where 
    newShoes
      | numOfHeels <= numOfShoes / 4 = Heels 
      | numOfLoafers <= numOfShoes / 4 = Loafers 
      | numOfSandals <= numOfShoes / 4 = Sandals
      | otherwise = Flats
    numOfShoes = fromIntegral . length . shoes $ wardrobe
    numOfHeels = fromIntegral . countOccurrences Heels $ shoes wardrobe
    numOfLoafers = fromIntegral . countOccurrences Loafers $ shoes wardrobe
    numOfSandals = fromIntegral . countOccurrences Sandals $ shoes wardrobe

addShoes capsule@(CapsuleWardrobe {season = AutumnWinter, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newShoes wardrobe}
  where 
    newShoes
      | numOfHeels <= numOfShoes / 4 = Heels 
      | numOfAnkleBoots <= numOfShoes / 4 = AnkleBoots 
      | numOfBoots <= numOfShoes / 4 = Boots 
      | otherwise = Flats
    numOfShoes = fromIntegral . length . shoes $ wardrobe
    numOfHeels = fromIntegral . countOccurrences Heels $ shoes wardrobe
    numOfAnkleBoots = fromIntegral . countOccurrences AnkleBoots $ shoes wardrobe
    numOfBoots = fromIntegral . countOccurrences Boots $ shoes wardrobe

addPurse :: CapsuleWardrobe -> CapsuleWardrobe
addPurse capsule@(CapsuleWardrobe {season = SpringSummer, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newPurse wardrobe}
  where 
    newPurse = RelaxedBag

addPurse capsule@(CapsuleWardrobe {season = AutumnWinter, style = Casual, wardrobe}) = capsule {wardrobe = addToWardrobe newPurse wardrobe}
  where 
    newPurse = RelaxedBag

addPurse capsule@(CapsuleWardrobe {season = SpringSummer, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newPurse wardrobe}
  where 
    newPurse = StructuredBag

addPurse capsule@(CapsuleWardrobe {season = AutumnWinter, style = Office, wardrobe}) = capsule {wardrobe = addToWardrobe newPurse wardrobe}
  where 
    newPurse = StructuredBag



-- Logic for Tops
-- springSummerCasualTop :: Wardrobe -> Top
-- springSummerCasualTop wardrobe
--   | numOfShortSleeveShirt <= numOfTops / 3 = ShortSleeveShirt 
--   | numOfShortSleeveBlouse <= numOfTops / 3 = ShortSleeveBlouse 
--   | otherwise = TankTop
--   where
--     numOfTops = fromIntegral . length . tops $ wardrobe
--     numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops wardrobe
--     numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops wardrobe

-- autumnWinterCasualTop :: CapsuleWardrobe -> Top
-- autumnWinterCasualTop capsule
--   | numOfLongSleeveShirt <= numOfTops / 2 = LongSleeveShirt 
--   | otherwise = LongSleeveBlouse
--   where
--     numOfTops = fromIntegral . length . tops $ capsule
--     numOfLongSleeveShirt = fromIntegral . countOccurrences LongSleeveShirt $ tops capsule

-- springSummerOfficeTop :: CapsuleWardrobe -> Top
-- springSummerOfficeTop capsule
--   | numOfShortSleeveBlouse <= numOfTops / 2 = ShortSleeveBlouse 
--   | numOfShortSleeveShirt <= numOfTops / 4 = ShortSleeveShirt 
--   | otherwise = TankTop
--   where
--     numOfTops = fromIntegral . length . tops $ capsule
--     numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops capsule
--     numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops capsule

-- autumnWinterOfficeTop :: CapsuleWardrobe -> Top
-- autumnWinterOfficeTop capsule
--   | numOfLongSleeveBlouse <= numOfTops / (3/2) = LongSleeveBlouse 
--   | otherwise = LongSleeveShirt
--   where
--     numOfTops = fromIntegral . length . tops $ capsule
--     numOfLongSleeveBlouse = fromIntegral . countOccurrences LongSleeveBlouse $ tops capsule

-- Logic for Dress
-- to do: think about the ratio, it is given 2 short sleeve dresses at first
-- springSummerCasualDress :: CapsuleWardrobe -> Dress
-- springSummerCasualDress capsule
--   | numOfShortSleeveDress <= numOfTops / 2 = ShortSleeveDress 
--   | otherwise = NoSleeveDress
--   where
--     numOfTops = fromIntegral . length . tops $ capsule
--     numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses capsule

-- autumnWinterCasualDress :: CapsuleWardrobe -> Dress
-- autumnWinterCasualDress capsule
--   | numOfLongSleeveDress <= numOfTops / 2 = LongSleeveDress 
--   | otherwise = NoSleeveDress
--   where
--     numOfTops = fromIntegral . length . tops $ capsule
--     numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses capsule

-- springSummerOfficeDress :: CapsuleWardrobe -> Dress
-- springSummerOfficeDress capsule
--   | numOfShortSleeveDress <= numOfTops / 2 = ShortSleeveDress 
--   | otherwise = NoSleeveDress
--   where
--     numOfTops = fromIntegral . length . tops $ capsule
--     numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses capsule

-- autumnWinterOfficeDress :: CapsuleWardrobe -> Dress
-- autumnWinterOfficeDress capsule
--   | numOfLongSleeveDress <= numOfTops / 2 = LongSleeveDress 
--   | otherwise = NoSleeveDress
--   where
--     numOfTops = fromIntegral . length . tops $ capsule
--     numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses capsule

-- Logic for Overalls
-- springSummerCasualOverall :: CapsuleWardrobe -> Overall
-- springSummerCasualOverall capsule
--   | numOfCardigan <= numOfOveralls / 6 = Cardigan 
--   | numOfSweatshirt <= numOfOveralls / 6 = Sweatshirt 
--   | numOfBlazer <= numOfOveralls / 6 = Blazer 
--   | numOfTrenchCoat <= numOfOveralls / 6 = TrenchCoat 
--   | numOfJacket <= numOfOveralls / 6 = Jacket 
--   | otherwise = Vest
--   where
--     numOfOveralls = fromIntegral . length . overalls $ capsule
--     numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls capsule
--     numOfSweatshirt = fromIntegral . countOccurrences Sweatshirt $ overalls capsule
--     numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls capsule
--     numOfTrenchCoat = fromIntegral . countOccurrences TrenchCoat $ overalls capsule
--     numOfJacket = fromIntegral . countOccurrences Jacket $ overalls capsule

-- autumnWinterCasualOverall :: CapsuleWardrobe -> Overall
-- autumnWinterCasualOverall capsule
--   | numOfCardigan <= numOfOveralls / 8 = Cardigan 
--   | numOfJacket <= numOfOveralls / 8 = Jacket 
--   | numOfSweatshirt <= numOfOveralls / 8 = Sweatshirt 
--   | numOfBlazer <= numOfOveralls / 8 = Blazer 
--   | numOfVest <= numOfOveralls / 8 = Vest 
--   | numOfWoolCoat <= numOfOveralls / 8 = WoolCoat 
--   | numOfSweater <= numOfOveralls / 8 = Sweater 
--   | otherwise = TrenchCoat
--   where
--     numOfOveralls = fromIntegral . length . overalls $ capsule
--     numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls capsule
--     numOfJacket = fromIntegral . countOccurrences Jacket $ overalls capsule
--     numOfSweatshirt = fromIntegral . countOccurrences Sweatshirt $ overalls capsule
--     numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls capsule
--     numOfVest = fromIntegral . countOccurrences Vest $ overalls capsule
--     numOfWoolCoat = fromIntegral . countOccurrences WoolCoat $ overalls capsule
--     numOfSweater = fromIntegral . countOccurrences Sweater $ overalls capsule

-- springSummerOfficeOverall :: CapsuleWardrobe -> Overall
-- springSummerOfficeOverall capsule
--   | numOfTrenchCoat <= numOfOveralls / 4 = TrenchCoat 
--   | numOfVest <= numOfOveralls / 4 = Vest 
--   | numOfCardigan <= numOfOveralls / 4 = Cardigan 
--   | otherwise = Blazer
--   where
--     numOfOveralls = fromIntegral . length . overalls $ capsule
--     numOfTrenchCoat = fromIntegral . countOccurrences TrenchCoat $ overalls capsule
--     numOfVest = fromIntegral . countOccurrences Vest $ overalls capsule
--     numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls capsule

-- autumnWinterOfficeOverall :: CapsuleWardrobe -> Overall
-- autumnWinterOfficeOverall capsule
--   | numOfSweater <= numOfOveralls / 6 = Sweater 
--   | numOfBlazer <= numOfOveralls / 6 = Blazer 
--   | numOfVest <= numOfOveralls / 6 = Vest 
--   | numOfWoolCoat <= numOfOveralls / 6 = WoolCoat 
--   | numOfCardigan <= numOfOveralls / 6 = Cardigan 
--   | otherwise = TrenchCoat
--   where
--     numOfOveralls = fromIntegral . length . overalls $ capsule
--     numOfSweater = fromIntegral . countOccurrences Sweater $ overalls capsule
--     numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls capsule
--     numOfVest = fromIntegral . countOccurrences Vest $ overalls capsule
--     numOfWoolCoat = fromIntegral . countOccurrences WoolCoat $ overalls capsule
--     numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls capsule

-- Logic for Skirt
-- springSummerCasualSkirt :: CapsuleWardrobe -> Skirt
-- springSummerCasualSkirt capsule
--   | numOfShortSkirt <= numOfSkirts / 2 = ShortSkirt 
--   | otherwise = LongSkirt
--   where
--     numOfSkirts = fromIntegral . length . skirts $ capsule
--     numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts capsule

-- autumnWinterCasualSkirt :: CapsuleWardrobe -> Skirt
-- autumnWinterCasualSkirt capsule
--   | numOfShortSkirt <= numOfSkirts / 2 = ShortSkirt 
--   | otherwise = LongSkirt
--   where
--     numOfSkirts = fromIntegral . length . skirts $ capsule
--     numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts capsule

-- springSummerOfficeSkirt :: CapsuleWardrobe -> Skirt
-- springSummerOfficeSkirt capsule
--   | numOfShortSkirt <= numOfSkirts / 2 = ShortSkirt 
--   | otherwise = LongSkirt
--   where
--     numOfSkirts = fromIntegral . length . skirts $ capsule
--     numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts capsule

-- autumnWinterOfficeSkirt :: CapsuleWardrobe -> Skirt
-- autumnWinterOfficeSkirt capsule
--   | numOfShortSkirt <= numOfSkirts / 2 = ShortSkirt 
--   | otherwise = LongSkirt
--   where
--     numOfSkirts = fromIntegral . length . skirts $ capsule
--     numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts capsule

-- Logic for Pants
-- springSummerCasualPants :: CapsuleWardrobe -> Pants
-- springSummerCasualPants capsule
--   | numOfJeansShorts <= numOfPants / 3 = JeansShorts 
--   | numOfLeggings <= numOfPants / 3 = Leggings
--   | otherwise = Jeans
--   where
--     numOfPants = fromIntegral . length . pants $ capsule
--     numOfJeansShorts = fromIntegral . countOccurrences JeansShorts $ pants capsule
--     numOfLeggings = fromIntegral . countOccurrences Leggings $ pants capsule

-- autumnWinterCasualPants :: CapsuleWardrobe -> Pants
-- autumnWinterCasualPants capsule
--   | numOfLeggings <= numOfPants / 2 = Leggings
--   | otherwise = Jeans
--   where
--     numOfPants = fromIntegral . length . pants $ capsule
--     numOfLeggings = fromIntegral . countOccurrences Leggings $ pants capsule

-- springSummerOfficePants :: CapsuleWardrobe -> Pants
-- springSummerOfficePants capsule
--   | numOfSocialShorts <= numOfPants / 2 = SocialShorts 
--   | otherwise = DressTrousers
--   where
--     numOfPants = fromIntegral . length . pants $ capsule
--     numOfSocialShorts = fromIntegral . countOccurrences SocialShorts $ pants capsule

-- autumnWinterOfficePants :: CapsuleWardrobe -> Pants
-- autumnWinterOfficePants capsule = DressTrousers

-- Logic for Shoes
-- springSummerCasualShoes :: CapsuleWardrobe -> Shoes
-- springSummerCasualShoes capsule
--   | numOfSneakers <= numOfShoes / 5 = Sneakers 
--   | numOfWedges <= numOfShoes / 5 = Wedges 
--   | numOfLoafers <= numOfShoes / 5 = Loafers 
--   | numOfSandals <= numOfShoes / 5 = Sandals
--   | otherwise = Flats
--   where
--     numOfShoes = fromIntegral . length . shoes $ capsule
--     numOfSneakers = fromIntegral . countOccurrences Sneakers $ shoes capsule
--     numOfWedges = fromIntegral . countOccurrences Wedges $ shoes capsule
--     numOfLoafers = fromIntegral . countOccurrences Loafers $ shoes capsule
--     numOfSandals = fromIntegral . countOccurrences Sandals $ shoes capsule

-- autumnWinterCasualShoes :: CapsuleWardrobe -> Shoes
-- autumnWinterCasualShoes capsule
--   | numOfSneakers <= numOfShoes / 5 = Sneakers 
--   | numOfAnkleBoots <= numOfShoes / 5 = AnkleBoots 
--   | numOfLoafers <= numOfShoes / 5 = Loafers 
--   | numOfBoots <= numOfShoes / 5 = Boots 
--   | otherwise = Flats
--   where
--     numOfShoes = fromIntegral . length . shoes $ capsule
--     numOfSneakers = fromIntegral . countOccurrences Sneakers $ shoes capsule
--     numOfAnkleBoots = fromIntegral . countOccurrences AnkleBoots $ shoes capsule
--     numOfLoafers = fromIntegral . countOccurrences Loafers $ shoes capsule
--     numOfBoots = fromIntegral . countOccurrences Boots $ shoes capsule

-- springSummerOfficeShoes :: CapsuleWardrobe -> Shoes
-- springSummerOfficeShoes capsule
--   | numOfHeels <= numOfShoes / 4 = Heels 
--   | numOfLoafers <= numOfShoes / 4 = Loafers 
--   | numOfSandals <= numOfShoes / 4 = Sandals
--   | otherwise = Flats
--   where
--     numOfShoes = fromIntegral . length . shoes $ capsule
--     numOfHeels = fromIntegral . countOccurrences Heels $ shoes capsule
--     numOfLoafers = fromIntegral . countOccurrences Loafers $ shoes capsule
--     numOfSandals = fromIntegral . countOccurrences Sandals $ shoes capsule

-- autumnWinterOfficeShoes :: CapsuleWardrobe -> Shoes
-- autumnWinterOfficeShoes capsule
--   | numOfHeels <= numOfShoes / 4 = Heels 
--   | numOfAnkleBoots <= numOfShoes / 4 = AnkleBoots 
--   | numOfBoots <= numOfShoes / 4 = Boots 
--   | otherwise = Flats
--   where
--     numOfShoes = fromIntegral . length . shoes $ capsule
--     numOfHeels = fromIntegral . countOccurrences Heels $ shoes capsule
--     numOfAnkleBoots = fromIntegral . countOccurrences AnkleBoots $ shoes capsule
--     numOfBoots = fromIntegral . countOccurrences Boots $ shoes capsule

-- Logic for Purse
springSummerCasualPurse :: Purse
springSummerCasualPurse = RelaxedBag 

autumnWinterCasualPurse :: Purse
autumnWinterCasualPurse = RelaxedBag 

springSummerOfficePurse :: Purse
springSummerOfficePurse = StructuredBag 

autumnWinterOfficePurse :: Purse
autumnWinterOfficePurse = StructuredBag 

