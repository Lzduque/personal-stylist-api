module CapsuleWardrobe where

import Data.Aeson (FromJSON,ToJSON,parseJSON,withObject,(.:),(.:?),(.!=))
import GHC.Generics (Generic)
import Data.List

import Lib

data CapsuleWardrobe =
  CapsuleWardrobe
  { season          :: Season
  , style           :: Style
  , numberOfOutfits :: NumberOfOutfits
  , colors          :: [Colors]
  , wardrobe        :: Wardrobe
  } deriving (Show, Generic, Eq, ToJSON)

instance FromJSON CapsuleWardrobe where 
  parseJSON = withObject "capsuleWardrobe" $ \cw -> do
    season          <- cw .: "season"
    style           <- cw .: "style"
    numberOfOutfits <- cw .: "numberOfOutfits"
    colors          <- cw .: "colors"
    wardrobe        <- cw .:? "wardrobe" .!= defaultWardrobe
    return CapsuleWardrobe{..}
      where
        defaultWardrobe =
          Wardrobe
            { tops = []
            , pants = []
            , skirts = []      
            , dresses = []     
            , overalls = []    
            , shoes = []       
            , purses = []      
            }

data Wardrobe =
  Wardrobe
  { tops        :: [Top]
  , pants       :: [Pants]
  , skirts      :: [Skirt]
  , dresses     :: [Dress]
  , overalls    :: [Overall]
  , shoes       :: [Shoes]
  , purses      :: [Purse]
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Season = SpringSummer | AutumnWinter
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Style = Casual | Office
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NumberOfOutfits = From10to20 | From21to30 | From31to40 | From41to50 | From51to60 | From61to70 | From71to80 | From81to90 | From91to100
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Colors = White | OffWhite | Beige | Brown | Black | Navy | Blue | LightBlue | DarkGreen | LightGreen | DarkYellow | LightYellow | DarkPink | LightPink | DarkRed | LightRed | DarkOrgange | LightOrange | DarkPurple | LightPurple
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

class Clothing a where
  addToWardrobe :: a -> Wardrobe -> Wardrobe
  takeColors :: [a] -> [b] -> [b]

data Top = LongSleeveShirt | LongSleeveBlouse | ShortSleeveShirt | ShortSleeveBlouse | TankTop
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Top where
  addToWardrobe newTop wardrobe = wardrobe { tops = tops wardrobe ++ [newTop] }
  takeColors cs = take (length cs)
  
data Pants = Jeans | JeansShorts | DressTrousers | SocialShorts | Leggings
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Pants where
  addToWardrobe newPants wardrobe = wardrobe { pants = pants wardrobe ++ [newPants] }
  takeColors cs = take (length cs)

data Skirt = LongSkirt | ShortSkirt
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Skirt where
  addToWardrobe newSkirt wardrobe = wardrobe { skirts = skirts wardrobe ++ [newSkirt] }
  takeColors cs = take (length cs)

data Dress = LongSleeveDress | ShortSleeveDress | NoSleeveDress
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Dress where
  addToWardrobe newDress wardrobe = wardrobe { dresses = dresses wardrobe ++ [newDress] }
  takeColors cs = reverse . take (length cs) . reverse

-- to do: think about transforming cardigans in layer2 and coats in layer3, so you can do a better count of the number of outfits (abstraction to layers)
-- to do: WinterCoat??
data Overall = Sweater | Cardigan | Jacket | Vest | Blazer | Sweatshirt | TrenchCoat | WoolCoat
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Overall where
  addToWardrobe newOverall wardrobe = wardrobe { overalls = overalls wardrobe ++ [newOverall] }
  takeColors cs = take (length cs)

data Shoes = Sandals | Flats | Heels | AnkleBoots | Boots | Loafers | Sneakers | Wedges
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Shoes where
  addToWardrobe newShoes wardrobe = wardrobe { shoes = shoes wardrobe ++ [newShoes] }
  takeColors cs = take (length cs)

data Purse = RelaxedBag | StructuredBag
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Purse where
  addToWardrobe newPurse wardrobe = wardrobe { purses = purses wardrobe ++ [newPurse] }
  takeColors cs = reverse . take (length cs) . reverse



-- ------------ FUNCTIONS
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

sortWardrobe :: Wardrobe -> Wardrobe
sortWardrobe wardrobe = 
  wardrobe 
  { tops = sort . tops $ wardrobe
  , pants = sort . pants $ wardrobe
  , skirts = sort . skirts $ wardrobe
  , dresses = sort . dresses $ wardrobe
  , overalls = sort . overalls $ wardrobe
  , shoes = sort . shoes $ wardrobe
  , purses = sort . purses $ wardrobe
  }

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



-- base Capsule Wardrobes
autumnWinterCasual :: Wardrobe
autumnWinterCasual =
  Wardrobe 
    { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse]
    , pants = [Jeans, Jeans]
    , skirts = []
    , dresses = []
    , overalls = [Sweater, TrenchCoat]
    , shoes = [Boots, Flats]
    , purses = [RelaxedBag]
    }

springSummerCasual :: Wardrobe
springSummerCasual =
  Wardrobe 
    { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse]
    , pants = [Jeans, Jeans]
    , skirts = []
    , dresses = []
    , overalls = [Vest, Jacket]
    , shoes = [Sandals, Flats]
    , purses = [RelaxedBag]
    }

autumnWinterOffice :: Wardrobe
autumnWinterOffice =
  Wardrobe 
    { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse]
    , pants = [DressTrousers, DressTrousers]
    , skirts = []
    , dresses = []
    , overalls = [Cardigan, TrenchCoat]
    , shoes = [Boots, Flats]
    , purses = [StructuredBag]
    }

springSummerOffice :: Wardrobe
springSummerOffice =
  Wardrobe 
    { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse]
    , pants = [DressTrousers, DressTrousers]
    , skirts = []
    , dresses = []
    , overalls = [Cardigan, Blazer]
    , shoes = [Sandals, Flats]
    , purses = [StructuredBag]
    }


-- to do

-- data BodyType = Rectangle | Circle | InvertedTriangle | Hourglass | Triangle
--   deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Hourglass
--   General 
--     "Try to choose light fabrics that don't add too much volume."
--     "The best fabric for your body type is soft but firm and elastic, such as viscose, polyester, jersey, silk, chiffon, soft wool crepe, fine knits and fabrics with lycra."
--   Dresses
--     "Try acentuating your curves, drawing attention to your waist."
--     "The best dresses are wrap, bodycon, monocrome, with V-neckline, but never too tight."
--   Tops
--     ""