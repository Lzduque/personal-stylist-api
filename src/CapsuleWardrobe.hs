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
  , preferences     :: [Preferences]
  , wardrobe        :: Wardrobe
  } deriving (Show, Generic, Eq, ToJSON)

instance FromJSON CapsuleWardrobe where 
  parseJSON = withObject "capsuleWardrobe" $ \cw -> do
    season          <- cw .: "season"
    style           <- cw .: "style"
    numberOfOutfits <- cw .: "numberOfOutfits"
    colors          <- cw .: "colors"
    preferences     <- cw .: "preferences"
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

data NumberOfOutfits = From10to20 | From21to30 | From31to40 | From41to50 | From51to60 | From61to70 | From71to80 | From81to90 | From91to100 | From101to110 | From111to120 | From121to130
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Colors = White | OffWhite | Beige | Brown | Black | Navy | Blue | LightBlue | DarkGreen | LightGreen | DarkYellow | LightYellow | DarkPink | LightPink | DarkRed | LightRed | DarkOrgange | LightOrange | DarkPurple | LightPurple
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Preferences = Skirts | Dresses | Pants | HighHeels
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
toRange From101to110 = (101,110)
toRange From111to120 = (111,120)
toRange From121to130 = (121,130)

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
    | topBottom > 2 && wantBottoms = addBottom capsule
    | topBottom < 3/2 && wantBottoms = addTop capsule
    | dressBottom < 1/3 && wantDresses = addDress capsule
    | topOverall >= 3 = addOverall capsule
    | topOverall < 2 && wantBottoms = addBottom capsule
    | dressTop <= 1/6 && wantDresses = addDress capsule
    | dressOverall >= 2 && wantDresses && not wantBottoms = addOverall capsule
    | wantDresses && not wantBottoms = addDress capsule
    | otherwise = addTop capsule
    where 
      numBottoms = fromIntegral $ (length . pants $ wardrobe capsule) + (length . skirts $ wardrobe capsule)
      topBottom = fromIntegral (length . tops $ wardrobe capsule) / numBottoms
      dressBottom = fromIntegral (length . dresses $ wardrobe capsule) / numBottoms
      dressOverall = fromIntegral (length . dresses $ wardrobe capsule) / fromIntegral (length . overalls $ wardrobe capsule)
      topOverall = fromIntegral (length . tops $ wardrobe capsule) / fromIntegral (length . overalls $ wardrobe capsule)
      dressTop = fromIntegral (length . dresses $ wardrobe capsule) / fromIntegral (length . tops $ wardrobe capsule)
      clothesPreferences = preferences capsule
      wantBottoms = Pants `elem` clothesPreferences || Skirts `elem` clothesPreferences
      wantDresses = Dresses `elem` clothesPreferences


addTop :: CapsuleWardrobe -> CapsuleWardrobe
addTop capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newTop = case (season, style) of
      (SpringSummer, Casual) -> if
        | numOfShortSleeveShirt <= numOfTops / 3 -> ShortSleeveShirt 
        | numOfShortSleeveBlouse <= numOfTops / 3 -> ShortSleeveBlouse 
        | otherwise -> TankTop
      (AutumnWinter, Casual) -> if
        | numOfLongSleeveShirt <= numOfTops / 2 -> LongSleeveShirt 
        | otherwise -> LongSleeveBlouse
      (SpringSummer, Office) -> if
        | numOfShortSleeveBlouse <= numOfTops / 2 -> ShortSleeveBlouse 
        | numOfShortSleeveShirt <= numOfTops / 4 -> ShortSleeveShirt 
        | otherwise -> TankTop
      (AutumnWinter, Office) -> if
        | numOfLongSleeveBlouse <= numOfTops / (3/2) -> LongSleeveBlouse 
        | otherwise -> LongSleeveShirt
  in capsule {wardrobe = addToWardrobe newTop wardrobe}
    where
      numOfTops = fromIntegral . length . tops $ wardrobe
      numOfShortSleeveShirt = fromIntegral . countOccurrences ShortSleeveShirt $ tops wardrobe
      numOfShortSleeveBlouse = fromIntegral . countOccurrences ShortSleeveBlouse $ tops wardrobe
      numOfLongSleeveShirt = fromIntegral . countOccurrences LongSleeveShirt $ tops wardrobe
      numOfLongSleeveBlouse = fromIntegral . countOccurrences LongSleeveBlouse $ tops wardrobe

addDress :: CapsuleWardrobe -> CapsuleWardrobe
addDress capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newDress = case (season, style) of
      (SpringSummer, Casual) -> if
        | numOfShortSleeveDress <= numOfDresses / 2 -> ShortSleeveDress 
        | otherwise -> NoSleeveDress
      (AutumnWinter, Casual) -> if
        | numOfLongSleeveDress <= numOfDresses / 3 -> LongSleeveDress 
        | numOfShortSleeveDress <= numOfDresses / 3 -> ShortSleeveDress 
        | otherwise -> NoSleeveDress
      (SpringSummer, Office) -> if
        | numOfShortSleeveDress <= numOfDresses / 2 -> ShortSleeveDress 
        | otherwise -> NoSleeveDress
      (AutumnWinter, Office) -> if
        | numOfLongSleeveDress <= numOfDresses / 3 -> LongSleeveDress 
        | numOfShortSleeveDress <= numOfDresses / 3 -> ShortSleeveDress 
        | otherwise -> NoSleeveDress
  in capsule {wardrobe = addToWardrobe newDress wardrobe}
    where
      numOfDresses = fromIntegral . length . dresses $ wardrobe
      numOfShortSleeveDress = fromIntegral . countOccurrences ShortSleeveDress $ dresses wardrobe
      numOfLongSleeveDress = fromIntegral . countOccurrences LongSleeveDress $ dresses wardrobe

addOverall :: CapsuleWardrobe -> CapsuleWardrobe
addOverall capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newOverall = case (season, style) of
      (SpringSummer, Casual) -> if
        | numOfJacket <= numOfOveralls / 6 -> Jacket 
        | numOfVest <= numOfOveralls / 6 -> Vest 
        | numOfCardigan <= numOfOveralls / 6 -> Cardigan 
        | numOfSweatshirt <= numOfOveralls / 6 -> Sweatshirt 
        | numOfBlazer <= numOfOveralls / 6 -> Blazer 
        | otherwise -> TrenchCoat
      (AutumnWinter, Casual) -> if
        | numOfSweater <= numOfOveralls / 8 -> Sweater 
        | numOfTrenchCoat <= numOfOveralls / 8 -> TrenchCoat 
        | numOfCardigan <= numOfOveralls / 8 -> Cardigan 
        | numOfJacket <= numOfOveralls / 8 -> Jacket 
        | numOfSweatshirt <= numOfOveralls / 8 -> Sweatshirt 
        | numOfBlazer <= numOfOveralls / 8 -> Blazer 
        | numOfVest <= numOfOveralls / 8 -> Vest 
        | otherwise -> WoolCoat
      (SpringSummer, Office) -> if
        | numOfCardigan <= numOfOveralls / 4 -> Cardigan 
        | numOfBlazer <= numOfOveralls / 4 -> Blazer 
        | numOfTrenchCoat <= numOfOveralls / 4 -> TrenchCoat 
        | otherwise -> Vest
      (AutumnWinter, Office) -> if
        | numOfCardigan <= numOfOveralls / 6 -> Cardigan 
        | numOfTrenchCoat <= numOfOveralls / 6 -> TrenchCoat 
        | numOfSweater <= numOfOveralls / 6 -> Sweater 
        | numOfBlazer <= numOfOveralls / 6 -> Blazer 
        | numOfVest <= numOfOveralls / 6 -> Vest 
        | otherwise -> WoolCoat
  in capsule {wardrobe = addToWardrobe newOverall wardrobe}
    where
      numOfOveralls = fromIntegral . length . overalls $ wardrobe
      numOfCardigan = fromIntegral . countOccurrences Cardigan $ overalls wardrobe
      numOfSweatshirt = fromIntegral . countOccurrences Sweatshirt $ overalls wardrobe
      numOfBlazer = fromIntegral . countOccurrences Blazer $ overalls wardrobe
      numOfTrenchCoat = fromIntegral . countOccurrences TrenchCoat $ overalls wardrobe
      numOfJacket = fromIntegral . countOccurrences Jacket $ overalls wardrobe
      numOfVest = fromIntegral . countOccurrences Vest $ overalls wardrobe
      numOfWoolCoat = fromIntegral . countOccurrences WoolCoat $ overalls wardrobe
      numOfSweater = fromIntegral . countOccurrences Sweater $ overalls wardrobe

addBottom :: CapsuleWardrobe -> CapsuleWardrobe
addBottom capsule
  | wantsSkirts && not wantsPants = addSkirt capsule
  | wantsPants && not wantsSkirts = addPants capsule
  | pantSkirt > 3 && wantsSkirts = addSkirt capsule
  | pantSkirt <= 3 && wantsPants = addPants capsule
  | otherwise = addPants capsule
  where
    numOfSkirts = fromIntegral (length . skirts $ wardrobe capsule)
    numOfPants = fromIntegral (length . pants $ wardrobe capsule)
    pantSkirt = numOfPants / numOfSkirts
    clothesPreferences = preferences capsule
    wantsPants = Pants `elem` clothesPreferences
    wantsSkirts = Skirts `elem` clothesPreferences

addSkirt :: CapsuleWardrobe -> CapsuleWardrobe
addSkirt capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newSkirt = case (season, style) of
      (SpringSummer, Casual) -> if
        | numOfShortSkirt <= numOfSkirts / 2 -> ShortSkirt 
        | otherwise -> LongSkirt
      (AutumnWinter, Casual) -> if
        | numOfShortSkirt <= numOfSkirts / 2 -> ShortSkirt 
        | otherwise -> LongSkirt
      (SpringSummer, Office) -> if
        | numOfShortSkirt <= numOfSkirts / 2 -> ShortSkirt 
        | otherwise -> LongSkirt
      (AutumnWinter, Office) -> if
        | numOfShortSkirt <= numOfSkirts / 2 -> ShortSkirt 
        | otherwise -> LongSkirt
  in capsule {wardrobe = addToWardrobe newSkirt wardrobe}
    where
      numOfSkirts = fromIntegral . length . skirts $ wardrobe
      numOfShortSkirt = fromIntegral . countOccurrences ShortSkirt $ skirts wardrobe

addPants :: CapsuleWardrobe -> CapsuleWardrobe
addPants capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newPants = case (season, style) of
      (SpringSummer, Casual) -> if
        | numOfJeans <= numOfPants / 3 -> Jeans
        | numOfJeansShorts <= numOfPants / 3 -> JeansShorts 
        | otherwise -> Leggings
      (AutumnWinter, Casual) -> if
        | numOfJeans <= numOfPants / 2 -> Jeans
        | otherwise -> Leggings
      (SpringSummer, Office) -> if
        | numOfDressTrousers <= numOfPants / 2 -> DressTrousers 
        | otherwise -> SocialShorts
      (AutumnWinter, Office) -> DressTrousers
  in capsule {wardrobe = addToWardrobe newPants wardrobe}
    where
      numOfPants = fromIntegral . length . pants $ wardrobe
      numOfJeansShorts = fromIntegral . countOccurrences JeansShorts $ pants wardrobe
      numOfJeans = fromIntegral . countOccurrences Jeans $ pants wardrobe
      numOfDressTrousers = fromIntegral . countOccurrences DressTrousers $ pants wardrobe

addAccessories :: CapsuleWardrobe -> CapsuleWardrobe
addAccessories capsule
  | shoesNumOfOutfits <= 1/25 = addShoes capsule
  | purseNumOfOutfits <= 1/35 = addPurse capsule
  | otherwise = capsule
  where
    numOfOutfits = fromIntegral . countOutfits $ wardrobe capsule
    shoesNumOfOutfits = fromIntegral (length . shoes $ wardrobe capsule) /  numOfOutfits
    purseNumOfOutfits = fromIntegral (length . purses $ wardrobe capsule) / numOfOutfits

addShoes :: CapsuleWardrobe -> CapsuleWardrobe
addShoes capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newShoes = case (season, style) of
      (SpringSummer, Casual) -> if
        | numOfSandals <= numOfShoes / 5 -> Sandals
        | numOfFlats <= numOfShoes / 5 -> Flats 
        | numOfSneakers <= numOfShoes / 5 -> Sneakers 
        | numOfWedges <= numOfShoes / 5 -> Wedges 
        | otherwise -> Loafers
      (AutumnWinter, Casual) -> if
        | numOfBoots <= numOfShoes / 5 -> Boots 
        | numOfFlats <= numOfShoes / 5 -> Flats 
        | numOfSneakers <= numOfShoes / 5 -> Sneakers 
        | numOfAnkleBoots <= numOfShoes / 5 -> AnkleBoots 
        | otherwise -> Loafers
      (SpringSummer, Office) -> if
        | numOfSandals <= numOfShoes / 4 -> Sandals
        | numOfFlats <= numOfShoes / 4 -> Flats 
        | numOfHeels <= numOfShoes / 4 && wantsHeels -> Heels 
        | otherwise -> Loafers
      (AutumnWinter, Office) -> if
        | numOfBoots <= numOfShoes / 4 -> Boots 
        | numOfHeels <= numOfShoes / 4 && wantsHeels -> Heels 
        | numOfFlats <= numOfShoes / 4 -> Flats 
        | otherwise -> AnkleBoots
  in capsule {wardrobe = addToWardrobe newShoes wardrobe}
    where
      numOfShoes = fromIntegral . length . shoes $ wardrobe
      numOfSneakers = fromIntegral . countOccurrences Sneakers $ shoes wardrobe
      numOfWedges = fromIntegral . countOccurrences Wedges $ shoes wardrobe
      numOfSandals = fromIntegral . countOccurrences Sandals $ shoes wardrobe
      numOfFlats = fromIntegral . countOccurrences Flats $ shoes wardrobe
      numOfAnkleBoots = fromIntegral . countOccurrences AnkleBoots $ shoes wardrobe
      numOfBoots = fromIntegral . countOccurrences Boots $ shoes wardrobe
      numOfHeels = fromIntegral . countOccurrences Heels $ shoes wardrobe
      clothesPreferences = preferences capsule
      wantsHeels = HighHeels `elem` clothesPreferences

addPurse :: CapsuleWardrobe -> CapsuleWardrobe
addPurse capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newPurse = case (season, style) of
      (SpringSummer, Casual) -> RelaxedBag
      (AutumnWinter, Casual) -> RelaxedBag
      (SpringSummer, Office) -> StructuredBag
      (AutumnWinter, Office) -> StructuredBag
  in capsule {wardrobe = addToWardrobe newPurse wardrobe}
