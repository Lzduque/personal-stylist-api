module CapsuleWardrobe where

import Data.Aeson (FromJSON,ToJSON,parseJSON,withObject,(.:),(.:?),(.!=))
import GHC.Generics (Generic)
import Data.List (null, sort, group, union)
import Prelude hiding (error)
import Lib

data Error =
  Error 
    { error   :: Bool
    , message :: String
    } deriving (Show, Generic, Eq, ToJSON, FromJSON)
  
data CapsuleWardrobe =
  CapsuleWardrobe
  { season          :: Season
  , style           :: Style
  , numberOfOutfits :: NumberOfOutfits
  , colors          :: ColorsTypes         
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
            , coats = []    
            , shoes = []       
            , purses = []      
            }

data Wardrobe =
  Wardrobe
  { tops        :: [Top]
  , pants       :: [Pants]
  , skirts      :: [Skirt]
  , dresses     :: [Dress]
  , coats       :: [Coat]
  , shoes       :: [Shoes]
  , purses      :: [Purse]
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data ColorsTypes =
  ColorsTypes
  { mains     :: [Colors]
  , neutrals  :: [Colors]
  , accents   :: [Colors]
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Season = SpringSummer | AutumnWinter
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Style = Casual | Office
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NumberOfOutfits = From10to20 | From21to30 | From31to40 | From41to50 | From51to60 | From61to70 | From71to80 | From81to90 | From91to100 | From101to110 | From111to120 | From121to130 | From131to140 | From141to150 | From151to160 | From161to170 | From171to180 | From181to190 | From191to200 | From201to210 | From211to220 | From221to230 | From231to240
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Colors = White | OffWhite | Beige | Camel | Brown | Gray | Black | Navy | Blue | LightBlue | DarkGreen | Green | LightGreen | DarkYellow | Yellow | LightYellow | DarkPink | Pink | LightPink | DarkRed | Red | Coral | DarkOrange | Orange | LightOrange | DarkPurple | Purple | LightPurple
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Preferences = Skirts | Dresses | Pants | LeggingsPants | ShortsPants
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

class Clothing a where
  addToWardrobe :: a -> Wardrobe -> Wardrobe
  takeColors :: [a] -> [b] -> [b]

data Top = ShirtTop | TShirtTankTop
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Top where
  addToWardrobe newTop wardrobe = wardrobe { tops = tops wardrobe ++ [newTop] }
  takeColors cs = take (length cs)
  
data Pants = JeansPants | DressPants | Shorts | SuitShorts | Leggings
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Pants where
  addToWardrobe newPants wardrobe = wardrobe { pants = pants wardrobe ++ [newPants] }
  takeColors cs = take (length cs)

data Skirt = BusinessSkirt | DaySkirt
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Skirt where
  addToWardrobe newSkirt wardrobe = wardrobe { skirts = skirts wardrobe ++ [newSkirt] }
  takeColors cs = take (length cs)

data Dress = BusinessDress | DayDress
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Dress where
  addToWardrobe newDress wardrobe = wardrobe { dresses = dresses wardrobe ++ [newDress] }
  takeColors cs = take (length cs)

-- to do: think about transforming cardigans in layer2 and coats in layer3, so you can do a better count of the number of outfits (abstraction to layers)
data Coat = Sweater | Jacket | Blazer
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Coat where
  addToWardrobe newCoat wardrobe = wardrobe { coats = coats wardrobe ++ [newCoat] }
  takeColors cs = take (length cs)

data Shoes = Shoes
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Shoes where
  addToWardrobe newShoes wardrobe = wardrobe { shoes = shoes wardrobe ++ [newShoes] }
  takeColors cs = take (length cs)

data Purse = RelaxedBag | StructuredBag
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Purse where
  addToWardrobe newPurse wardrobe = wardrobe { purses = purses wardrobe ++ [newPurse] }
  takeColors cs = take (length cs)



-- ------------ FUNCTIONS
countOutfits :: Wardrobe -> Int
countOutfits wardrobe = (numOfTops * numOfPants * numOfCoats) + (numOfTops * numOfSkirts * numOfCoats) + (numOfDresses * numOfCoats) where
    numOfTops = length . tops $ wardrobe
    numOfPants = length . pants $ wardrobe
    numOfSkirts = length . skirts $ wardrobe
    numOfDresses = length . dresses $ wardrobe
    numOfCoats = length . coats $ wardrobe

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
toRange From131to140 = (131,140)
toRange From141to150 = (141,150)
toRange From151to160 = (151,160)
toRange From161to170 = (161,170)
toRange From171to180 = (171,180)
toRange From181to190 = (181,190)
toRange From191to200 = (191,200)
toRange From201to210 = (201,210)
toRange From211to220 = (211,220)
toRange From221to230 = (221,230)
toRange From231to240 = (231,240)

sortWardrobe :: Wardrobe -> Wardrobe
sortWardrobe wardrobe = 
  wardrobe 
  { tops = sort . tops $ wardrobe
  , pants = sort . pants $ wardrobe
  , skirts = sort . skirts $ wardrobe
  , dresses = sort . dresses $ wardrobe
  , coats = sort . coats $ wardrobe
  , shoes = sort . shoes $ wardrobe
  , purses = sort . purses $ wardrobe
  }

fillUpWardrobe :: CapsuleWardrobe -> Either Error CapsuleWardrobe
fillUpWardrobe capsule
    | totalOutfits `inRange` rangeOfOutfits = Right $ capsule { wardrobe = sortWardrobe $ wardrobe capsule }
    | totalOutfits > snd rangeOfOutfits = Left Error { error = True, message = "No capsule can be generated within this range, for these parameters. Please, change the number of outfits." }
    | null (preferences capsule) && null allColors = Left Error { error = True, message = "Please, select at least one preference and one color."}
    | null $ preferences capsule = Left Error { error = True, message = "Please, select at least one preference."}
    | null allColors = Left Error { error = True, message = "Please, select at least one color."}
    | Skirts `notElem` preferences capsule && Pants `notElem` preferences capsule && Dresses `notElem` preferences capsule = Left Error { error = True, message = "Please, select at least one of these preferences: Skirts, Dresses or Pants."}
    | otherwise = fillUpWardrobe newCapsule
    where
      totalOutfits = countOutfits . wardrobe $ capsule
      rangeOfOutfits = toRange . numberOfOutfits $ capsule
      newCapsule = addAccessories $ addMoreClothes capsule
      allColors = ((mains $ colors capsule) `union` (neutrals $ colors capsule)) `union` (accents $ colors capsule)

groupByClothing :: CapsuleWardrobe -> [(String,Int,[Colors])]
groupByClothing capsule = concat [ftops, fpants, fskirts, fdresses, fcoats, fshoes, fpurses]
  where
    f :: (Clothing a, Eq a, Show a) => [Colors] -> [a] -> [(String,Int,[Colors])]
    f colors = map (\cs -> (show . head $ cs, length cs, takeColors cs colors)) . group
    ftops = f allColors (tops . wardrobe $ capsule)
    fpants = f allColors . pants . wardrobe $ capsule
    fskirts = f allColors . skirts . wardrobe $ capsule
    fdresses = f dressesColors . dresses . wardrobe $ capsule
    fcoats = f allColors . coats . wardrobe $ capsule
    fshoes = f accessoriesColors . shoes . wardrobe $ capsule
    fpurses = f accessoriesColors . purses . wardrobe $ capsule
    allColors = ((mains $ colors capsule) `union` (neutrals $ colors capsule)) `union` (accents $ colors capsule)
    accessoriesColors = ((neutrals $ colors capsule) `union` (accents $ colors capsule)) `union` (mains $ colors capsule)
    dressesColors = ((accents $ colors capsule) `union` (neutrals $ colors capsule)) `union` (mains $ colors capsule)

addMoreClothes :: CapsuleWardrobe -> CapsuleWardrobe
addMoreClothes capsule
    | topBottom > 2 && wantBottoms = addBottom capsule
    | topBottom < 3/2 && wantBottoms = addTop capsule
    | dressBottom < 1/3 && wantDresses = addDress capsule
    | topCoat >= 3 = addCoat capsule
    | topCoat < 2 && wantBottoms = addBottom capsule
    | dressTop <= 1/6 && wantDresses = addDress capsule
    | dressCoat >= 2 && wantDresses && not wantBottoms = addCoat capsule
    | wantDresses && not wantBottoms = addDress capsule
    | otherwise = addTop capsule
    where 
      numBottoms = fromIntegral $ (length . pants $ wardrobe capsule) + (length . skirts $ wardrobe capsule)
      topBottom = fromIntegral (length . tops $ wardrobe capsule) / numBottoms
      dressBottom = fromIntegral (length . dresses $ wardrobe capsule) / numBottoms
      dressCoat = fromIntegral (length . dresses $ wardrobe capsule) / fromIntegral (length . coats $ wardrobe capsule)
      topCoat = fromIntegral (length . tops $ wardrobe capsule) / fromIntegral (length . coats $ wardrobe capsule)
      dressTop = fromIntegral (length . dresses $ wardrobe capsule) / fromIntegral (length . tops $ wardrobe capsule)
      clothesPreferences = preferences capsule
      wantBottoms = Pants `elem` clothesPreferences || Skirts `elem` clothesPreferences
      wantDresses = Dresses `elem` clothesPreferences


addTop :: CapsuleWardrobe -> CapsuleWardrobe
addTop capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newTop = case (season, style) of
      (SpringSummer, Casual) -> if
        | numOfShirtTop <= numOfTops / 2 -> ShirtTop 
        | otherwise -> TShirtTankTop
      (AutumnWinter, Casual) -> if
        | numOfShirtTop <= numOfTops / 2 -> ShirtTop 
        | otherwise -> TShirtTankTop
      (SpringSummer, Office) -> ShirtTop 
      (AutumnWinter, Office) -> ShirtTop
  in capsule {wardrobe = addToWardrobe newTop wardrobe}
    where
      numOfTops = fromIntegral . length . tops $ wardrobe
      numOfShirtTop = fromIntegral . countOccurrences ShirtTop $ tops wardrobe

addDress :: CapsuleWardrobe -> CapsuleWardrobe
addDress capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newDress = case (season, style) of
      (SpringSummer, Casual) -> DayDress
      (AutumnWinter, Casual) -> DayDress
      (SpringSummer, Office) -> BusinessDress
      (AutumnWinter, Office) -> BusinessDress
  in capsule {wardrobe = addToWardrobe newDress wardrobe}

addCoat :: CapsuleWardrobe -> CapsuleWardrobe
addCoat capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newCoat = case (season, style) of
      (SpringSummer, Casual) -> if 
        | numOfSweater <= numOfCoats / 2 -> Sweater 
        | otherwise -> Jacket
      (AutumnWinter, Casual) -> if
        | numOfSweater <= numOfCoats / 2 -> Sweater 
        | otherwise -> Jacket
      (SpringSummer, Office) -> if
        | numOfSweater <= numOfCoats / 2 -> Sweater 
        | otherwise -> Blazer
      (AutumnWinter, Office) -> if
        | numOfSweater <= numOfCoats / 2 -> Sweater 
        | otherwise -> Blazer
  in capsule {wardrobe = addToWardrobe newCoat wardrobe}
    where
      numOfCoats = fromIntegral . length . coats $ wardrobe
      numOfSweater = fromIntegral . countOccurrences Sweater $ coats wardrobe

addBottom :: CapsuleWardrobe -> CapsuleWardrobe
addBottom capsule
  | wantsSkirts && not wantsPants = addSkirt capsule
  | wantsPants && not wantsSkirts = addPants capsule
  | pantSkirt > 2 && wantsSkirts = addSkirt capsule
  | pantSkirt <= 2 && wantsPants = addPants capsule
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
      (SpringSummer, Casual) -> DaySkirt
      (AutumnWinter, Casual) -> DaySkirt
      (SpringSummer, Office) -> BusinessSkirt
      (AutumnWinter, Office) -> BusinessSkirt
  in capsule {wardrobe = addToWardrobe newSkirt wardrobe}

addPants :: CapsuleWardrobe -> CapsuleWardrobe
addPants capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newPants = case (season, style) of
      (SpringSummer, Casual) -> if
        | numOfJeansPants <= numOfPants / 3 -> JeansPants
        | numOfLeggings <= numOfPants / 3 && wantsLeggings -> Leggings 
        | numOfShorts <= numOfPants / 3 && wantsShorts -> Shorts 
        | otherwise -> JeansPants
      (AutumnWinter, Casual) -> if
        | numOfJeansPants <= numOfPants / 2 -> JeansPants
        | numOfLeggings <= numOfPants / 2 && wantsLeggings -> Leggings
        | otherwise -> JeansPants
      (SpringSummer, Office) -> if
        | numOfDressPants <= numOfPants / 2 -> DressPants 
        | numOfSuitShorts <= numOfPants / 2 && wantsShorts -> SuitShorts 
        | otherwise -> DressPants
      (AutumnWinter, Office) -> DressPants
  in capsule {wardrobe = addToWardrobe newPants wardrobe}
    where
      numOfPants = fromIntegral . length . pants $ wardrobe
      numOfLeggings = fromIntegral . countOccurrences Leggings $ pants wardrobe
      numOfShorts = fromIntegral . countOccurrences Shorts $ pants wardrobe
      numOfJeansPants = fromIntegral . countOccurrences JeansPants $ pants wardrobe
      numOfDressPants = fromIntegral . countOccurrences DressPants $ pants wardrobe
      numOfSuitShorts = fromIntegral . countOccurrences SuitShorts $ pants wardrobe
      clothesPreferences = preferences capsule
      wantsLeggings = LeggingsPants `elem` clothesPreferences
      wantsShorts = ShortsPants `elem` clothesPreferences

addAccessories :: CapsuleWardrobe -> CapsuleWardrobe
addAccessories capsule
  | shoesNumOfOutfits <= 1/40 = addShoes capsule
  | purseNumOfOutfits <= 1/40 = addPurse capsule
  | otherwise = capsule
  where
    numOfOutfits = fromIntegral . countOutfits $ wardrobe capsule
    shoesNumOfOutfits = fromIntegral (length . shoes $ wardrobe capsule) /  numOfOutfits
    purseNumOfOutfits = fromIntegral (length . purses $ wardrobe capsule) / numOfOutfits

addShoes :: CapsuleWardrobe -> CapsuleWardrobe
addShoes capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newShoes = case (season, style) of
      (SpringSummer, Casual) -> Shoes
      (AutumnWinter, Casual) -> Shoes
      (SpringSummer, Office) -> Shoes
      (AutumnWinter, Office) -> Shoes
  in capsule {wardrobe = addToWardrobe newShoes wardrobe}

addPurse :: CapsuleWardrobe -> CapsuleWardrobe
addPurse capsule@(CapsuleWardrobe {season, style, wardrobe}) =
  let 
    newPurse = case (season, style) of
      (SpringSummer, Casual) -> RelaxedBag
      (AutumnWinter, Casual) -> RelaxedBag
      (SpringSummer, Office) -> StructuredBag
      (AutumnWinter, Office) -> StructuredBag
  in capsule {wardrobe = addToWardrobe newPurse wardrobe}
