module CapsuleWardrobe where

import Data.Aeson (FromJSON,ToJSON,parseJSON,withObject,(.:),(.:?),(.!=))
import GHC.Generics (Generic)
import Data.List

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