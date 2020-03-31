module CapsuleWardrobe where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)
import Data.List

data CapsuleWardrobe =
  CapsuleWardrobe { tops        :: [Top]
                  , pants       :: [Pants]
                  , skirts      :: [Skirt]
                  , dresses     :: [Dress]
                  , overalls    :: [Overall]
                  , shoes       :: [Shoes]
                  , purses      :: [Purse]
                  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

class Clothing a where
  addToCapsule :: a -> CapsuleWardrobe -> CapsuleWardrobe
  takeColors :: [a] -> [b] -> [b]

data Top = LongSleeveShirt | LongSleeveBlouse | ShortSleeveShirt | ShortSleeveBlouse | TankTop
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Top where
  addToCapsule newTop capsule = capsule { tops = tops capsule ++ [newTop] }
  takeColors cs = take (length cs)
  
data Pants = Jeans | JeansShorts | DressTrousers | SocialShorts | Leggings
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Pants where
  addToCapsule newPants capsule = capsule { pants = pants capsule ++ [newPants] }
  takeColors cs = take (length cs)

data Skirt = LongSkirt | ShortSkirt
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Skirt where
  addToCapsule newSkirt capsule = capsule { skirts = skirts capsule ++ [newSkirt] }
  takeColors cs = take (length cs)

data Dress = LongSleeveDress | ShortSleeveDress | NoSleeveDress
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Dress where
  addToCapsule newDress capsule = capsule { dresses = dresses capsule ++ [newDress] }
  takeColors cs = reverse . take (length cs) . reverse

-- to do: think about transforming cardigans in layer2 and coats in layer3, so you can do a better count of the number of outfits (abstraction to layers)
-- to do: WinterCoat??
data Overall = Sweater | Cardigan | Jacket | Vest | Blazer | Sweatshirt | TrenchCoat | WoolCoat
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Overall where
  addToCapsule newOverall capsule = capsule { overalls = overalls capsule ++ [newOverall] }
  takeColors cs = take (length cs)

data Shoes = Sandals | Flats | Heels | AnkleBoots | Boots | Loafers | Sneakers | Wedges
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Shoes where
  addToCapsule newShoes capsule = capsule { shoes = shoes capsule ++ [newShoes] }
  takeColors cs = take (length cs)

data Purse = RelaxedBag | StructuredBag
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Purse where
  addToCapsule newPurse capsule = capsule { purses = purses capsule ++ [newPurse] }
  takeColors cs = reverse . take (length cs) . reverse



-- ------------ FUNCTIONS
sortCapsule :: CapsuleWardrobe -> CapsuleWardrobe
sortCapsule capsule = 
  capsule 
  { tops = sort . tops $ capsule
  , pants = sort . pants $ capsule
  , skirts = sort . skirts $ capsule
  , dresses = sort . dresses $ capsule
  , overalls = sort . overalls $ capsule
  , shoes = sort . shoes $ capsule
  , purses = sort . purses $ capsule
  }



-- base Capsule Wardrobes
autumnWinterCasualCW :: CapsuleWardrobe
autumnWinterCasualCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Sweater, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [RelaxedBag]
                  }

springSummerCasualCW :: CapsuleWardrobe
springSummerCasualCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Vest, Jacket]
                  , shoes = [Sandals, Flats]
                  , purses = [RelaxedBag]
                  }

autumnWinterOfficeCW :: CapsuleWardrobe
autumnWinterOfficeCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse]
                  , pants = [DressTrousers, DressTrousers]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [StructuredBag]
                  }

springSummerOfficeCW :: CapsuleWardrobe
springSummerOfficeCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse]
                  , pants = [DressTrousers, DressTrousers]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, Blazer]
                  , shoes = [Sandals, Flats]
                  , purses = [StructuredBag]
                  }

