module CapsuleWardrobe where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)

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

data Top = LongSleeveShirt | LongSleeveBlouse | ShortSleeveShirt | ShortSleeveBlouse | TankTop
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Top where
  addToCapsule newTop capsule = capsule { tops = tops capsule ++ [newTop] }
  
data Pants = Jeans | JeansShorts | DressTrousers | SocialShorts | Leggings
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Pants where
  addToCapsule newPants capsule = capsule { pants = pants capsule ++ [newPants] }

data Skirt = LongSkirt | ShortSkirt
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Skirt where
  addToCapsule newSkirt capsule = capsule { skirts = skirts capsule ++ [newSkirt] }

data Dress = LongSleeveDress | ShortSleeveDress | NoSleeveDress
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Dress where
  addToCapsule newDress capsule = capsule { dresses = dresses capsule ++ [newDress] }

-- to do: think about transforming cardigans in layer2 and coats in layer3, so you can do a better count of the number of outfits (abstraction to layers)
-- to do: WinterCoat??
data Overall = Sweater | Cardigan | Jacket | Vest | Blazer | Sweatshirt | TrenchCoat | WoolCoat
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Overall where
  addToCapsule newOverall capsule = capsule { overalls = overalls capsule ++ [newOverall] }

data Shoes = Sandals | Flats | Heels | AnkleBoots | Boots | Loafers | Sneakers | Wedges
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Shoes where
  addToCapsule newShoes capsule = capsule { shoes = shoes capsule ++ [newShoes] }

data Purse = RelaxedBag | StructuredBag
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Clothing Purse where
  addToCapsule newPurse capsule = capsule { purses = purses capsule ++ [newPurse] }

-- base Capsule Wardrobes
autumnWinterCasualCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Sweater, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [RelaxedBag]
                  }

springSummerCasualCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Vest, Jacket]
                  , shoes = [Sandals, Flats]
                  , purses = [RelaxedBag]
                  }

autumnWinterOfficeCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse]
                  , pants = [DressTrousers, DressTrousers]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [StructuredBag]
                  }

springSummerOfficeCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse]
                  , pants = [DressTrousers, DressTrousers]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, Blazer]
                  , shoes = [Sandals, Flats]
                  , purses = [StructuredBag]
                  }

