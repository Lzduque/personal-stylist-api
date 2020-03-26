module CapsuleWardrobe
  (CapsuleWardrobe(..)
  , Top(..)
  , Bottom(..)
  , Dress(..)
  , Overall(..)
  , Shoe(..)
  , Purse(..)
  , autumnWinterCasualCW
  , springSummerCasualCW
  , autumnWinterOfficeCW
  , springSummerOfficeCW
  ) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)

data CapsuleWardrobe =
  CapsuleWardrobe { tops        :: [Top]
                  , bottoms     :: [Bottom]
                  , dresses     :: [Dress]
                  , overalls    :: [Overall]
                  , shoes       :: [Shoe]
                  , purses      :: [Purse]
                  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Top = ShortSleeveShirt | LongSleeveShirt | TankTop | ShortSleeveBlouse | LongSleeveBlouse | Cardigan
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Bottom = SkinnyJeans | StraightJeans | CroppedJeans | Capris | JeansShorts | Skirt | StraightPants
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Dress = Dress
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Overall = TrenchCoat | WinterCoat
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Shoe = Sandals | Bailarinas | Heels | Boots
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Purse = Tote | Backpack | Clutch
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


autumnWinterCasualCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, Cardigan]
                  , bottoms = [SkinnyJeans, Skirt]
                  , dresses = [Dress]
                  , overalls = [TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
                  , purses = [Backpack]
                  }

springSummerCasualCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse, Cardigan]
                  , bottoms = [SkinnyJeans, Skirt]
                  , dresses = [Dress]
                  , overalls = [TrenchCoat]
                  , shoes = [Sandals, Bailarinas]
                  , purses = [Tote]
                  }

autumnWinterOfficeCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, Cardigan]
                  , bottoms = [StraightPants, Skirt]
                  , dresses = [Dress]
                  , overalls = [TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
                  , purses = [Backpack]
                  }

springSummerOfficeCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse, Cardigan]
                  , bottoms = [StraightPants, Skirt]
                  , dresses = [Dress]
                  , overalls = [TrenchCoat]
                  , shoes = [Sandals, Bailarinas]
                  , purses = [Tote]
                  }

