module CapsuleWardrobe
  (CapsuleWardrobe(..)
  , Tops(..)
  , Bottoms(..)
  , Overalls(..)
  , Shoes(..)
  , Purses(..)
  , autumnWinterCasualCW
  , springSummerCasualCW
  , autumnWinterOfficeCW
  , springSummerOfficeCW
  ) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)

data CapsuleWardrobe =
  CapsuleWardrobe { tops        :: [Tops]
                  , bottoms     :: [Bottoms]
                  , overalls    :: [Overalls]
                  , shoes       :: [Shoes]
                  , purses      :: [Purses]
                  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Tops = ShortSleeveShirt | LongSleeveShirt | TankTop | ShortSleeveBlouse | LongSleeveBlouse | Cardigan
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Bottoms = SkinnyJeans | StraightJeans | CroppedJeans | Capris | JeansShorts | Skirt | StraightPants
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Overalls = Dress | TrenchCoat | WinterCoat
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Shoes = Sandals | Bailarinas | Heels | Boots
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Purses = Tote | Backpack | Clutch
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


autumnWinterCasualCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse, Cardigan]
                  , bottoms = [SkinnyJeans, Skirt]
                  , overalls = [Dress, TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
                  , purses = [Backpack]
                  }

springSummerCasualCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse, Cardigan]
                  , bottoms = [SkinnyJeans, Skirt]
                  , overalls = [Dress, TrenchCoat]
                  , shoes = [Sandals, Bailarinas]
                  , purses = [Tote]
                  }

autumnWinterOfficeCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse, Cardigan]
                  , bottoms = [StraightPants, Skirt]
                  , overalls = [Dress, TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
                  , purses = [Backpack]
                  }

springSummerOfficeCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse, Cardigan]
                  , bottoms = [StraightPants, Skirt]
                  , overalls = [Dress, TrenchCoat]
                  , shoes = [Sandals, Bailarinas]
                  , purses = [Tote]
                  }

