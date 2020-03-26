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

data Top = ShortSleeveShirt | LongSleeveShirt | TankTop | ShortSleeveBlouse | LongSleeveBlouse
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Bottom = SkinnyJeans | StraightJeans | CroppedJeans | Capris | JeansShorts | Skirt | StraightPants
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Dress = Dress
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Overall = Cardigan | TrenchCoat | WinterCoat
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Shoe = Sandals | Bailarinas | Heels | Boots
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Purse = Tote | Backpack | Clutch
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


autumnWinterCasualCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse]
                  , bottoms = [SkinnyJeans, Skirt]
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
                  , purses = [Backpack]
                  }

springSummerCasualCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse]
                  , bottoms = [SkinnyJeans, Skirt]
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Sandals, Bailarinas]
                  , purses = [Tote]
                  }

autumnWinterOfficeCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse]
                  , bottoms = [StraightPants, Skirt]
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
                  , purses = [Backpack]
                  }

springSummerOfficeCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse]
                  , bottoms = [StraightPants, Skirt]
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Sandals, Bailarinas]
                  , purses = [Tote]
                  }

