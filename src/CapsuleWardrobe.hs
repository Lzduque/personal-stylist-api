module CapsuleWardrobe
  (CapsuleWardrobe(..)
  , Top(..)
  , Pants(..)
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
                  , pants       :: [Pants]
                  , skirts      :: [Skirt]
                  , dresses     :: [Dress]
                  , overalls    :: [Overall]
                  , shoes       :: [Shoe]
                  , purses      :: [Purse]
                  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Top = ShortSleeveShirt | LongSleeveShirt | TankTop | ShortSleeveBlouse | LongSleeveBlouse
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Pants = SkinnyJeans | StraightJeans | CroppedJeans | Capris | JeansShorts | StraightPants
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Skirt = Skirt
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Dress = Dress
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- to do: think about transforming cardigans in layer2 and coats in layer3, so you can do a better count of the number of outfits (abstraction to layers)
data Overall = Cardigan | TrenchCoat | WinterCoat
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Shoe = Sandals | Bailarinas | Heels | Boots
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Purse = Tote | Backpack | Clutch
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


autumnWinterCasualCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse]
                  , pants = [SkinnyJeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
                  , purses = [Backpack]
                  }

springSummerCasualCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse]
                  , pants = [SkinnyJeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Sandals, Bailarinas]
                  , purses = [Tote]
                  }

autumnWinterOfficeCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse]
                  , pants = [StraightPants]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat, WinterCoat]
                  , shoes = [Boots, Bailarinas]
                  , purses = [Backpack]
                  }

springSummerOfficeCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse]
                  , pants = [StraightPants]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Sandals, Bailarinas]
                  , purses = [Tote]
                  }

