module CapsuleWardrobe
  (CapsuleWardrobe(..)
  , Top(..)
  , Pants(..)
  , Skirt(..)
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
-- WinterCoat??
data Overall = Cardigan | TrenchCoat
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Shoe = Sandals | Flats | Heels | Boots
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Purse = Tote | Backpack | Clutch
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


autumnWinterCasualCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse]
                  , pants = [SkinnyJeans, SkinnyJeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [Backpack]
                  }

springSummerCasualCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse]
                  , pants = [SkinnyJeans, SkinnyJeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Sandals, Flats]
                  , purses = [Tote]
                  }

autumnWinterOfficeCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse]
                  , pants = [StraightPants, StraightPants]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [Backpack]
                  }

springSummerOfficeCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse]
                  , pants = [StraightPants, StraightPants]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Sandals, Flats]
                  , purses = [Tote]
                  }

