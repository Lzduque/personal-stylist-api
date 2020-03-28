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

data Top = LongSleeveShirt | LongSleeveBlouse | ShortSleeveShirt | ShortSleeveBlouse | TankTop
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Pants = Jeans | JeansShorts | SocialPants | SocialShorts
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Skirt = LongSkirt | ShortSkirt
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Dress = LongSleeveDress | ShortSleeveDress | NoSleeveDress
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- to do: think about transforming cardigans in layer2 and coats in layer3, so you can do a better count of the number of outfits (abstraction to layers)
-- to do: WinterCoat??
data Overall = Cardigan | TrenchCoat | WoolCoat
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Shoe = Sandals | Flats | Heels | Boots
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Purse = Tote | Backpack | Clutch
  deriving (Show, Eq, Generic, ToJSON, FromJSON)



data Clothing a = Clothing Top | Clothing Pants | 

-- base Capsule Wardrobes
autumnWinterCasualCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveShirt, LongSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [Backpack]
                  }

springSummerCasualCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveShirt, ShortSleeveBlouse]
                  , pants = [Jeans, Jeans]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Sandals, Flats]
                  , purses = [Tote]
                  }

autumnWinterOfficeCW =
  CapsuleWardrobe { tops = [LongSleeveShirt, LongSleeveBlouse, LongSleeveBlouse]
                  , pants = [SocialPants, SocialPants]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Boots, Flats]
                  , purses = [Backpack]
                  }

springSummerOfficeCW =
  CapsuleWardrobe { tops = [ShortSleeveShirt, ShortSleeveBlouse, ShortSleeveBlouse]
                  , pants = [SocialPants, SocialPants]
                  , skirts = []
                  , dresses = []
                  , overalls = [Cardigan, TrenchCoat]
                  , shoes = [Sandals, Flats]
                  , purses = [Tote]
                  }

