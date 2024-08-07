{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , DerivingVia
  , DataKinds
  , DeriveGeneric
  , RankNTypes
  , TemplateHaskell
  , FlexibleContexts
  #-}

module Lib.Types.Store where

import Lib.Types.Store.Groups (Groups, emptyGroups)
import Lib.Types.Store.Space (Space)
import Lib.Types.Id (GroupId, SpaceId)
import Lib.Types.Permission (Permission)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Lens.TH (makeLensesFor)
import Test.QuickCheck (Arbitrary (arbitrary))


data TabulatedPermissionsForGroup = TabulatedPermissionsForGroup
  { tabulatedPermissionsForGroupUniverse :: Permission
  , tabulatedPermissionsForGroupSpaces :: HashMap SpaceId Permission
  , tabulatedPermissionsForGroupEntities :: HashMap SpaceId Permission
  } deriving (Eq, Show, Read)
makeLensesFor
  [ ("tabulatedPermissionsForGroupUniverse", "forGroupUniverse")
  , ("tabulatedPermissionsForGroupSpaces", "forGroupSpaces")
  , ("tabulatedPermissionsForGroupEntities", "forGroupEntities")
  ] ''TabulatedPermissionsForGroup

instance Semigroup TabulatedPermissionsForGroup where
  x <> y = TabulatedPermissionsForGroup
    { tabulatedPermissionsForGroupUniverse =
        tabulatedPermissionsForGroupUniverse x
          <> tabulatedPermissionsForGroupUniverse y
    , tabulatedPermissionsForGroupSpaces =
        HM.unionWith (<>)
          (tabulatedPermissionsForGroupSpaces x)
          (tabulatedPermissionsForGroupSpaces y)
    , tabulatedPermissionsForGroupEntities =
        HM.unionWith (<>)
          (tabulatedPermissionsForGroupEntities x)
          (tabulatedPermissionsForGroupEntities y)
    }
instance Monoid TabulatedPermissionsForGroup where
  mempty = TabulatedPermissionsForGroup
    { tabulatedPermissionsForGroupUniverse = mempty
    , tabulatedPermissionsForGroupSpaces = mempty
    , tabulatedPermissionsForGroupEntities = mempty
    }
instance Arbitrary TabulatedPermissionsForGroup where
  arbitrary = do
    u <- arbitrary
    ss <- arbitrary
    es <- arbitrary
    pure (TabulatedPermissionsForGroup u ss es)

hasLessOrEqualPermissionsTo
  :: TabulatedPermissionsForGroup
  -> TabulatedPermissionsForGroup
  -> Bool
hasLessOrEqualPermissionsTo
  (TabulatedPermissionsForGroup xu xs xe)
  (TabulatedPermissionsForGroup yu ys ye) =
    xu <= yu && HM.isSubmapOfBy (<=) xs ys && HM.isSubmapOfBy (<=) xe ye

data Store = Store
  { storeGroups :: Groups
  , storeSpaces :: HashMap SpaceId Space
  , storeSpacePermissions :: HashMap GroupId (HashMap SpaceId Permission)
  , storeEntityPermissions :: HashMap GroupId (HashMap SpaceId Permission)
  , storeTabulatedPermissions :: HashMap GroupId TabulatedPermissionsForGroup 
  } deriving (Eq, Show, Read)
makeLensesFor
  [ ("storeGroups", "toGroups")
  , ("storeSpaces", "toSpaces")
  , ("storeSpacePermissions", "toSpacePermissions")
  , ("storeEntityPermissions", "toEntityPermissions")
  , ("storeTabulatedPermissions", "toTabulatedPermissions")
  ] ''Store

emptyStore :: Store
emptyStore = Store
  { storeGroups = emptyGroups
  , storeSpaces = mempty
  , storeSpacePermissions = mempty
  , storeEntityPermissions = mempty
  , storeTabulatedPermissions = mempty
  }
