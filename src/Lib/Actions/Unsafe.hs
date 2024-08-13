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

module Lib.Actions.Unsafe where

import Lib.Types.Store
  ( Shared (..)
  , Store (..)
  , emptyTemp
  )
import Lib.Types.Store.Groups (emptyGroups)


-- Note that this doesn't grant any initial "admin" actor
unsafeEmptyShared :: Shared
unsafeEmptyShared = Shared
  { sharedStore = Store
    { storeGroups = emptyGroups
    , storeActors = mempty
    , storeSpaces = mempty
    , storeEntities = mempty
    , storeVersions = mempty
    , storeSpacePermissions = mempty
    , storeEntityPermissions = mempty
    , storeGroupPermissions = mempty
    , storeMemberPermissions = mempty
    }
  , sharedTemp = emptyTemp
  }

