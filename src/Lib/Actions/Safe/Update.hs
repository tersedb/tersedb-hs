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

module Lib.Actions.Safe.Update where

import Lib.Actions.Safe.Verify (canDeleteEntity, canCreateEntity, conditionally)
import Lib.Types.Id (SpaceId, EntityId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , SinglePermission (..)
  , escalate
  , collectionPermission
  )
import Lib.Types.Store
  ( Shared
  , store
  , toEntities
  , toSpaces
  )
import Lib.Types.Store.Tabulation.Group
  ( forUniverse
  , forSpaces
  , forEntities
  )
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (space)

import Data.Maybe (fromMaybe)
import Control.Lens ((^.), at, ix, non, (.~))
import Control.Monad.State (MonadState (get), modify)
import Control.Monad.Extra (andM, orM)


-- | Moving an entity between spaces requires delete authority on the current space, and create authority on the destination space
updateEntitySpace :: MonadState Shared m => ActorId -> EntityId -> SpaceId -> m Bool
updateEntitySpace updater eId newSId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure False -- FIXME
    Just e -> do
      canAdjust <- andM
        [ canDeleteEntity updater (e ^. space) eId
        , canCreateEntity updater newSId
        ]
      flip conditionally canAdjust $ do
        modify $ store . toEntities . ix eId . space .~ newSId
        modify $ store . toSpaces . ix (e ^. space) . entities . at eId .~ Nothing
        modify $ store . toSpaces . ix newSId . entities . at eId .~ Just ()
