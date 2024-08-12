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

module Lib.Actions.Safe.Verify.SpaceAndEntity where

import Lib.Actions.Safe.Verify.Utils (canDo, canDoWithTab)
import Lib.Types.Store
  ( Shared
  , store
  , temp
  , toReferencesFromEntities
  , toReferencesFromSpaces
  , toSubscriptionsFrom
  , toSubscriptionsFromSpaces
  , toVersions
  , toEntities
  )
import Lib.Types.Store.Tabulation.Group (forUniverse, forSpaces, forEntities)
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Version (entity)
import Lib.Types.Id (ActorId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , SinglePermission (Exists)
  , escalate
  , collectionPermission
  )

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.HashSet as HS
import Control.Lens ((^.), at, non)
import Control.Monad.State (MonadState, get)
import Control.Monad.Extra (andM, orM)

-- * Spaces

canReadSpace :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canReadSpace reader sId =
  canDo
    (\t -> fromMaybe (t ^. forUniverse . collectionPermission) (t ^. forSpaces . at sId))
    reader
    Read

canCreateSpace :: MonadState Shared m => ActorId -> m Bool
canCreateSpace creater =
  canDo (\t -> t ^. forUniverse . collectionPermission) creater Create

canUpdateSpace :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canUpdateSpace updater sId =
  canDo
    (\t -> fromMaybe (t ^. forUniverse . collectionPermission) (t ^. forSpaces . at sId))
    updater
    Update

canDeleteSpace :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canDeleteSpace deleter sId = do
  refsAndSubs <- do
    s <- get
    let refs = s ^. temp . toReferencesFromSpaces . at sId . non mempty
        subs = s ^. temp . toSubscriptionsFromSpaces . at sId . non mempty
    pure (HS.union refs subs)
  andM $ 
    (canDo
      (\t -> fromMaybe (t ^. forUniverse . collectionPermission) (t ^. forSpaces . at sId))
      deleter
      Delete) : (map (canUpdateVersion deleter) $ HS.toList refsAndSubs)

hasSpacePermission :: MonadState Shared m => ActorId -> SpaceId -> SinglePermission -> m Bool
hasSpacePermission aId sId p =
  canDoWithTab
    (\t -> fromMaybe (t ^. forUniverse . collectionPermission) (t ^. forSpaces . at sId))
    aId
    (\t -> escalate (t ^. forUniverse) p)

-- * Entities

canReadEntity :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canReadEntity reader sId = andM
  [ canDo (\t -> t ^. forEntities . at sId . non Blind) reader Read
  , canReadSpace reader sId
  ]

canCreateEntity :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canCreateEntity creater sId = andM
  [ canDo (\t -> t ^. forEntities . at sId . non Blind) creater Create -- FIXME dilemma of being permitted entity rights but not space rights
  , canReadSpace creater sId
  ]

canUpdateEntity :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canUpdateEntity updater sId = andM
  [ canDo (\t -> t ^. forEntities . at sId . non Blind) updater Update -- FIXME dilemma of being permitted entity rights but not space rights
  , canReadSpace updater sId
  ]

-- FIXME references & subscriptions!
canDeleteEntity :: MonadState Shared m => ActorId -> SpaceId -> EntityId -> m Bool
canDeleteEntity deleter sId eId = do
  refsAndSubs <- do
    s <- get
    let refs = s ^. temp . toReferencesFromEntities . at eId . non mempty
        subs = s ^. temp . toSubscriptionsFrom . at eId . non mempty
    pure (HS.union refs subs)
  andM $
    (canDo (\t -> t ^. forEntities . at sId . non Blind) deleter Delete) :
    (canReadSpace deleter sId) :
    (map (canUpdateVersion deleter) $ HS.toList refsAndSubs)

hasEntityPermission :: MonadState Shared m => ActorId -> SpaceId -> CollectionPermission -> m Bool
hasEntityPermission aId sId p = andM
  [ canReadSpace aId sId
  , orM
    [ canDo (\t -> t ^. forEntities . at sId . non Blind) aId p
    , canDo (\t -> t ^. forUniverse . collectionPermission) aId Update
    ]
  ]

canReadVersion :: MonadState Shared m => ActorId -> VersionId -> m Bool
canReadVersion updater vId = do
  s <- get
  let v = fromJust (s ^. store . toVersions . at vId)
      e = fromJust (s ^. store . toEntities . at (v ^. entity))
  canReadEntity updater (e ^. space)

canCreateVersion :: MonadState Shared m => ActorId -> VersionId -> m Bool
canCreateVersion updater vId = do
  s <- get
  let v = fromJust (s ^. store . toVersions . at vId)
      e = fromJust (s ^. store . toEntities . at (v ^. entity))
  canCreateEntity updater (e ^. space)

canUpdateVersion :: MonadState Shared m => ActorId -> VersionId -> m Bool
canUpdateVersion updater vId = do
  s <- get
  let v = fromJust (s ^. store . toVersions . at vId)
      e = fromJust (s ^. store . toEntities . at (v ^. entity))
  canUpdateEntity updater (e ^. space)

canDeteteVersion :: MonadState Shared m => ActorId -> VersionId -> m Bool
canDeteteVersion updater vId = do
  s <- get
  let v = fromJust (s ^. store . toVersions . at vId)
      e = fromJust (s ^. store . toEntities . at (v ^. entity))
  canDeleteEntity updater (e ^. space) (v ^. entity)
