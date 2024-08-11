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

module Lib.Actions.Unsafe
  ( unsafeEmptyShared
  , unsafeStoreGroup
  , unsafeStoreActor
  , unsafeAddMember
  , unsafeStoreSpace
  , unsafeStoreEntity
  , StoreVersionError
  , unsafeStoreVersion
  , LinkGroupError (..)
  , unsafeLinkGroups
  , unsafeUnlinkGroups
  , unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  , unsafeAdjustGroupPermission
  , unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  , unsafeAdjustMemberPermission
  ) where

import Lib.Actions.Tabulation
  ( updateTabulationStartingAt
  )
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption
  , SinglePermission
  )
import Lib.Types.Store
  ( Shared (..)
  , Store (..)
  , Temp (..)
  , store
  , temp
  , toGroups
  , toActors
  , toSpaces
  , toEntities
  , toVersions
  , toReferencesFrom
  , toReferencesFromEntities
  , toReferencesFromSpaces
  , toSubscriptionsFrom
  , toSubscriptionsFromSpaces
  , toSpacePermissions
  , toEntityPermissions
  , toGroupPermissions
  , toMemberPermissions
  )
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (initEntity, addVersion, space)
import Lib.Types.Store.Version (Version, references, subscriptions, entity)
import Lib.Types.Store.Groups
  ( Group
  , emptyGroup
  , emptyGroups
  , hasCycle
  , nodes
  , universePermission
  , organizationPermission
  , recruiterPermission
  , roots
  , outs
  , edges
  , next
  , prev
  , members
  )

import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Foldable (for_, foldlM)
import Control.Lens (Lens', (&), (^.), (.~), (%~), at, non, ix)
import Control.Monad.State (MonadState (get, put), modify)
import Control.Monad.Extra (when)



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
  , sharedTemp = Temp
    { tempReferencesFrom = mempty
    , tempReferencesFromEntities = mempty
    , tempReferencesFromSpaces = mempty
    , tempSubscriptionsFrom = mempty
    , tempSubscriptionsFromSpaces = mempty
    , tempTabulatedGroups = mempty
    }
  }

-- | Sets the group to empty
unsafeStoreGroup :: MonadState Shared m => GroupId -> m ()
unsafeStoreGroup gId = do
  modify $ store . toGroups . nodes . at gId %~ Just . fromMaybe emptyGroup
  s <- get
  when (null (s ^. store . toGroups . nodes . at gId . non emptyGroup . prev)) $
    modify $ store . toGroups . roots . at gId .~ Just ()

unsafeStoreActor :: MonadState Shared m => ActorId -> m ()
unsafeStoreActor aId = do
  modify $ store . toActors . at aId .~ Just mempty

unsafeAddMember :: MonadState Shared m => GroupId -> ActorId -> m ()
unsafeAddMember gId aId = do
  modify $ store . toActors . at aId . non mempty . at gId .~ Just ()
  modify $ store . toGroups . nodes . at gId . non emptyGroup . members . at aId .~ Just ()

-- | Sets the space to empty
unsafeStoreSpace :: MonadState Shared m => SpaceId -> m ()
unsafeStoreSpace sId = do
  modify $ store . toSpaces . at sId .~ Just mempty

unsafeStoreEntity
  :: MonadState Shared m
  => EntityId
  -> SpaceId
  -> VersionId
  -> (EntityId -> Version)
  -> m (Either StoreVersionError ())
unsafeStoreEntity eId sId vId buildVersion = do
  s <- get
  let v = buildVersion eId
      s' = s & store . toEntities . at eId .~ Just (initEntity sId vId)
             & store . toSpaces . ix sId . entities . at eId .~ Just ()
             & store . toVersions . at vId .~ Just (buildVersion eId)
      storeRefs :: Shared -> VersionId -> Either StoreVersionError Shared
      storeRefs s' refId =
        let s = s' & temp . toReferencesFrom . at refId . non mempty . at vId .~ Just ()
        in case s ^. store . toVersions . at refId of
            Nothing -> Left $ ReferenceVersionNotFound refId
            Just ref ->
              let refEId = ref ^. entity
                  s' = s & temp . toReferencesFromEntities . at refEId . non mempty . at vId .~ Just ()
              in  case s' ^. store . toEntities . at refEId of
                    Nothing -> Left $ ReferenceEntityNotFound refEId
                    Just refE ->
                      let refSId = refE ^. space
                      in  pure $ s' & temp . toReferencesFromSpaces . at refSId . non mempty . at vId .~ Just ()
      eS = foldlM storeRefs s' (HS.toList $ v ^. references)
  case eS of
    Left e -> pure (Left e)
    Right s -> do
      let storeSubs :: Shared -> EntityId -> Either StoreVersionError Shared
          storeSubs s subId =
            let s' = s & temp . toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()
            in  case s' ^. store . toEntities . at subId of
                  Nothing -> Left $ SubscriptionEntityNotFound subId
                  Just sub ->
                    let subSId = sub ^. space
                    in  pure $ s' & temp . toSubscriptionsFromSpaces . at subSId . non mempty . at vId .~ Just ()
          eS = foldlM storeSubs s (HS.toList $ v ^. subscriptions)
      case eS of
        Left e -> pure (Left e)
        Right s -> Right () <$ put s

data StoreVersionError
  = ReferenceVersionNotFound VersionId
  | ReferenceEntityNotFound EntityId
  | SubscriptionEntityNotFound EntityId
  deriving (Eq, Show, Read)

unsafeStoreVersion
  :: MonadState Shared m
  => EntityId
  -> VersionId
  -> (EntityId -> Version)
  -> m (Either StoreVersionError ())
unsafeStoreVersion eId vId buildVersion = do
  s <- get
  let v = buildVersion eId
      s' = s & store . toEntities . ix eId %~ flip addVersion vId
             & store . toVersions . at vId .~ Just v
      storeRefs :: Shared -> VersionId -> Either StoreVersionError Shared
      storeRefs s' refId =
        let s = s' & temp . toReferencesFrom . at refId . non mempty . at vId .~ Just ()
        in case s ^. store . toVersions . at refId of
            Nothing -> Left $ ReferenceVersionNotFound refId
            Just ref ->
              let refEId = ref ^. entity
                  s' = s & temp . toReferencesFromEntities . at refEId . non mempty . at vId .~ Just ()
              in  case s' ^. store . toEntities . at refEId of
                    Nothing -> Left $ ReferenceEntityNotFound refEId
                    Just refE ->
                      let refSId = refE ^. space
                      in  pure $ s' & temp . toReferencesFromSpaces . at refSId . non mempty . at vId .~ Just ()
      eS = foldlM storeRefs s' (HS.toList $ v ^. references)
  case eS of
    Left e -> pure (Left e)
    Right s -> do
      let storeSubs :: Shared -> EntityId -> Either StoreVersionError Shared
          storeSubs s subId =
            let s' = s & temp . toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()
            in  case s' ^. store . toEntities . at subId of
                  Nothing -> Left $ SubscriptionEntityNotFound subId
                  Just sub ->
                    let subSId = sub ^. space
                    in  pure $ s' & temp . toSubscriptionsFromSpaces . at subSId . non mempty . at vId .~ Just ()
          eS = foldlM storeSubs s (HS.toList $ v ^. subscriptions)
      case eS of
        Left e -> pure (Left e)
        Right s -> Right () <$ put s

data LinkGroupError
  = CycleDetected [GroupId]
  | DuplicateEdge GroupId GroupId
  | MultiParent GroupId
  deriving (Eq, Show, Read)

-- | Loads the parent's untabulated permissions if it's not already tabulated!
unsafeLinkGroups :: MonadState Shared m => GroupId -> GroupId -> m (Either LinkGroupError ())
unsafeLinkGroups from to = do
  s <- get
  let groups = s ^. store . toGroups
  if HS.member (from, to) (groups ^. edges)
  then pure (Left (DuplicateEdge from to))
  else if HS.member to (groups ^. outs)
  then pure (Left (MultiParent to))
  else
    let newGroups = groups
          & edges . at (from,to) .~ Just ()
          & outs . at to .~ Just ()
          & outs . at from .~ Nothing
          & roots . at to .~ Nothing
          & nodes . ix from . next . at to .~ Just ()
          & nodes . ix to . prev .~ Just from
    in case hasCycle newGroups of
        Just cycle -> pure . Left $ CycleDetected cycle
        Nothing -> do
          modify $ store . toGroups .~ newGroups
          updateTabulationStartingAt to
          pure (Right ())

unsafeUnlinkGroups :: MonadState Shared m => GroupId -> GroupId -> m ()
unsafeUnlinkGroups from to = do
  s <- get
  let groups = s ^. store . toGroups
      newGroups = groups
        & edges . at (from,to) .~ Nothing
        & outs . at to .~ Nothing
        & outs . at from .~ Just ()
        & roots . at to .~ Just ()
        & nodes . ix from . next . at to .~ Nothing
        & nodes . ix to . prev .~ Nothing
  put $ s
      & store . toGroups .~ newGroups
  updateTabulationStartingAt to

unsafeAdjustPermissionForGroup
  :: MonadState Shared m
  => Lens' Group a
  -> (a -> a)
  -> GroupId
  -> m ()
unsafeAdjustPermissionForGroup project f gId = do
  modify $ store . toGroups . nodes . ix gId . project %~ f
  updateTabulationStartingAt gId

unsafeAdjustUniversePermission
  :: MonadState Shared m
  => (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
  -> GroupId
  -> m ()
unsafeAdjustUniversePermission =
  unsafeAdjustPermissionForGroup universePermission

unsafeAdjustOrganizationPermission
  :: MonadState Shared m
  => (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
  -> GroupId
  -> m ()
unsafeAdjustOrganizationPermission =
  unsafeAdjustPermissionForGroup organizationPermission

unsafeAdjustRecruiterPermission
  :: MonadState Shared m
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> m ()
unsafeAdjustRecruiterPermission =
  unsafeAdjustPermissionForGroup recruiterPermission

unsafeAdjustPermission
  :: MonadState Shared m
  => Lens' Shared a
  -> (a -> a)
  -> GroupId
  -> m ()
unsafeAdjustPermission project f gId = do
  modify $ project %~ f
  updateTabulationStartingAt gId

unsafeAdjustSpacePermission
  :: MonadState Shared m
  => (Maybe SinglePermission -> Maybe SinglePermission)
  -> GroupId
  -> SpaceId
  -> m ()
unsafeAdjustSpacePermission f gId sId =
  unsafeAdjustPermission
    (store . toSpacePermissions . at gId . non mempty . at sId)
    f
    gId

unsafeAdjustEntityPermission
  :: MonadState Shared m
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> SpaceId
  -> m ()
unsafeAdjustEntityPermission f gId sId =
  unsafeAdjustPermission
    (store . toEntityPermissions . at gId . non mempty . at sId . non Blind)
    f
    gId

unsafeAdjustGroupPermission
  :: MonadState Shared m
  => (Maybe SinglePermission -> Maybe SinglePermission) -- ^ get new permission
  -> GroupId -- ^ group gaining new permission
  -> GroupId -- ^ group being subject to manipulation
  -> m ()
unsafeAdjustGroupPermission f gId gId' =
  unsafeAdjustPermission
    (store . toGroupPermissions . at gId . non mempty . at gId')
    f
    gId

unsafeAdjustMemberPermission
  :: MonadState Shared m
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> GroupId
  -> m ()
unsafeAdjustMemberPermission f gId gId' =
  unsafeAdjustPermission
    (store . toMemberPermissions . at gId . non mempty . at gId' . non Blind)
    f
    gId
