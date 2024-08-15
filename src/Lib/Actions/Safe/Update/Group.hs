module Lib.Actions.Safe.Update.Group where

import Lib.Actions.Safe.Verify (conditionally, canUpdateGroup)
import Lib.Actions.Safe.Verify.SpaceAndEntity (hasSpacePermission, hasEntityPermission)
import Lib.Actions.Safe.Verify.Group (hasGroupPermission)
import Lib.Actions.Safe.Verify.Member (hasMemberPermission)
import Lib.Actions.Safe.Verify.Utils (canDo)
import Lib.Actions.Unsafe.Update.Group
  ( unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  , unsafeAdjustGroupPermission
  , unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  , unsafeAdjustMemberPermission
  , unsafeLinkGroups
  , unsafeUnlinkGroups
  , unsafeUpdateGroupParent
  , unsafeUpdateGroupChildren
  , LinkGroupError
  )
import Lib.Types.Id (GroupId, SpaceId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (..)
  )
import Lib.Types.Store (Shared, store, toGroups)
import Lib.Types.Store.Groups (next, nodes, prev)
import Lib.Types.Store.Tabulation.Group
  ( forUniverse
  , forOrganization
  , forRecruiter
  )

import Data.Maybe (fromMaybe)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Control.Lens ((^.), (^?), ix, _Just)
import Control.Monad.State (MonadState, get)
import Control.Monad.Extra (andM)


linkGroups
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> GroupId
  -> m (Maybe (Either LinkGroupError ()))
linkGroups updater gId childId = do
  canAdjust <- andM
    [ canUpdateGroup updater gId
    , canUpdateGroup updater childId
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeLinkGroups gId childId

unlinkGroups
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> GroupId
  -> m Bool
unlinkGroups updater gId childId = do
  canAdjust <- andM
    [ canUpdateGroup updater gId
    , canUpdateGroup updater childId
    ]
  conditionally (unsafeUnlinkGroups gId childId) canAdjust

updateGroupParent
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> Maybe GroupId
  -> m (Maybe (Either LinkGroupError ()))
updateGroupParent updater gId mParent = do
  canAdjust <- andM
    [ canUpdateGroup updater gId
    , do  s <- get
          case s ^? store . toGroups . nodes . ix gId . prev . _Just of
            Nothing -> pure True
            Just oldParent -> canUpdateGroup updater oldParent
    , case mParent of
        Nothing -> pure True
        Just newParent -> canUpdateGroup updater newParent
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeUpdateGroupParent gId mParent


updateGroupChildren
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> HashSet GroupId
  -> m (Maybe (Either LinkGroupError ()))
updateGroupChildren updater gId newChildren = do
  s <- get
  case s ^? store . toGroups . nodes . ix gId . next of
    Nothing -> pure Nothing
    Just oldChildren -> do
      canAdjust <- andM $
        (canUpdateGroup updater gId) :
        (map (canUpdateGroup updater) (HS.toList (newChildren <> oldChildren)))
      if not canAdjust then pure Nothing else
        Just <$> unsafeUpdateGroupChildren gId newChildren



-- | Will only update the group if the actor has same or greater permission
setUniversePermission
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setUniversePermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forUniverse) creator p
    ]
  conditionally
    (unsafeAdjustUniversePermission (const p) gId)
    canAdjust

setOrganizationPermission
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setOrganizationPermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forOrganization) creator p
    ]
  conditionally
    (unsafeAdjustOrganizationPermission (const p) gId)
    canAdjust

setRecruiterPermission
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setRecruiterPermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forRecruiter) creator p
    ]
  conditionally
    (unsafeAdjustRecruiterPermission (const p) gId)
    canAdjust

setSpacePermission
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> Maybe SinglePermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> SpaceId -- ^ relevant to this space
  -> m Bool
setSpacePermission creator p gId sId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , hasSpacePermission creator sId (fromMaybe Exists p)
    ]
  conditionally
    (unsafeAdjustSpacePermission (const p) gId sId)
    canAdjust

setEntityPermission
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> SpaceId -- ^ relevant to this space
  -> m Bool
setEntityPermission creator p gId sId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , hasEntityPermission creator sId p
    ]
  conditionally
    (unsafeAdjustEntityPermission (const p) gId sId)
    canAdjust

setGroupPermission
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> Maybe SinglePermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> GroupId -- ^ relevant to this group (grants one group to NEAO other groups)
  -> m Bool
setGroupPermission creator p gId towardGId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , hasGroupPermission creator towardGId (fromMaybe Exists p)
    ]
  conditionally
    (unsafeAdjustGroupPermission (const p) gId towardGId)
    canAdjust

setMemberPermission
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ the permission being granted
  -> GroupId -- ^ the group gaining the permission
  -> GroupId -- ^ the group that can have their members manipulated
  -> m Bool
setMemberPermission creator p manipulatorGId manipulatedGId = do
  canAdjust <- andM
    [ canUpdateGroup creator manipulatorGId
    , hasMemberPermission creator manipulatedGId p
    ]
  conditionally
    (unsafeAdjustMemberPermission (const p) manipulatorGId manipulatedGId)
    canAdjust
