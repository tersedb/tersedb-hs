module Lib.Actions.Unsafe.Update.Group
  ( LinkGroupError (..)
  , unsafeLinkGroups
  , unsafeUnlinkGroups
  , unsafeUpdateGroupParent
  , unsafeUpdateGroupChildren
  , unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  , unsafeAdjustGroupPermission
  , unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  , unsafeAdjustMemberPermission
  ) where

import Lib.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Types.Id (GroupId, SpaceId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption
  , SinglePermission
  )
import Lib.Types.Store
  ( Shared (..)
  , store
  , toGroups
  , toSpacePermissions
  , toEntityPermissions
  , toGroupPermissions
  , toMemberPermissions
  )
import Lib.Types.Store.Groups
  ( Group
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
  )

import Data.Foldable (for_, foldlM)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import Control.Lens (Lens', (&), (^.), (.~), (%~), at, non, ix, (^?), _Just)
import Control.Monad.State (MonadState (get, put), modify, runState)


data LinkGroupError
  = CycleDetected [GroupId]
  | MultiParent GroupId
  deriving (Eq, Show, Read)

-- | Loads the parent's untabulated permissions if it's not already tabulated!
unsafeLinkGroups :: MonadState Shared m => GroupId -> GroupId -> m (Either LinkGroupError ())
unsafeLinkGroups from to = do
  s <- get
  let groups = s ^. store . toGroups
  if HS.member (from, to) (groups ^. edges)
  then pure (Right ())
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

unsafeUpdateGroupParent
  :: MonadState Shared m
  => GroupId
  -> Maybe GroupId
  -> m (Either LinkGroupError ())
unsafeUpdateGroupParent gId mParent = do
  s <- get
  case s ^? store . toGroups . nodes . ix gId . prev . _Just of
    mOldParent | mOldParent == mParent -> pure (Right ())
    Nothing -> unsafeLinkGroups (fromJust mParent) gId
    Just oldParent -> do
      unsafeUnlinkGroups oldParent gId
      case mParent of
        Nothing -> pure (Right ())
        Just newParent -> unsafeLinkGroups newParent gId

unsafeUpdateGroupChildren
  :: MonadState Shared m
  => GroupId
  -> HashSet GroupId
  -> m (Either LinkGroupError ())
unsafeUpdateGroupChildren gId children = do
  let newChildren = HS.delete gId children
  s <- get
  case s ^? store . toGroups . nodes . ix gId . next of
    Nothing -> pure (Right ()) -- FIXME gId not found - should be error?
    Just oldChildren -> do
      let toAdd = newChildren `HS.difference` oldChildren
          toRemove = oldChildren `HS.difference` newChildren
          addChild :: Shared -> GroupId -> Either LinkGroupError Shared
          addChild s toAdd =
            let (mX :: Either LinkGroupError (), s' :: Shared) =
                  runState (unsafeLinkGroups gId toAdd) s
            in  fmap (const s') mX
      case foldlM addChild s toAdd of
        Left e -> pure (Left e)
        Right s' -> do
          put s'
          for_ toRemove $ \toRemove -> unsafeUnlinkGroups gId toRemove
          pure (Right ())

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
