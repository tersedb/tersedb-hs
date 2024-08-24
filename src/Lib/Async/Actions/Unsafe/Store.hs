module Lib.Async.Actions.Unsafe.Store where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad.Extra (when)
import Control.Monad.Reader (MonadReader (ask))
import Data.Maybe (isNothing)
import DeferredFolds.UnfoldlM (forM_)
import Lib.Async.Types.Store (
  Shared,
  newPermissionsPerGroup,
  store,
  temp,
  toActors,
  toGroupsPrev,
  toMemberOf,
  toMembers,
  toPermOrganization,
  toPermOther,
  toPermRecruiter,
  toPermUniverse,
  toRoots,
  toSpacesHiddenTo,
 )
import Lib.Types.Id (ActorId, GroupId, SpaceId)
import Lib.Types.Permission (CollectionPermission (Blind), collectionPermission)
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import qualified StmContainers.Set as Set

unsafeStoreGroup :: (MonadReader Shared m) => GroupId -> m (STM ())
unsafeStoreGroup gId = do
  s <- ask
  pure $ do
    Map.insert minBound gId (s ^. store . toPermUniverse)
    Map.insert minBound gId (s ^. store . toPermOrganization)
    Map.insert minBound gId (s ^. store . toPermRecruiter)
    gs <- newPermissionsPerGroup
    Map.insert gs gId (s ^. store . toPermOther)
    mPrev <- Map.lookup gId (s ^. store . toGroupsPrev)
    when (isNothing mPrev) $
      Set.insert gId (s ^. store . toRoots)

-- updateTabulationStartingAt gId

unsafeStoreActor :: (MonadReader Shared m) => ActorId -> m (STM ())
unsafeStoreActor aId = do
  s <- ask
  pure $
    Set.insert aId (s ^. store . toActors)

unsafeAddMember :: (MonadReader Shared m) => GroupId -> ActorId -> m (STM ())
unsafeAddMember gId aId = do
  s <- ask
  pure $ do
    Multimap.insert gId aId (s ^. temp . toMemberOf)
    Multimap.insert aId gId (s ^. store . toMembers)

unsafeStoreSpace :: (MonadReader Shared m) => SpaceId -> m (STM ())
unsafeStoreSpace sId = do
  s <- ask
  pure $ do
    forM_ (Map.unfoldlM (s ^. store . toPermUniverse)) $ \(gId, p) ->
      when (p ^. collectionPermission == Blind) $
        Multimap.insert gId sId (s ^. temp . toSpacesHiddenTo)

-- FIXME what about already set permissions against this sId? Specifically granting it visibility?

-- unsafeStoreEntity
--   :: MonadReader Shared m
--   => EntityId
--   -> SpaceId
--   -> VersionId
--   -> Maybe VersionId
--   -> m (STM (Either VersionId ()))
-- unsafeStoreEntity eId sId vId mForkId = do
--   s <- ask
--   pure $ do
