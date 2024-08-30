module Lib.Async.Actions.Unsafe.Store where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Extra (when)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing)
import DeferredFolds.UnfoldlM (forM_)
import qualified Focus
import Lib.Async.Actions.Tabulation (
  loadForks,
  loadRefsAndSubs,
  updateTabulatedPermissionsStartingAt,
 )
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (
  Shared,
  newPermissionsPerGroup,
  store,
  temp,
  toActors,
  toEntities,
  toEntityOf,
  toForks,
  toGroups,
  toGroupsPrev,
  toMemberOf,
  toMembers,
  toPermOrganization,
  toPermOther,
  toPermRecruiter,
  toPermUniverse,
  toRoots,
  toSpaceEntities,
  toSpaceOf,
  toSpaces,
  toSpacesHiddenTo,
  toVersions,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (CollectionPermission (Blind), collectionPermission)
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import qualified StmContainers.Set as Set

unsafeStoreGroup :: GroupId -> TerseM STM ()
unsafeStoreGroup gId = do
  s <- ask
  liftBase $ do
    Set.insert gId (s ^. store . toGroups)
    Map.insert minBound gId (s ^. store . toPermUniverse)
    Map.insert minBound gId (s ^. store . toPermOrganization)
    Map.insert minBound gId (s ^. store . toPermRecruiter)
    gs <- newPermissionsPerGroup
    Map.insert gs gId (s ^. store . toPermOther)
    mPrev <- Map.lookup gId (s ^. store . toGroupsPrev)
    when (isNothing mPrev) $
      Set.insert gId (s ^. store . toRoots)
  updateTabulatedPermissionsStartingAt gId

unsafeStoreActor :: ActorId -> TerseM STM ()
unsafeStoreActor aId = do
  s <- ask
  liftBase $
    Set.insert aId (s ^. store . toActors)

unsafeAddMember :: GroupId -> ActorId -> TerseM STM ()
unsafeAddMember gId aId = do
  s <- ask
  liftBase $ do
    Multimap.insert gId aId (s ^. temp . toMemberOf)
    Multimap.insert aId gId (s ^. store . toMembers)

unsafeStoreSpace :: SpaceId -> TerseM STM ()
unsafeStoreSpace sId = do
  s <- ask
  liftBase $ do
    Set.insert sId (s ^. store . toSpaces)
    forM_ (Map.unfoldlM (s ^. store . toPermUniverse)) $ \(gId, p) ->
      -- FIXME what about already set permissions against this sId? Specifically granting it visibility?
      when (p ^. collectionPermission == Blind) $
        Multimap.insert gId sId (s ^. temp . toSpacesHiddenTo)

unsafeStoreEntity
  :: EntityId
  -> SpaceId
  -> VersionId
  -> Maybe VersionId
  -> TerseM STM ()
unsafeStoreEntity eId sId vId mForkId = do
  s <- ask
  liftBase $ do
    Map.insert (NE.singleton vId) eId (s ^. store . toEntities)
    Multimap.insert eId sId (s ^. store . toSpaceEntities)
    case mForkId of
      Nothing -> pure ()
      Just forkId -> Map.insert forkId eId (s ^. store . toForks)
    Set.insert vId (s ^. store . toVersions)
    Map.insert sId eId (s ^. temp . toSpaceOf)
    Map.insert eId vId (s ^. temp . toEntityOf)
  loadRefsAndSubs vId
  loadForks eId

unsafeStoreVersion
  :: EntityId
  -> VersionId
  -> TerseM STM ()
unsafeStoreVersion eId vId = do
  s <- ask
  liftBase $ do
    Map.focus (Focus.adjust (vId <|)) eId (s ^. store . toEntities)
    Set.insert vId (s ^. store . toVersions)
    Map.insert eId vId (s ^. temp . toEntityOf)
  loadRefsAndSubs vId
