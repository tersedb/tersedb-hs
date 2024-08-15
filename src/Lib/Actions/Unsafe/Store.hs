module Lib.Actions.Unsafe.Store where

import Lib.Actions.Tabulation
  ( LoadRefsAndSubsError
  , loadRefsAndSubs
  , updateTabulationStartingAt
  )
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission (CollectionPermission (Blind), collectionPermission)
import Lib.Types.Store
  ( Shared (..)
  , store
  , temp
  , toGroups
  , toActors
  , toSpaces
  , toEntities
  , toVersions
  , toTabulatedGroups
  , toSpacesHiddenTo
  )
import Lib.Types.Store.Tabulation.Group (forUniverse)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (initEntity, addVersion)
import Lib.Types.Store.Version (Version)
import Lib.Types.Store.Groups
  ( emptyGroup
  , nodes
  , roots
  , prev
  , members
  )

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Control.Lens ((&), (^.), (.~), (%~), at, non, ix)
import Control.Monad.State (MonadState (get, put), modify)
import Control.Monad.Extra (when)

-- | Sets the group to empty
unsafeStoreGroup :: MonadState Shared m => GroupId -> m ()
unsafeStoreGroup gId = do
  modify $ store . toGroups . nodes . at gId %~ Just . fromMaybe emptyGroup
  s <- get
  -- save this group as a root
  when (null (s ^. store . toGroups . nodes . at gId . non emptyGroup . prev)) $
    modify $ store . toGroups . roots . at gId .~ Just ()
  updateTabulationStartingAt gId

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
  s <- get
  let blindGroups = HM.keysSet $
        HM.filter (\t -> (t ^. forUniverse . collectionPermission) == Blind)
          (s ^. temp . toTabulatedGroups) -- FIXME use some kind of non-universally blind tracker
  put $ s
      & store . toSpaces . at sId .~ Just mempty
      & temp . toSpacesHiddenTo . at sId
        .~ if null blindGroups then Nothing else Just blindGroups

unsafeStoreEntity
  :: MonadState Shared m
  => EntityId
  -> SpaceId
  -> VersionId
  -> (EntityId -> Version)
  -> m (Either LoadRefsAndSubsError ())
unsafeStoreEntity eId sId vId buildVersion = do
  s <- get
  let v = buildVersion eId
      s' = s & store . toEntities . at eId .~ Just (initEntity sId vId)
             & store . toSpaces . ix sId . entities . at eId .~ Just ()
             & store . toVersions . at vId .~ Just (buildVersion eId)
  case loadRefsAndSubs vId v (s' ^. store) (s' ^. temp) of
    Left e -> pure (Left e)
    Right t -> Right () <$ put (s' & temp .~ t)

unsafeStoreVersion
  :: MonadState Shared m
  => EntityId
  -> VersionId
  -> (EntityId -> Version)
  -> m (Either LoadRefsAndSubsError ())
unsafeStoreVersion eId vId buildVersion = do
  s <- get
  let v = buildVersion eId
      s' = s & store . toEntities . ix eId %~ flip addVersion vId
             & store . toVersions . at vId .~ Just v
  case loadRefsAndSubs vId v (s' ^. store) (s' ^. temp) of
    Left e -> pure (Left e)
    Right t -> Right () <$ put (s' & temp .~ t)
