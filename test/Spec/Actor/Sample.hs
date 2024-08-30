module Spec.Actor.Sample where

import Control.Concurrent.STM (STM, TMVar)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (CollectionPermission)
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import StmContainers.Set (Set)
import qualified StmContainers.Set as Set

-- | Represents the universe visible to an actor
data ObservableScope = ObservableScope
  { observableScopeActors :: Set ActorId
  , observableScopeGroups
      :: Map GroupId (CollectionPermission, CollectionPermission)
  , observableScopeSpaces
      :: Map SpaceId (CollectionPermission, CollectionPermission)
  , observableScopeEntities :: Set EntityId
  , observableScopeVersions :: Set VersionId
  }

newObservableScope :: STM ObservableScope
newObservableScope = ObservableScope <$> Set.new <*> Map.new <*> Map.new <*> Set.new <*> Set.new

-- I'd like to allow for an actor to try and figure stuff out if it doesn't know yet -
-- like try and get what groups they belong to, and the permissions of each group,
-- or also the the compiled permissions they have for themselves. Likewise, I'd like for them
-- to scan the set of spaces available to them, the set of all groups they can see,
-- see what entities & versions are inside each space, see the set of all other actors,
-- then after doing a fair bit of reading - make some actions (that they're allowed to do).
-- If they fail in doing something, it shouldn't be the end of the world, but they should probably
-- adjust their notion of what they're allowed to do.
