module Lib.Async.Types.Store where

import StmContainers.Multimap (Multimap)
import Lib.Types.Id (VersionId, EntityId, SpaceId, GroupId)

data Temp = Temp
  { tempReferencesFrom :: Multimap VersionId VersionId
  , tempSubscriptionsFrom :: Multimap EntityId VersionId
  , tempForksFrom :: Multimap VersionId EntityId
  , tempSpacesHiddenTo :: Multimap SpaceId GroupId
  }


-- data Store = Store
--   { storeActors :: 
--   }
