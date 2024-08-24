module Lib.Async.Types.Tabulation where

import Control.Concurrent.STM (STM)
import Control.Lens.TH (makeLensesFor)
import Data.Hashable (Hashable)
import DeferredFolds.UnfoldlM (forM_)
import qualified Focus
import GHC.Generics (Generic)
import Lib.Types.Id (GroupId, SpaceId)
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
 )
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map

data TabulatedPermissions = TabulatedPermissions
  { tabulatedPermissionsSpaces :: Map SpaceId CollectionPermission
  , tabulatedPermissionsEntities :: Map SpaceId CollectionPermission
  , tabulatedPermissionsGroups :: Map GroupId CollectionPermission
  , tabulatedPermissionsMembers :: Map GroupId CollectionPermission
  }
  deriving (Generic)
makeLensesFor
  [ ("tabulatedPermissionsSpaces", "forSpaces")
  , ("tabulatedPermissionsEntities", "forEntities")
  , ("tabulatedPermissionsGroups", "forGroups")
  , ("tabulatedPermissionsMembers", "forMembers")
  ]
  ''TabulatedPermissions

new :: STM TabulatedPermissions
new = TabulatedPermissions <$> Map.new <*> Map.new <*> Map.new <*> Map.new

absorb :: TabulatedPermissions -> TabulatedPermissions -> STM ()
absorb keep take = do
  let over
        :: (Hashable k) => (TabulatedPermissions -> Map k CollectionPermission) -> STM ()
      over f = forM_ (Map.unfoldlM (f take)) $ \(sId, p) ->
        Map.focus (Focus.alter (Just . maybe p (<> p))) sId (f keep)
  over tabulatedPermissionsSpaces
  over tabulatedPermissionsEntities
  over tabulatedPermissionsGroups
  over tabulatedPermissionsMembers
