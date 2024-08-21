module Lib.Actions.Safe.Verify.Utils where

import Lib.Types.Id (ActorId)
import Lib.Types.Permission (
    CollectionPermission,
    CollectionPermissionWithExemption,
    collectionPermission,
 )
import Lib.Types.Store (
    Shared,
    store,
    temp,
    toActors,
    toTabulatedGroups,
 )
import Lib.Types.Store.Tabulation.Group (
    TabulatedPermissionsForGroup (..),
 )

import Control.Lens (Lens', at, non, (^.))
import Control.Monad.Extra (when)
import Control.Monad.State (MonadState (get))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)

{- | Looks first for the groups the user is in, then sees if any of the groups
can do the action, depicted by the Lens
-}
canDoWithTab ::
    ( MonadState Shared m
    , Ord a
    ) =>
    -- | Specific permission being checked
    (TabulatedPermissionsForGroup -> a) ->
    -- | Actor requesting permission
    ActorId ->
    -- | Minimum permission actor needs
    (TabulatedPermissionsForGroup -> a) ->
    m Bool
canDoWithTab proj creator getP = do
    s <- get
    pure $ case s ^. store . toActors . at creator of
        Just groups ->
            let perGroup gId =
                    let tab = s ^. temp . toTabulatedGroups . at gId . non mempty
                     in proj tab >= getP tab
             in any perGroup groups
        _ -> False

canDo ::
    ( MonadState Shared m
    , Ord a
    ) =>
    -- | Specific permission being checked
    (TabulatedPermissionsForGroup -> a) ->
    -- | Actor requesting permission
    ActorId ->
    -- | Minimum permission actor needs
    a ->
    m Bool
canDo a b c = canDoWithTab a b (const c)

conditionally :: (Applicative m) => m () -> Bool -> m Bool
conditionally f t = t <$ when t f

withCollectionPermission ::
    (Hashable a) =>
    a ->
    Lens' TabulatedPermissionsForGroup CollectionPermissionWithExemption ->
    Lens' TabulatedPermissionsForGroup (HashMap a CollectionPermission) ->
    TabulatedPermissionsForGroup ->
    CollectionPermission
withCollectionPermission xId projMajor projMinor t =
    let maj = t ^. projMajor . collectionPermission
     in maybe maj (\p -> p `min` maj) (t ^. projMinor . at xId)
