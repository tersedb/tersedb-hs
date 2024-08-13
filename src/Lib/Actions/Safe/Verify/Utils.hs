module Lib.Actions.Safe.Verify.Utils where

import Lib.Types.Id (ActorId)
import Lib.Types.Store
  ( Shared
  , store
  , temp
  , toActors
  , toTabulatedGroups
  )
import Lib.Types.Store.Tabulation.Group
  ( TabulatedPermissionsForGroup (..)
  )

import qualified Data.HashSet as HS
import Control.Lens ((^.), at, non)
import Control.Monad.State (MonadState (get))
import Control.Monad.Extra (when)

-- | Looks first for the groups the user is in, then sees if any of the groups
-- can do the action, depicted by the Lens
canDoWithTab
  :: ( MonadState Shared m
     , Ord a
     ) => (TabulatedPermissionsForGroup -> a) -- ^ Specific permission being checked
       -> ActorId -- ^ Actor requesting permission
       -> (TabulatedPermissionsForGroup -> a) -- ^ Minimum permission actor needs
       -> m Bool
canDoWithTab proj creator getP = do
  s <- get
  pure $ case s ^. store . toActors . at creator of
    Just groups ->
      let perGroup gId =
            let tab = s ^. temp . toTabulatedGroups . at gId . non mempty
            in  proj tab >= getP tab
      in  any perGroup (HS.toList groups)
    _ -> False

canDo
  :: ( MonadState Shared m
     , Ord a
     ) => (TabulatedPermissionsForGroup -> a) -- ^ Specific permission being checked
       -> ActorId -- ^ Actor requesting permission
       -> a -- ^ Minimum permission actor needs
       -> m Bool
canDo a b c = canDoWithTab a b (const c)

conditionally :: Applicative m => m () -> Bool -> m Bool
conditionally f t = t <$ when t f
