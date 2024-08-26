module Lib.Async.Actions.Unsafe.Remove where

import Lib.Types.Id (VersionId)
import Lib.Async.Types.Monad (TerseM)
import Control.Concurrent.STM (STM)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.Base (MonadBase(liftBase))
import qualified StmContainers.Set as Set
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import Lib.Async.Types.Store (store, toVersions, temp, toEntityOf, toEntities, toReferences, toReferencesFrom, toSubscriptions, toSubscriptionsFrom, toForks, toForksFrom)
import Control.Lens ((^.))
import qualified Data.List.NonEmpty as NE
import qualified Focus
import Control.Monad (unless)
import DeferredFolds.UnfoldlM (forM_)

unsafeRemoveVersion :: VersionId -> TerseM STM ()
unsafeRemoveVersion vId = do
  s <- ask
  liftBase $ do
    mEId <- Map.lookup vId (s ^. temp . toEntityOf)
    case mEId of
      Nothing -> pure ()
      Just eId -> do
        vs <- Map.lookup eId (s ^. store . toEntities)
        unless (length vs == 1) $ do
          Map.focus (Focus.adjust (NE.fromList . NE.filter (/= vId))) eId (s ^. store . toEntities)
          Set.delete vId (s ^. store . toVersions)
          forM_ (Multimap.unfoldlMByKey vId (s ^. store . toReferences)) $ \refId ->
            Multimap.delete vId refId (s ^. temp . toReferencesFrom)
          Multimap.deleteByKey vId (s ^. store . toReferences)
          Multimap.deleteByKey vId (s ^. temp . toReferencesFrom)
          forM_ (Multimap.unfoldlMByKey vId (s ^. store . toSubscriptions)) $ \subId ->
            Multimap.delete vId subId (s ^. temp . toSubscriptionsFrom)
          Multimap.deleteByKey vId (s ^. store . toSubscriptions)
          forM_ (Multimap.unfoldlMByKey vId (s ^. temp . toForksFrom)) $ \forkId ->
            Map.delete forkId (s ^. store . toForks)
          Multimap.deleteByKey vId (s ^. temp . toForksFrom)

