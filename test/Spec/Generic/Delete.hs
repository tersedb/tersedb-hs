module Spec.Generic.Delete where

import Test.Syd (Spec, it, shouldBe, context, shouldSatisfy)
import Test.QuickCheck (suchThat, elements, forAll)
import Spec.Sync.Sample.Store (arbitraryShared)
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Types.Store.Entity as Sync
import qualified Lib.Sync.Types.Store.Version as Sync
import Control.Lens ((^.), (^?), Ixed (ix))
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, fromJust)
import Data.Foldable (for_)
import Control.Monad (unless)
import Lib.Class (TerseDB(genSyncShared, commit), TerseDBGen (runTerseDB), removeVersion)
import Data.Data (Proxy (Proxy))


removeTests :: forall n m. (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
removeTests Proxy = do
  it "Version" $
    let gen = suchThat arbitraryShared $ \(s, _, _) ->
          not . null . HM.filter (\e -> length (e ^. Sync.versions) > 1) $
            s ^. Sync.store . Sync.toEntities
     in forAll gen $ \(s, adminActor, adminGroup) ->
          let entsWith2OrMoreVersions =
                HM.filter (\e -> length (e ^. Sync.versions) > 1) $
                  s ^. Sync.store . Sync.toEntities
           in forAll (elements $ HM.toList entsWith2OrMoreVersions) $ \(eId, e) ->
                forAll (elements . NE.toList $ e ^. Sync.versions) $ \vId -> do
                  let go :: m Sync.Shared
                      go = do
                        worked <- removeVersion (NE.singleton adminActor) vId
                        unless worked $ error $ "Couldn't remove version " <> show (vId)
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  context "Version shouldn't exist" $
                    shouldSatisfy (s', vId, eId) $ \_ ->
                      (s' ^? Sync.store . Sync.toVersions . ix vId) == Nothing
                  context "Version shouldn't be in its entity" $
                    ( filter (== vId) . fromMaybe mempty . fmap NE.toList $
                        s' ^? Sync.store . Sync.toEntities . ix eId . Sync.versions
                      )
                      `shouldBe` mempty
                  context "References are removed" $
                    for_ (fromJust (s ^? Sync.store . Sync.toVersions . ix vId . Sync.references)) $ \refId ->
                      (s' ^? Sync.temp . Sync.toReferencesFrom . ix refId . ix vId) `shouldBe` Nothing
                  context "Subscriptions are removed" $
                    for_ (fromJust (s ^? Sync.store . Sync.toVersions . ix vId . Sync.subscriptions)) $ \subId ->
                      (s' ^? Sync.temp . Sync.toSubscriptionsFrom . ix subId . ix vId) `shouldBe` Nothing
                  context "Doesn't exist as a reference" $
                    (s' ^? Sync.temp . Sync.toReferencesFrom . ix vId) `shouldBe` Nothing
