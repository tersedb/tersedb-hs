{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , RecordWildCards
  , FlexibleInstances
  , NamedFieldPuns
  #-}

module Spec.Sample.Store where

import Spec.Sample.Tree
  ( SampleGroupTree (..)
  , loadSampleTree
  , storeSampleTree
  )

import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , SinglePermission
  )
import Lib.Types.Store (Shared, store, temp)
import Lib.Types.Store.Version (genesisVersion, forkVersion)
import Lib.Actions.Unsafe.Store
  ( unsafeStoreActor
  , unsafeStoreActor
  , unsafeStoreSpace
  , unsafeStoreEntity
  , unsafeStoreVersion
  , unsafeAddMember
  )
import Lib.Actions.Unsafe.Update
  ( unsafeUpdateVersionReferences
  , unsafeUpdateVersionSubscriptions
  )
import Lib.Actions.Unsafe.Update.Group
  ( unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  )
import Lib.Actions.Safe.Store
  ( storeActor
  , storeSpace
  , storeEntity
  , storeForkedEntity
  , storeNextVersion
  , addMember
  )
import Lib.Actions.Safe.Update
  ( updateVersionReferences
  , updateVersionSubscriptions
  )
import Lib.Actions.Safe.Update.Group
  ( setSpacePermission
  , setEntityPermission
  )

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Foldable (traverse_, for_, foldlM)
import qualified Data.Text.Lazy as LT
import Data.Maybe (isNothing, fromJust, mapMaybe)
import Data.List.NonEmpty (NonEmpty, uncons)
import qualified Data.List.NonEmpty as NE
import Text.Pretty.Simple (pShowNoColor)
import Control.Monad (void, replicateM)
import Control.Monad.Extra (unless)
import Control.Monad.State (State, execState, modify, get)
import Control.Lens ((%~), (.~), (^.), at, _1, _2)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Gen
  , listOf
  , listOf1
  , elements
  , chooseInt
  )

type SampleEntity =
  ( SpaceId
  , NonEmpty (VersionId, HashSet VersionId, HashSet EntityId)
  , Maybe VersionId
  )

data SampleStore = SampleStore
  { sampleSpaces :: HashSet SpaceId
  , sampleEntities :: [(EntityId, SampleEntity)] -- "should be forked"
  , sampleActors :: HashMap ActorId (HashSet GroupId)
  , sampleGroups :: SampleGroupTree (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission) -- spaces & entities
  } deriving (Eq, Show, Read)


instance Arbitrary SampleStore where
  arbitrary = do
    spaces <- arbitrary
    -- FIXME sample entityId's from set
    entities <- if null spaces then pure mempty else do
      let go :: [(EntityId, SampleEntity)] -> () -> Gen [(EntityId, SampleEntity)]
          go generatedSoFar () = do
            entity <- arbitrary
            space <- elements (HS.toList spaces)
            let entsAndVersions :: HashMap EntityId (HashSet VersionId) -- nonempty HashSet
                entsAndVersions = HM.fromList $
                  map
                    (\(k, (_, vs, _)) -> (k, HS.fromList $ map (\(vId,_,_) -> vId) (NE.toList vs)))
                    generatedSoFar
            versions <- fmap NE.fromList . listOf1 $ do
              (vId :: VersionId) <- arbitrary
              refs <- do
                refsCount <- chooseInt (0, 5)
                if null entsAndVersions then pure mempty else do
                  fmap HS.fromList . replicateM refsCount $ do
                    (_, versionsToRefTo) <- elements $ HM.toList entsAndVersions
                    elements $ HS.toList versionsToRefTo
              subs <- do
                subsCount <- chooseInt (0, 5)
                if null entsAndVersions then pure mempty else do
                  fmap HS.fromList . replicateM subsCount $ do
                    elements $ HM.keys entsAndVersions
              pure (vId, refs, subs)
            fork <- do
              shouldBeForked <- (== 0) <$> chooseInt (0, 10)
              if not shouldBeForked then pure Nothing else do
                if null entsAndVersions then pure Nothing else do
                  (_, versionsToForkFrom) <- elements $ HM.toList entsAndVersions
                  Just <$> elements (HS.toList versionsToForkFrom)
            pure $ (entity, (space, versions, fork)) : generatedSoFar
      count <- arbitrary
      reverse <$> foldlM go [] (replicate count ())
    (groupsStructure :: SampleGroupTree ()) <- arbitrary
    let fromSpaces :: Gen (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission) -- Spaces & entities rights 
        fromSpaces =
          let relevantPermissions :: Arbitrary a => Gen (HashMap SpaceId a)
              relevantPermissions =
                if null spaces
                then pure mempty
                else fmap HM.fromList . listOf $ (,) <$> elements (HS.toList spaces) <*> arbitrary
          in  (,) <$> relevantPermissions <*> relevantPermissions
    groups <- sequenceA $ fmap (const fromSpaces) groupsStructure -- replaces () with fromSpaces
    let allGroups :: HashSet GroupId
        allGroups =
          let go (SampleGroupTree g _ _ _ _ gs) = HS.insert g . HS.unions $ fmap go gs
          in  go groups
    actors <- fmap HM.fromList . listOf $ do
      aId <- arbitrary
      gs <- if null allGroups
            then pure mempty
            else fmap HS.fromList . listOf . elements $ HS.toList allGroups
      pure (aId, gs)
    pure SampleStore
      { sampleSpaces = spaces
      , sampleEntities = entities
      , sampleActors = actors
      , sampleGroups = groups
      }
  shrink (SampleStore spaces entities actors groups) =
    [ SampleStore 
        { sampleSpaces = spaces
        , sampleEntities = entities
        , sampleActors = fmap (HS.filter (\g -> g == current groups)) actors
        , sampleGroups = groups { children = [] }
        }
    ]


loadSample :: SampleStore -> Shared
loadSample SampleStore{..} = flip execState (loadSampleTree sampleGroups) $ do
  for_ (HS.toList sampleSpaces) $ \sId -> do
    unsafeStoreSpace sId
  for_ sampleEntities $ \(eId, (sId, vIds, mFork)) -> do
    let ((vId, refIds, subIds), vIdsTail) = uncons vIds
    case mFork of
      Nothing -> do
        eWorked <- unsafeStoreEntity eId sId vId genesisVersion
        case eWorked of
          Left e -> error $ "Error during store genesis entity " <> show e
          Right () -> pure ()
      Just fork -> do
        eWorked <- unsafeStoreEntity eId sId vId (flip forkVersion fork)
        case eWorked of
          Left e -> error $ "Error during store fork entity " <> show e
          Right () -> pure ()
    eWorked <- unsafeUpdateVersionReferences vId refIds
    case eWorked of
      Left e -> error $ "Error during version references update " <> show e
      Right () -> pure ()
    eWorked <- unsafeUpdateVersionSubscriptions vId subIds
    case eWorked of
      Left e -> error $ "Error during version subscriptions update " <> show e
      Right () -> pure ()
    case vIdsTail of
      Nothing -> pure ()
      Just vIdsTail -> void . (\f -> foldlM f vId vIdsTail) $ \prevVId (vId, refIds, subIds) -> do
        eWorked <- unsafeStoreVersion eId vId (flip forkVersion prevVId)
        case eWorked of
          Left e -> error $ "Error during store version " <> show e
          Right () -> do
            eWorked <- unsafeUpdateVersionReferences vId refIds
            case eWorked of
              Left e -> error $ "Error during version references update " <> show e
              Right () -> do
                eWorked <- unsafeUpdateVersionSubscriptions vId subIds
                case eWorked of
                  Left e -> error $ "Error during version subscriptions update " <> show e
                  Right () -> pure vId
  let loadPermissions :: SampleGroupTree (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission)
                      -> State Shared ()
      loadPermissions SampleGroupTree{current, auxPerGroup = (spacesPerms, entityPerms), children} = do
        for_ (HM.toList spacesPerms) $ \(space, permission) -> do
          unsafeAdjustSpacePermission (const (Just permission)) current space
        for_ (HM.toList entityPerms) $ \(space, permission) -> do
          unsafeAdjustEntityPermission (const permission) current space
        traverse_ loadPermissions children
  loadPermissions sampleGroups
  for_ (HM.toList sampleActors) $ \(aId, gs) -> do
    unsafeStoreActor aId
    for_ (HS.toList gs) $ \gId ->
      unsafeAddMember gId aId


storeSample :: SampleStore -> ActorId -> GroupId -> Shared
storeSample SampleStore{..} adminActor adminGroup =
  flip execState (storeSampleTree sampleGroups adminActor adminGroup) $ do
    for_ (HS.toList sampleSpaces) $ \sId -> do
      succeeded <- storeSpace adminActor sId
      unless succeeded $ do
        s <- get
        error $ "Failed to store space " <> show sId <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setEntityPermission adminActor Update adminGroup sId 
      unless succeeded $ do
        s <- get
        error $ "Failed to set entity permissions " <> show sId <> " - " <> LT.unpack (pShowNoColor s)
          
    for_ sampleEntities $ \(eId, (sId, vIds, mFork)) -> do -- FIXME use a State to keep retrying on fork failure? Or just sort the list?
      let ((vId, refIds, subIds), vIdsTail) = uncons vIds
      case mFork of
        Nothing -> do
          succeeded <- storeEntity adminActor eId sId vId
          unless succeeded $ do
            s <- get
            error $ "Failed to store entity " <> show (eId, sId, vId) <> " - " <> LT.unpack (pShowNoColor s)
        Just fork -> do
          mE <- storeForkedEntity adminActor eId sId vId fork
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $ "Failed to store forked entity " <> show (mE, eId, sId, vId, fork) <> " - " <> LT.unpack (pShowNoColor s)
      mE <- updateVersionReferences adminActor vId refIds
      case mE of
        Just (Right ()) -> pure ()
        _ -> do
          s <- get
          error $ "Failed to store version references " <> show (mE, eId, vId) <> " - " <> LT.unpack (pShowNoColor s)
      mE <- updateVersionSubscriptions adminActor vId subIds
      case mE of
        Just (Right ()) -> pure ()
        _ -> do
          s <- get
          error $ "Failed to store version subscriptions " <> show (mE, eId, vId) <> " - " <> LT.unpack (pShowNoColor s)
      case vIdsTail of
        Nothing -> pure ()
        Just vIdsTail -> for_ vIdsTail $ \(vId, refIds, subIds) -> do
          mE <- storeNextVersion adminActor eId vId
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $ "Failed to store version " <> show (mE, eId, vId) <> " - " <> LT.unpack (pShowNoColor s)
          mE <- updateVersionReferences adminActor vId refIds
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $ "Failed to store version references " <> show (mE, eId, vId) <> " - " <> LT.unpack (pShowNoColor s)
          mE <- updateVersionSubscriptions adminActor vId subIds
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $ "Failed to store version subscriptions " <> show (mE, eId, vId) <> " - " <> LT.unpack (pShowNoColor s)
    let loadPermissions :: SampleGroupTree (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission)
                        -> State Shared ()
        loadPermissions SampleGroupTree{current, auxPerGroup = (spacesPerms, entityPerms), children} = do
          for_ (HM.toList spacesPerms) $ \(space, permission) -> do
            succeeded <- setSpacePermission adminActor (Just permission) current space
            unless succeeded $ do
              s <- get
              error $ "Failed to set space permission " <> show (current, space) <> " - " <> LT.unpack (pShowNoColor s)
          for_ (HM.toList entityPerms) $ \(space, permission) -> do
            succeeded <- setEntityPermission adminActor permission current space
            unless succeeded $ do
              s <- get
              error $ "Failed to set entity permission " <> show (current, space) <> " - " <> LT.unpack (pShowNoColor s)
          traverse_ loadPermissions children
    loadPermissions sampleGroups
    for_ (HM.toList sampleActors) $ \(aId, gs) -> do
      succeeded <- storeActor adminActor aId
      unless succeeded $ do
        s <- get
        error $ "Failed to store actor " <> show aId <> " - " <> LT.unpack (pShowNoColor s)
      for_ (HS.toList gs) $ \gId -> do
        succeeded <- addMember adminActor gId aId
        unless succeeded $ do
          s <- get
          error $ "Failed to add member " <> show (aId, gId) <> " - " <> LT.unpack (pShowNoColor s)
