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
import Lib.Actions.Unsafe.Update.Group
  ( unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  )
import Lib.Actions.Safe.Store
  ( storeActor
  , storeSpace
  , storeEntity
  , storeForkedEntity
  , storeVersion
  , addMember
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
import Control.Monad (void)
import Control.Monad.Extra (unless)
import Control.Monad.State (State, execState, modify, get)
import Control.Lens ((%~), (.~), (^.), at, _1, _2)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Gen
  , listOf
  , elements
  , chooseInt
  )


data SampleStore = SampleStore
  { sampleSpaces :: HashSet SpaceId
  , sampleEntities :: HashMap EntityId (SpaceId, NonEmpty VersionId, Maybe VersionId) -- "should be forked"
  , sampleActors :: HashMap ActorId (HashSet GroupId)
  , sampleGroups :: SampleGroupTree (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission) -- spaces & entities
  } deriving (Eq, Show, Read)


sortSampleEntities :: HashMap EntityId (SpaceId, NonEmpty VersionId, Maybe VersionId)
                   -> [(EntityId, (SpaceId, NonEmpty VersionId, Maybe VersionId))]
sortSampleEntities sampleEntities =
  let es = reverse $ execState backtrackLinks initialLinks ^. _2
      rebuild eId = (eId, fromJust $ HM.lookup eId sampleEntities)
  in  concatMap (\es' -> map rebuild (HS.toList es')) es
  where
    forksOf :: HashMap VersionId (HashSet EntityId)
    forksOf = foldr (HM.unionWith HS.union) mempty
             . mapMaybe
                (\(eId, (_,_,mFork)) -> fmap (\vId -> HM.singleton vId (HS.singleton eId)) mFork)
             $ HM.toList sampleEntities
    backtrackLinks :: State (HashSet VersionId, [HashSet EntityId]) ()
    backtrackLinks = do
      (versionsToGet, xs) <- get
      if null versionsToGet then pure () else do
        let go :: HashSet VersionId
               -> VersionId
               -> State (HashSet VersionId, [HashSet EntityId]) (HashSet VersionId)
            go nextVs vChildId = case HM.lookup vChildId forksOf of
              Nothing -> pure nextVs
              Just es -> do
                modify $ _2 %~ (es:)
                let go' eId nextVs' =
                      let (sId, vIds, _mFork) = fromJust $ HM.lookup eId sampleEntities
                      in  foldr HS.insert nextVs' vIds
                pure $ foldr go' nextVs $ HS.toList es
        nextVersionsToGet <- foldlM go mempty (HS.toList versionsToGet)
        modify $ _1 .~ nextVersionsToGet
        backtrackLinks
    initialLinks :: (HashSet VersionId, [HashSet EntityId])
    initialLinks = 
      ( HS.fromList $ concatMap (\(_,(_,vs,_)) -> NE.toList vs) noChildren
      , [HS.fromList $ map fst noChildren]
      )
      where
        noChildren :: [(EntityId, (SpaceId, NonEmpty VersionId, Maybe VersionId))]
        noChildren = filter (\(_,(_,_,mFork)) -> isNothing mFork) $ HM.toList sampleEntities


instance Arbitrary SampleStore where
  arbitrary = do
    spaces <- arbitrary
    -- FIXME sample entityId's from set
    entities <- if null spaces then pure mempty else do
      let go generatedSoFar () = do
            entity <- arbitrary
            space <- elements (HS.toList spaces)
            versions <- arbitrary
            fork <- do
              shouldBeForked <- (== 0) <$> chooseInt (0, 10)
              if not shouldBeForked then pure Nothing else do
                let entsAndVersions :: HashMap EntityId (HashSet VersionId) -- nonempty HashSet, too
                    entsAndVersions = HM.fromList $
                      map
                        (\(k, (_, vs, _)) -> (k, HS.fromList (NE.toList vs)))
                        generatedSoFar
                if null entsAndVersions then pure Nothing else do
                  (entToForkFrom :: EntityId) <- elements $ HM.keys entsAndVersions
                  versionToForkFrom <- elements . HS.toList . fromJust $ entsAndVersions ^. at entToForkFrom
                  pure $ Just versionToForkFrom
            pure $ (entity, (space, versions, fork)) : generatedSoFar
      count <- arbitrary
      fmap HM.fromList $ foldlM go [] (replicate count ())
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
  for_ (sortSampleEntities sampleEntities) $ \(eId, (sId, vIds, mFork)) -> do
    let (vId, vIdsTail) = uncons vIds
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
    case vIdsTail of
      Nothing -> pure ()
      Just vIdsTail -> void . (\f -> foldlM f vId vIdsTail) $ \prevVId vId -> do
        eWorked <- unsafeStoreVersion eId vId (flip forkVersion prevVId)
        case eWorked of
          Left e -> error $ "Error during store version " <> show e
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
          
    for_ (sortSampleEntities sampleEntities) $ \(eId, (sId, vIds, mFork)) -> do -- FIXME use a State to keep retrying on fork failure? Or just sort the list?
      let (vId, vIdsTail) = uncons vIds
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
      case vIdsTail of
        Nothing -> pure ()
        Just vIdsTail -> for_ vIdsTail $ \vId -> do
          mE <- storeVersion adminActor eId vId
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $ "Failed to store version " <> show (mE, eId, vId) <> " - " <> LT.unpack (pShowNoColor s)
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
