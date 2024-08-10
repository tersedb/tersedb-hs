{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , RecordWildCards
  , FlexibleInstances
  , NamedFieldPuns
  #-}

import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (Adjust)
  )
import Lib.Types.Store
  ( Store
  , TabulatedPermissionsForGroup
  , toGroups
  , toSpaces
  , toEntities
  , toTabulatedPermissions
  , hasLessOrEqualPermissionsTo
  )
import Lib.Types.Store.Groups
  ( Groups
  , nodes
  , edges
  , roots
  , outs
  , next
  , prev
  , hasCycle
  , emptyGroup
  , emptyGroups
  )
import Lib.Types.Store.Space (Space (..), entities)
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Version (genesisVersion, forkVersion)
import Lib.Actions.Unsafe
  ( unsafeEmptyStore
  , unsafeStoreGroup
  , unsafeStoreActor
  , unsafeStoreActor
  , unsafeStoreSpace
  , unsafeStoreEntity
  , unsafeStoreVersion
  , unsafeAddMember
  , unsafeLinkGroups
  , unsafeUnlinkGroups
  , unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  , unsafeAdjustGroupPermission
  , unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  )
import Lib.Actions.Safe
  ( emptyStore
  , storeGroup
  , storeActor
  , storeSpace
  , storeEntity
  , storeForkedEntity
  , storeVersion
  , addMember
  , setUniversePermission
  , setOrganizationPermission
  , setRecruiterPermission
  , setGroupPermission
  , setSpacePermission
  , setEntityPermission
  )
import Lib.Actions.Tabulation
  ( resetTabulation
  , initTabulatedPermissionsForGroup
  , updateTabulationStartingAt
  )

import qualified Data.Aeson as Aeson
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Foldable (traverse_, for_, foldlM)
import qualified Data.Text.Lazy as LT
import Data.Maybe (isNothing, fromJust, fromMaybe, mapMaybe)
import Data.List.NonEmpty (NonEmpty, uncons)
import qualified Data.List.NonEmpty as NE
import Text.Read (readMaybe)
import Text.Pretty.Simple (pShowNoColor)
import Topograph (pairs)
import Control.Monad (void)
import Control.Monad.Extra (when, unless)
import Control.Monad.State (State, execState, modify, put, get)
import Control.Lens ((%~), (.~), (&), (^.), at, ix, _1, _2)
import Test.Syd (sydTest, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Gen
  , property
  , getSize
  , resize
  , listOf
  , forAll
  , elements
  , chooseInt
  )

import Debug.Trace (traceShow)


main :: IO ()
main = sydTest $ do
  describe "Id" $ do
    describe "parsing" $ do
      it "show / read should be isomorphic" $
        property $ \(id :: GroupId) ->
          readMaybe (show id) `shouldBe` Just id
      it "aeson should be isomorphic" $
        property $ \(id :: GroupId) ->
          Aeson.decode (Aeson.encode id) `shouldBe` Just id
  describe "CollectionPermission" $ do
    it "has a lower bound" $
      property $ \(x :: CollectionPermission) -> x `shouldSatisfy` (>= minBound)
    it "has an upper bound" $
      property $ \(x :: CollectionPermission) -> x `shouldSatisfy` (<= maxBound)
    it "is a semigroup" $
      property $ \(x :: CollectionPermission) y z -> (x <> y) <> z `shouldBe` x <> (y <> z)
    it "is a monoid left" $
      property $ \(x :: CollectionPermission) -> x <> mempty `shouldBe` x
    it "is a monoid right" $
      property $ \(x :: CollectionPermission) -> mempty <> x `shouldBe` x
    it "is commutative" $
      property $ \(x :: CollectionPermission) y -> y <> x `shouldBe` x <> y
-- TODO tabulating should be idempotent
  describe "TabulatedPermissionsForGroup" $ do
    it "is a semigroup" $
      property $ \(x :: TabulatedPermissionsForGroup) y z ->
        (x <> (y <> z)) `shouldBe` ((x <> y) <> z)
    it "is a monoid left" $
      property $ \(x :: TabulatedPermissionsForGroup) ->
        (x <> mempty) `shouldBe` x
    it "is a monoid right" $
      property $ \(x :: TabulatedPermissionsForGroup) ->
        (mempty <> x) `shouldBe` x
    it "commutes" $
      property $ \(x :: TabulatedPermissionsForGroup) y ->
        (x <> y) `shouldBe` (y <> x)
    it "union of two is superset of left" $
      property $ \(x :: TabulatedPermissionsForGroup) y ->
        (x, x <> y) `shouldSatisfy` (uncurry hasLessOrEqualPermissionsTo)
  describe "Groups" $ do
    it "cycles are detected" $
      property $ \(xs :: [GroupId]) ->
        if length xs <= 1 then True `shouldBe` True
        else hasCycle (loadCycle xs) `shouldBe` Just (xs !! 0 : reverse xs)
  describe "Store" $ do
    let testPermissionInheritance root cs store =
          let tabs = store ^. toTabulatedPermissions
              descendants =
                let go (SampleGroupTree x _ _ _ _ xs) = do
                      modify (x:)
                      traverse_ go xs
                in  execState (traverse_ go cs) []
          in  if null descendants
              then property True
              else forAll (elements descendants) $ \descendant ->
                    case (HM.lookup root tabs, HM.lookup descendant tabs) of
                      (Just r, Just d) ->
                        (store, r, d) `shouldSatisfy` (\_ -> r `hasLessOrEqualPermissionsTo` d)
                      tabs -> error $ "Tab wasn't found " <> show tabs
    describe "Group Inheritance" $ do
      it "tabulating while linking is the same as tabulating after linking" $
        property $ \(xs :: SampleGroupTree ()) ->
          loadSampleTree xs `shouldBe` execState resetTabulation (loadSampleTreeNoTab xs)
      it "resetting tabulation is idempotent" $
        property $ \(xs :: SampleGroupTree ()) ->
          execState (resetTabulation >> resetTabulation) (loadSampleTreeNoTab xs)
            `shouldBe` execState resetTabulation (loadSampleTreeNoTab xs)
      it "unlinking causes disjoint trees" $
        property $ \(xs :: SampleGroupTree ()) ->
          let store = loadSampleTree xs
              createdEdges = store ^. toGroups . edges
          in if null createdEdges
          then property True
          else forAll (elements (HS.toList createdEdges)) $ \(from, to) ->
            let newStore = execState (unsafeUnlinkGroups from to) store
                newGroups = newStore ^. toGroups
                rootsWithoutTo = HS.delete to (newGroups ^. roots)
                descendants :: GroupId -> HashSet GroupId
                descendants gId =
                  let children = fromJust (HM.lookup gId (newGroups ^. nodes)) ^. next
                  in  HS.insert gId (HS.unions (map descendants (HS.toList children)))
            in  (newStore, from, to) `shouldSatisfy` (\_ ->
                  to `HS.member` (newStore ^. toGroups . roots)
                  && all
                    (\descendantOfTo ->
                       not . HS.member descendantOfTo . HS.unions . map descendants $ HS.toList rootsWithoutTo
                    )
                    (HS.toList (descendants to))
                )
          
      describe "total vs. incremental tabulation" $ do
        let testsPerStoreBuild buildStore = do
              it "all descendants are supersets of roots - only universal permission" $
                property $ \(xs :: SampleGroupTree ()) ->
                  testPermissionInheritance (current xs) (children xs) (buildStore xs)
        describe "total" . testsPerStoreBuild $ execState resetTabulation . loadSampleTreeNoTab
        describe "incremental" $ testsPerStoreBuild loadSampleTree
    describe "Full Store" $ do
      it "all descendants are supersets of roots - build all permissions" $
        property $ \(xs :: SampleStore) ->
          let groups = sampleGroups xs
          in  testPermissionInheritance (current groups) (children groups) (loadSample xs)
      it "all spaces are disjoint" $
        property $ \(xs :: SampleStore) ->
          let store = loadSample xs
          in  foldr HS.intersection mempty (fmap (^. entities) (store ^. toSpaces))
                `shouldBe` mempty
      it "all elements exist in their space" $
        property $ \(xs :: SampleStore) ->
          let store = loadSample xs
          in  if null (store ^. toEntities)
              then property True
              else forAll (elements . HM.toList $ store ^. toEntities) $ \(eId, e) ->
                    (store, eId, HM.lookup (e ^. space) (store ^. toSpaces)) `shouldSatisfy` (\(_,_,mSpace) ->
                      maybe False (\space -> HS.member eId (space ^. entities)) mSpace
                    )
      describe "Safe" $ do
        it "should be identical to unsafe" $
          property $ \(xs :: SampleStore, adminActor :: ActorId, adminGroup :: GroupId) ->
            let safeStore = storeSample xs adminActor adminGroup
                unsafeStore = flip execState (loadSample xs) $ do
                  -- setup admin
                  unsafeStoreGroup adminGroup
                  unsafeAdjustUniversePermission
                    (const $ CollectionPermissionWithExemption Delete True)
                    adminGroup
                  unsafeAdjustOrganizationPermission
                    (const $ CollectionPermissionWithExemption Delete True)
                    adminGroup
                  unsafeAdjustRecruiterPermission (const Delete) adminGroup
                  unsafeStoreActor adminActor
                  unsafeAddMember adminGroup adminActor
                  -- "backdate" the granting of group adjust rights to admin group
                  s <- get
                  for_ (HM.keys $ s ^. toGroups . nodes) $ \gId ->
                    unless (gId == adminGroup) $
                      unsafeAdjustGroupPermission (const (Just Adjust)) adminGroup gId
                  -- "backdate" the granting of space create rights to admin group
                  s <- get
                  for_ (HM.keys $ s ^. toSpaces) $ \sId ->
                    unsafeAdjustEntityPermission (const Create) adminGroup sId
                  resetTabulation
            in  safeStore `shouldBe` unsafeStore

data SampleGroupTree a = SampleGroupTree
  { current :: GroupId
  , univ :: CollectionPermissionWithExemption
  , org :: CollectionPermissionWithExemption
  , recr :: CollectionPermission
  , auxPerGroup :: a
  , children :: [SampleGroupTree a]
  } deriving (Eq, Show, Read)
instance Functor SampleGroupTree where
  fmap f x = x
    { auxPerGroup = f (auxPerGroup x)
    , children = map (fmap f) (children x)
    }
-- | Breadth-first approach
instance Foldable SampleGroupTree where
  foldr f acc (SampleGroupTree _ _ _ _ x xs) =
    foldr (\x' acc' -> foldr f acc' x') (f x acc) xs
instance Traversable SampleGroupTree where
  sequenceA SampleGroupTree{..} =
    (SampleGroupTree current univ org recr)
      <$> auxPerGroup
      <*> sequenceA (map sequenceA children)

instance Arbitrary (SampleGroupTree ()) where
  arbitrary = resize 10 go
    where
      go = do
        current <- arbitrary
        s <- getSize
        children <- resize (s `div` 2) (listOf go)
        univ <- arbitrary
        org <- arbitrary
        recr <- arbitrary
        pure (SampleGroupTree current univ org recr () children)
  shrink (SampleGroupTree current univ org recr () children) =
    [ SampleGroupTree current univ org recr () []
    ] ++
    children ++
    [ SampleGroupTree current univ' org' recr' () children'
    | (univ', org', recr', children') <- shrink (univ, org, recr, children) ]


data SampleStore = SampleStore
  { sampleSpaces :: HashSet SpaceId
  , sampleEntities :: HashMap EntityId (SpaceId, NonEmpty VersionId, Maybe VersionId) -- "should be forked"
  , sampleActors :: HashMap ActorId (HashSet GroupId)
  , sampleGroups :: SampleGroupTree (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission) -- spaces & entities
  } deriving (Eq, Show, Read)

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


loadSample :: SampleStore -> Store
loadSample SampleStore{..} = flip execState (loadSampleTree sampleGroups) $ do
  for_ (HS.toList sampleSpaces) $ \sId -> do
    unsafeStoreSpace sId
  for_ (HM.toList sampleEntities) $ \(eId, (sId, vIds, mFork)) -> do
    let (vId, vIdsTail) = uncons vIds
    case mFork of
      Nothing -> unsafeStoreEntity eId sId vId genesisVersion
      Just fork -> unsafeStoreEntity eId sId vId (flip forkVersion fork)
    case vIdsTail of
      Nothing -> pure ()
      Just vIdsTail -> void . (\f -> foldlM f vId vIdsTail) $ \prevVId vId -> do
        unsafeStoreVersion eId vId (flip forkVersion prevVId)
        pure vId
  let loadPermissions :: SampleGroupTree (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission)
                      -> State Store ()
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


storeSample :: SampleStore -> ActorId -> GroupId -> Store
storeSample SampleStore{..} adminActor adminGroup =
  flip execState (storeSampleTree sampleGroups adminActor adminGroup) $ do
    for_ (HS.toList sampleSpaces) $ \sId -> do
      succeeded <- storeSpace adminActor sId
      unless succeeded $ do
        s <- get
        error $ "Failed to store space " <> show sId <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setEntityPermission adminActor Create adminGroup sId 
      unless succeeded $ do
        s <- get
        error $ "Failed to set entity permissions " <> show sId <> " - " <> LT.unpack (pShowNoColor s)
    let forksOf :: HashMap VersionId (HashSet EntityId)
        forksOf = foldr (HM.unionWith HS.union) mempty
                 . mapMaybe
                    (\(eId, (_,_,mFork)) -> fmap (\vId -> HM.singleton vId (HS.singleton eId)) mFork)
                 $ HM.toList sampleEntities
        backtrackLinks :: State
          ( HashSet VersionId
          , [HashSet EntityId]
          ) ()
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
        initialLinks ::
          ( HashSet VersionId
          , [HashSet EntityId]
          )
        initialLinks = 
          ( HS.fromList $ concatMap (\(_,(_,vs,_)) -> NE.toList vs) noChildren
          , [HS.fromList $ map fst noChildren]
          )
          where
            noChildren :: [(EntityId, (SpaceId, NonEmpty VersionId, Maybe VersionId))]
            noChildren = filter (\(_,(_,_,mFork)) -> isNothing mFork) $ HM.toList sampleEntities
        sortedSampleEntities :: [(EntityId, (SpaceId, NonEmpty VersionId, Maybe VersionId))]
        sortedSampleEntities =
          let es = reverse $ execState backtrackLinks initialLinks ^. _2
              rebuild eId = (eId, fromJust $ HM.lookup eId sampleEntities)
          in  concatMap (\es' -> map rebuild (HS.toList es')) es
          
    for_ sortedSampleEntities $ \(eId, (sId, vIds, mFork)) -> do -- FIXME use a State to keep retrying on fork failure? Or just sort the list?
      let (vId, vIdsTail) = uncons vIds
      case mFork of
        Nothing -> do
          succeeded <- storeEntity adminActor eId sId vId
          unless succeeded $ do
            s <- get
            error $ "Failed to store entity " <> show (eId, sId, vId) <> " - " <> LT.unpack (pShowNoColor s)
        Just fork -> do
          eSucceeded <- storeForkedEntity adminActor eId sId vId fork
          case eSucceeded of
            Right succeeded ->
              unless succeeded $ do
                s <- get
                error $ "Failed to store forked entity " <> show (eId, sId, vId, fork) <> " - " <> LT.unpack (pShowNoColor s)
            Left e -> do
              s <- get
              error $ "Error storing forked entity " <> show (e, eId, sId, vId, fork) <> " - " <> LT.unpack (pShowNoColor s)
      case vIdsTail of
        Nothing -> pure ()
        Just vIdsTail -> for_ vIdsTail $ \vId -> do
          succeeded <- storeVersion adminActor eId vId
          unless succeeded $ do
            s <- get
            error $ "Failed to store version " <> show (eId, vId) <> " - " <> LT.unpack (pShowNoColor s)
    let loadPermissions :: SampleGroupTree (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission)
                        -> State Store ()
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


loadSampleTree :: SampleGroupTree a -> Store
loadSampleTree xs = flip execState unsafeEmptyStore $ do
  modify $ toGroups . roots . at (current xs) .~ Just ()
  go xs
  where
    setupNode current univ org recr = do
      modify $ toGroups . nodes . at current %~ Just . fromMaybe emptyGroup
      unsafeAdjustUniversePermission (const univ) current
      unsafeAdjustOrganizationPermission (const org) current
      unsafeAdjustRecruiterPermission (const recr) current
      currentTab <- initTabulatedPermissionsForGroup current
      -- ensures that singleton maps still have loaded tabs
      modify $ toTabulatedPermissions . at current %~ Just . fromMaybe currentTab

    go :: SampleGroupTree a -> State Store ()
    go SampleGroupTree{..} = do
      setupNode current univ org recr
      -- loads all nodes
      let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
            setupNode child univChild orgChild recrChild
            mE <- unsafeLinkGroups current child
            case mE of
              Left e -> do
                s <- get
                error $ "Error detected " <> show e <> " - store: " <> LT.unpack (pShowNoColor s)
              Right _ -> pure ()
      traverse_ linkChild children
      traverse_ go children


storeSampleTree :: SampleGroupTree a -> ActorId -> GroupId -> Store
storeSampleTree xs adminActor adminGroup = flip execState (emptyStore adminActor adminGroup) $ do
  -- modify $ toGroups . roots . at (current xs) .~ Just ()
  succeeded <- storeGroup adminActor (current xs)
  unless succeeded $ do
    s <- get
    error $ "Failed to store first group " <> show (current xs) <> " - " <> LT.unpack (pShowNoColor s)
  go xs
  where
    setupNode current univ org recr = do
      succeeded <- storeGroup adminActor current -- adds current as a root
      unless succeeded $ do
        s <- get
        error $ "Failed to store group " <> show current <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setGroupPermission adminActor (Just Adjust) adminGroup current
      unless succeeded $ do
        s <- get
        error $ "Failed to grant admin group permissions over current " <> show current <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setUniversePermission adminActor univ current
      unless succeeded $ do
        s <- get
        error $ "Failed to set universe permission " <> show (univ, current) <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setOrganizationPermission adminActor org current
      unless succeeded $ do
        s <- get
        error $ "Failed to set organization permission " <> show (org, current) <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setRecruiterPermission adminActor recr current
      unless succeeded $ do
        s <- get
        error $ "Failed to set recruiter permission " <> show (recr, current) <> " - " <> LT.unpack (pShowNoColor s)
      currentTab <- initTabulatedPermissionsForGroup current
      -- ensures that singleton maps still have loaded tabs
      modify $ toTabulatedPermissions . at current %~ Just . fromMaybe currentTab

    go :: SampleGroupTree a -> State Store ()
    go SampleGroupTree{..} = do
      setupNode current univ org recr
      -- loads all nodes
      let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
            setupNode child univChild orgChild recrChild
            mE <- unsafeLinkGroups current child -- FIXME
            case mE of
              Left e -> do
                s <- get
                error $ "Error detected " <> show e <> " - store: " <> LT.unpack (pShowNoColor s)
              Right _ -> pure ()
      traverse_ linkChild children
      traverse_ go children


loadSampleTreeNoTab :: SampleGroupTree a -> Store
loadSampleTreeNoTab xs = flip execState unsafeEmptyStore $ do
  modify $ toGroups . roots . at (current xs) .~ Just ()
  go xs
  where
    addLink :: GroupId -> GroupId -> State Store ()
    addLink from to = do
      let adjustGroups groups = groups
            & edges . at (from,to) .~ Just ()
            & outs . at to .~ Just ()
            & outs . at from .~ Nothing
            & roots . at to .~ Nothing
            & nodes . ix from . next . at to .~ Just ()
            & nodes . ix to . prev .~ Just from
      modify $ toGroups %~ adjustGroups

    setupNode current univ org recr = do
      modify $ toGroups . nodes . at current %~ Just . fromMaybe emptyGroup
      unsafeAdjustUniversePermission (const univ) current
      unsafeAdjustOrganizationPermission (const org) current
      unsafeAdjustRecruiterPermission (const recr) current

    go :: SampleGroupTree a -> State Store ()
    go SampleGroupTree{..} = do
      setupNode current univ org recr
      let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
            setupNode child univChild orgChild recrChild
            addLink current child
      traverse_ linkChild children
      traverse_ go children

loadCycle :: [GroupId] -> Groups
loadCycle gs = execState go emptyGroups
  where
    addGroup :: GroupId -> State Groups ()
    addGroup gId = modify $ nodes . at gId .~ Just emptyGroup

    addSingleLink :: (GroupId, GroupId) -> State Groups ()
    addSingleLink (from, to) = do
      modify $ nodes . ix from . next . at to .~ Just ()
      modify $ nodes . ix to . prev .~ Just from

    go :: State Groups ()
    go = do
      void $ traverse addGroup gs
      if length gs <= 1
      then pure ()
      else do
        modify $ roots . at (gs !! 0) .~ Just ()
        void $ traverse addSingleLink (pairs gs)
        addSingleLink (gs !! (length gs - 1), gs !! 0)
