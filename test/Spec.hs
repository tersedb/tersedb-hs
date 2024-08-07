{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , RecordWildCards
  , FlexibleInstances
  , NamedFieldPuns
  #-}

import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission (Permission)
import Lib.Types.Store
  ( Store
  , TabulatedPermissionsForGroup
  , emptyStore
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
import Lib.Types.Group
  ( storeGroup
  , storeSpace
  , storeEntity
  , storeVersion
  , linkGroups
  , unlinkGroups
  , resetTabulation
  , initTabulatedPermissionsForGroup
  , adjustUniversePermission
  , adjustSpacePermission
  , adjustEntityPermission
  )

import qualified Data.Aeson as Aeson
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Foldable (traverse_, for_, foldlM)
import qualified Data.Text.Lazy as LT
import Data.Maybe (isNothing, fromJust)
import Data.List.NonEmpty (NonEmpty, uncons)
import Text.Read (readMaybe)
import Text.Pretty.Simple (pShowNoColor)
import Topograph (pairs)
import Control.Monad (void)
import Control.Monad.State (State, execState, modify, put, get)
import Control.Lens ((%~), (.~), (&), (^.))
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
  describe "Permission" $ do
    it "has a lower bound" $
      property $ \(x :: Permission) -> x `shouldSatisfy` (>= minBound)
    it "has an upper bound" $
      property $ \(x :: Permission) -> x `shouldSatisfy` (<= maxBound)
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
                let go (SampleGroupTree x _ _ xs) = do
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
            let newStore = execState (unlinkGroups from to) store
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

data SampleGroupTree a = SampleGroupTree
  { current :: GroupId
  , univ :: Permission
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
  foldr f acc (SampleGroupTree _ _ x xs) =
    foldr (\x' acc' -> foldr f acc' x') (f x acc) xs
instance Traversable SampleGroupTree where
  sequenceA SampleGroupTree{..} =
    (SampleGroupTree current univ)
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
        pure (SampleGroupTree current univ () children)
  shrink (SampleGroupTree current univ () children) =
    [ SampleGroupTree current univ () []
    ] ++
    children ++
    [ SampleGroupTree current univ' () children'
    | (univ', children') <- shrink (univ, children) ]


data SampleStore = SampleStore
  { sampleSpaces :: HashSet SpaceId
  , sampleEntities :: HashMap EntityId (SpaceId, NonEmpty VersionId)
  , sampleGroups :: SampleGroupTree (HashMap SpaceId Permission, HashMap SpaceId Permission)
  } deriving (Eq, Show, Read)

instance Arbitrary SampleStore where
  arbitrary = do
    spaces <- resize 50 arbitrary
    -- FIXME sample entityId's from set
    entities <- resize 100 $ if null spaces then pure mempty else do
      fmap HM.fromList . listOf $ do
        entity <- arbitrary
        space <- elements (HS.toList spaces)
        versions <- arbitrary
        pure (entity, (space, versions))
    (groupsStructure :: SampleGroupTree ()) <- arbitrary
    let fromSpaces :: Gen (HashMap SpaceId Permission, HashMap SpaceId Permission)
        fromSpaces =
          let relevantPermissions =
                if null spaces
                then pure mempty
                else fmap HM.fromList . listOf $ (,) <$> elements (HS.toList spaces) <*> arbitrary
          in  (,) <$> relevantPermissions <*> relevantPermissions
    groups <- sequenceA $ fmap (const fromSpaces) groupsStructure
    pure SampleStore
      { sampleSpaces = spaces
      , sampleEntities = entities
      , sampleGroups = groups
      }


loadSample :: SampleStore -> Store
loadSample SampleStore{..} = flip execState (loadSampleTree sampleGroups) $ do
  for_ (HS.toList sampleSpaces) $ \sId -> do
    storeSpace sId
  for_ (HM.toList sampleEntities) $ \(eId, (sId, vIds)) -> do
    let (vId, vIdsTail) = uncons vIds
    storeEntity eId sId vId genesisVersion
    case vIdsTail of
      Nothing -> pure ()
      Just vIdsTail -> void . (\f -> foldlM f vId vIdsTail) $ \prevVId vId -> do
        storeVersion eId vId (\eId -> forkVersion eId vId)
        pure vId
  let loadPermissions SampleGroupTree{current, auxPerGroup = (spacesPerms, entityPerms), children} = do
        for_ (HM.toList spacesPerms) $ \(space, permission) -> do
          adjustSpacePermission (const permission) current space
        for_ (HM.toList entityPerms) $ \(space, permission) -> do
          adjustEntityPermission (const permission) current space
        traverse_ loadPermissions children
  loadPermissions sampleGroups


loadSampleTree :: SampleGroupTree a -> Store
loadSampleTree xs = flip execState emptyStore $ do
  modify $ toGroups . roots %~ HS.insert (current xs)
  go xs
  where
    setupNode current univ = do
      modify $ toGroups . nodes %~ HM.alter (\mg -> if isNothing mg then Just emptyGroup else mg) current
      adjustUniversePermission (const univ) current
      currentTab <- initTabulatedPermissionsForGroup current
      -- ensures that singleton maps still have loaded tabs
      modify $ toTabulatedPermissions %~ HM.alter (\mt -> if isNothing mt then Just currentTab else mt) current

    go :: SampleGroupTree a -> State Store ()
    go SampleGroupTree{..} = do
      setupNode current univ
      -- loads all nodes
      let linkChild (SampleGroupTree child univChild _ _) = do
            setupNode child univChild
            mE <- linkGroups current child
            case mE of
              Left e -> do
                s <- get
                error $ "Error detected " <> show e <> " - store: " <> LT.unpack (pShowNoColor s)
              Right _ -> pure ()
      traverse_ linkChild children
      traverse_ go children

loadSampleTreeNoTab :: SampleGroupTree a -> Store
loadSampleTreeNoTab xs = flip execState emptyStore $ do
  modify $ toGroups . roots %~ HS.insert (current xs)
  go xs
  where
    addLink :: GroupId -> GroupId -> State Store ()
    addLink from to = do
      let adjustGroups groups = groups
            & edges %~ HS.insert (from, to)
            & outs %~ (HS.insert to . HS.delete from)
            & roots %~ HS.delete to
            & nodes %~ (HM.adjust (next %~ HS.insert to) from . HM.adjust (prev .~ Just from) to)
      modify (toGroups %~ adjustGroups)

    setupNode current univ = do
      modify $ toGroups . nodes %~ HM.alter (\mg -> if isNothing mg then Just emptyGroup else mg) current
      adjustUniversePermission (const univ) current

    go :: SampleGroupTree a -> State Store ()
    go SampleGroupTree{..} = do
      setupNode current univ
      let linkChild (SampleGroupTree child univChild _ _) = do
            setupNode child univChild
            addLink current child
      traverse_ linkChild children
      traverse_ go children

loadCycle :: [GroupId] -> Groups
loadCycle gs = execState go emptyGroups
  where
    addGroup :: GroupId -> State Groups ()
    addGroup gId = modify (nodes %~ HM.insert gId emptyGroup)

    addSingleLink :: (GroupId, GroupId) -> State Groups ()
    addSingleLink (from, to) =
      modify $
        nodes %~ (HM.adjust (next .~ HS.singleton to) from . HM.adjust (prev .~ Just from) to)

    go :: State Groups ()
    go = do
      void $ traverse addGroup gs
      if length gs <= 1
      then pure ()
      else do
        modify (roots .~ HS.singleton (gs !! 0))
        void $ traverse addSingleLink (pairs gs)
        addSingleLink (gs !! (length gs - 1), gs !! 0)
