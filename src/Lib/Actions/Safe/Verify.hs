{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , DerivingVia
  , DataKinds
  , DeriveGeneric
  , RankNTypes
  , TemplateHaskell
  , FlexibleContexts
  #-}

module Lib.Actions.Safe.Verify where

import Lib.Types.Id (GroupId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , collectionPermission
  )
import Lib.Types.Store
  ( Shared
  , store
  , temp
  , toActors
  , toTabulatedGroups
  )
import Lib.Types.Store.Tabulation.Group
  ( TabulatedPermissionsForGroup (..)
  , forOrganization
  , forGroups
  )

import qualified Data.HashSet as HS
import Control.Lens ((^.), at, non)
import Control.Monad.State (MonadState (get))
import Control.Monad.Extra (orM, when)

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

canUpdateGroup :: MonadState Shared m => ActorId -> GroupId -> m Bool
canUpdateGroup creator gId = orM
  [ canDo (\t -> t ^. forGroups . at gId . non Blind) creator Update
  , canDo (\t -> t ^. forOrganization . collectionPermission) creator Update
  ]

conditionally :: Applicative m => m () -> Bool -> m Bool
conditionally f t = t <$ when t f

