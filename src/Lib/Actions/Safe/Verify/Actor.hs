{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Actions.Safe.Verify.Actor where

import Lib.Actions.Safe.Verify.Utils (canDo)
import Lib.Types.Id (ActorId)
import Lib.Types.Permission (
  CollectionPermission (..),
 )
import Lib.Types.Store (Shared)
import Lib.Types.Store.Tabulation.Group (forRecruiter)

import Control.Lens ((^.))
import Control.Monad.Extra (orM)
import Control.Monad.State (MonadState)

canReadActor :: (MonadState Shared m) => ActorId -> m Bool
canReadActor reader =
  canDo (\t -> t ^. forRecruiter) reader Read

canCreateActor :: (MonadState Shared m) => ActorId -> m Bool
canCreateActor creater =
  canDo (\t -> t ^. forRecruiter) creater Create

canUpdateActor :: (MonadState Shared m) => ActorId -> ActorId -> m Bool
canUpdateActor updater aId =
  orM
    [ pure (updater == aId)
    , canDo (\t -> t ^. forRecruiter) updater Update
    ]

canDeleteActor :: (MonadState Shared m) => ActorId -> ActorId -> m Bool
canDeleteActor deleter aId =
  orM
    [ pure (deleter == aId)
    , canDo (\t -> t ^. forRecruiter) deleter Delete
    ]
