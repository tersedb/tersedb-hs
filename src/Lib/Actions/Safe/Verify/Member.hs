{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Actions.Safe.Verify.Member where

import Lib.Actions.Safe.Verify.Group (canReadGroup)
import Lib.Actions.Safe.Verify.Utils (canDo, withCollectionPermission)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (CollectionPermission (..), collectionPermission)
import Lib.Types.Store (Shared)
import Lib.Types.Store.Tabulation.Group (forMembers, forOrganization)

import Control.Lens (at, non, (^.))
import Control.Monad.Extra (andM, orM)
import Control.Monad.State (MonadState)

canReadMember :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canReadMember reader gId =
  andM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) reader Read
    , canReadGroup reader gId
    ]

canCreateMember :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canCreateMember creater gId =
  andM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) creater Create
    , canReadGroup creater gId
    ]

{- | Semantically holds no purpose; there is no occasion in which a membership would be updated
as its not an element of any collection -- there is no @MemberId@.
-}
canUpdateMember :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canUpdateMember updater gId =
  andM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) updater Update
    , canReadGroup updater gId
    ]

canDeleteMember :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canDeleteMember deleter gId =
  andM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) deleter Delete
    , canReadGroup deleter gId
    ]

hasMemberPermission
  :: (MonadState Shared m) => ActorId -> GroupId -> CollectionPermission -> m Bool
hasMemberPermission aId gId p =
  andM
    [ canReadGroup aId gId
    , orM
        [ canDo (\t -> t ^. forMembers . at gId . non Blind) aId p
        , canDo (\t -> t ^. forOrganization . collectionPermission) aId Update
        ]
    ]
