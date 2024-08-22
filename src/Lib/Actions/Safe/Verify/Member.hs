module Lib.Actions.Safe.Verify.Member (
  anyCanReadMember,
  anyCanCreateMember,
  anyCanUpdateMember,
  anyCanDeleteMember,
  hasMemberPermission,
) where

import Control.Lens (at, non, (^.))
import Control.Monad.Extra (andM, anyM, orM)
import Control.Monad.State (MonadState)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Lib.Actions.Safe.Verify.Group (canReadGroup)
import Lib.Actions.Safe.Verify.Utils (canDo)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (CollectionPermission (..), collectionPermission)
import Lib.Types.Store (Shared)
import Lib.Types.Store.Tabulation.Group (forMembers, forOrganization)

canReadMember :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canReadMember reader gId =
  andM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) reader Read
    , canReadGroup reader gId
    ]

anyCanReadMember :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanReadMember readers gId =
  anyM (`canReadMember` gId) (NE.toList readers)

canCreateMember :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canCreateMember creater gId =
  andM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) creater Create
    , canReadGroup creater gId
    ]

anyCanCreateMember
  :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanCreateMember creaters gId =
  anyM (`canCreateMember` gId) (NE.toList creaters)

{- | Semantically holds no purpose; there is no occasion in which a membership would be updated
as its not an element of any collection -- there is no @MemberId@.
-}
canUpdateMember :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canUpdateMember updater gId =
  andM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) updater Update
    , canReadGroup updater gId
    ]

anyCanUpdateMember
  :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanUpdateMember updaters gId =
  anyM (`canUpdateMember` gId) (NE.toList updaters)

canDeleteMember :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canDeleteMember deleter gId =
  andM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) deleter Delete
    , canReadGroup deleter gId
    ]

anyCanDeleteMember
  :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanDeleteMember deleters gId =
  anyM (`canDeleteMember` gId) (NE.toList deleters)

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
