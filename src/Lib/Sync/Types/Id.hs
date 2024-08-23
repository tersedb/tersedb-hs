{-
TerseDB - Entity Management System
Copyright (C) 2024  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

You can reach me at athan.clark@gmail.com.
-}

module Lib.Sync.Types.Id (GroupId, ActorId, SpaceId, EntityId, VersionId) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import System.Random.Stateful (Uniform (uniformM))
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Instances ()

-- | 12 byte random number
newtype Id = Id
  { getId :: Int
  }
  deriving (Show, Read, ToJSON, FromJSON, Eq, Ord, Hashable, Generic)

instance Uniform Id where
  uniformM gen = Id <$> uniformM gen

instance Arbitrary Id where
  arbitrary = Id <$> chooseInt (minBound, maxBound)

newtype GroupId = GroupId
  { getGroupId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform GroupId where
  uniformM gen = GroupId <$> uniformM gen

newtype ActorId = ActorId
  { getActorId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform ActorId where
  uniformM gen = ActorId <$> uniformM gen

newtype SpaceId = SpaceId
  { getSpaceId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform SpaceId where
  uniformM gen = SpaceId <$> uniformM gen

newtype EntityId = EntityId
  { getEntityId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform EntityId where
  uniformM gen = EntityId <$> uniformM gen

newtype VersionId = VersionId
  { getVersionId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform VersionId where
  uniformM gen = VersionId <$> uniformM gen
