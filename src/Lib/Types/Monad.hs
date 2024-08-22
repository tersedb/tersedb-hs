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

module Lib.Types.Monad where

import Lib.Types.Store (Shared, Store, store, temp)
import qualified Lib.Types.Store as Shared

import Control.Lens (Lens', over, set, view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (StateT, get, modify, put)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)

-- class MonadTerseTemp (m :: Type -> Type) where
--   type TempResultM m :: Type -> Type
--   runTempResult :: m (TempResultM m a) -> m a

-- class MonadTerseReadStore (m :: Type -> Type) where
--   runQuery :: QueryEvent event => event -> m (EventResult event)

-- class MonadTerseTemp m => MonadTerseReadTemp (m :: Type -> Type) where
--   type Temp m :: Type
--   askTemp :: Lens' (Temp m) a -> m (TempResultM m a)

-- class MonadTerseStateStore (m :: Type -> Type) where
--   runUpdate :: UpdateEvent event => event -> m (EventResult event)

-- class MonadTerseReadTemp m => MonadTerseStateTemp (m :: Type -> Type) where
--   putTemp :: Lens' (Temp m) a -> a -> m (TempResultM m ())
--   modifyTemp :: Lens' (Temp m) a -> (a -> a) -> m (TempResultM m ())

-- -- | Uses StateT for all mutations
-- newtype SimpleTerseM a = SimpleTerseM
--   { getSimpleTerseM :: StateT Shared IO a
--   } deriving (Functor, Applicative, Monad, MonadIO)

-- instance MonadTerseTemp SimpleTerseM where
--   type TempResultM SimpleTerseM = Identity
--   joinTempResult = fmap runIdentity

-- instance MonadTerseReadStore SimpleTerseM where
--   runQuery q = SimpleTerseM $ do
--     s <- get
--     acid <- liftIO (openMemoryState (s ^. store))
--     liftIO (query acid q)

-- instance MonadTerseReadTemp SimpleTerseM where
--   type Temp SimpleTerseM = Shared.Temp
--   getTemp proj = Identity . view (temp . proj) <$> SimpleTerseM get

-- instance MonadTerseStateStore SimpleTerseM where
--   runUpdate u = SimpleTerseM $ do
--     s <- get
--     acid <- liftIO (openMemoryState (s ^. store))
--     res <- liftIO (update acid u)
--     s' <- liftIO (query acid ask) -- FIXME can't use ask, cause it's not an event
--     modify (store .~ s')
--     pure res

-- instance MonadTerseStateTemp SimpleTerseM where
--   putTemp proj x = SimpleTerseM $ do
--     y <- get
--     Identity <$> put (set (temp . proj) x y)
--   modifyTemp proj f =
--     SimpleTerseM (Identity <$> modify (over (temp . proj) f))

-- -- | Uses AcidState
-- newtype ModerateTerseM a = ModerateTerseM
--   { getModerateTerseM :: ReaderT (AcidState Store) (StateT Temp IO) a
--   } deriving (Functor, Applicative, Monad, MonadIO)

-- instance MonadTerseStore ModerateTerseM where
--   type StoreResultM ModerateTerseM = Identity
--   joinStoreResult = fmap runIdentity

-- instance MonadTerseTemp ModerateTerseM where
--   type TempResultM ModerateTerseM = Identity
--   joinTempResult = fmap runIdentity

-- instance MonadTerseReadStore ModerateTerseM where
--   type Store ModerateTerseM = Shared.Store
--   getStore proj = Identity . view (store . proj) <$> ModerateTerseM get

-- instance MonadTerseReadTemp ModerateTerseM where
--   type Temp ModerateTerseM = Shared.Temp
--   getTemp proj = Identity . view (temp . proj) <$> ModerateTerseM get

-- instance MonadTerseStateStore ModerateTerseM where
--   putStore proj x = ModerateTerseM $ do
--     y <- get
--     Identity <$> put (set (store . proj) x y)
--   modifyStore proj f =
--     ModerateTerseM (Identity <$> modify (over (store . proj) f))

-- instance MonadTerseStateTemp ModerateTerseM where
--   putTemp proj x = ModerateTerseM $ do
--     y <- get
--     Identity <$> put (set (temp . proj) x y)
--   modifyTemp proj f =
--     ModerateTerseM (Identity <$> modify (over (temp . proj) f))

type SheepdogM = StateT Shared IO
