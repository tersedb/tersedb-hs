module Lib.Api where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Proxy (Proxy))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Lib.Api.Action (Action (CreateAction), MutableAction, isMutable)
import Lib.Api.Action.Create (CreateAction (..))
import Lib.Api.Response (Authorize (Authorized), Response, makeResponse)
import Lib.Class (
  TerseDB (commit),
 )
import Lib.Types.Errors (UnauthorizedAction (UnauthorizedAction))
import Lib.Types.Id (ActorId, AnyId (..))
import System.Random.Stateful (Uniform (uniformM), globalStdGen)

-- | If one action is unauthorized, continue
actMany
  :: forall m n
   . (TerseDB n m, MonadIO n)
  => Proxy m
  -> m ()
  -- ^ Conditional action to perform when mutating
  -> (MutableAction -> n ())
  -- ^ Each mutation performed that's authorized
  -> NonEmpty ActorId
  -> [Action]
  -> n [Authorize (Maybe Response)]
actMany Proxy beforeMutate onAuthorized actors xs = do
  ids <- traverse genId xs
  let ys :: m [Authorize (Maybe Response, Maybe MutableAction)]
      ys = traverse go (zip xs ids)
       where
        go
          :: (Action, Maybe AnyId) -> m (Authorize (Maybe Response, Maybe MutableAction))
        go (x, mId) = makeResponse actors x mId
  rs <- commit $ do
    when (any isMutable xs) beforeMutate
    ys
  for rs $ \mAuth -> do
    case mAuth of
      Authorized (_, Just action') -> onAuthorized action'
      _ -> pure ()
    pure (fst <$> mAuth)

act
  :: forall m n
   . (TerseDB n m, MonadIO n)
  => Proxy m
  -> m ()
  -> (MutableAction -> n ())
  -> NonEmpty ActorId
  -> Action
  -> n (Authorize (Maybe Response))
act Proxy beforeMutate onAuthorized actors x = do
  mId <- genId x
  let y :: m (Authorize (Maybe Response, Maybe MutableAction))
      y = makeResponse actors x mId
  mAuth <- commit $ do
    when (isMutable x) beforeMutate
    y
  case mAuth of
    Authorized (_, Just action') -> onAuthorized action'
    _ -> pure ()
  pure (fst <$> mAuth)

-- | If one action is unauthorized, abandon all transactions
actManyStrict
  :: forall m n
   . (TerseDB n m, MonadIO n, MonadThrow m)
  => Proxy m
  -> m ()
  -- ^ Conditional action to perform when mutating
  -> ([MutableAction] -> n ())
  -- ^ Mutations performed, in the same order as the actions supplied
  -> NonEmpty ActorId
  -> [Action]
  -> n [Maybe Response]
actManyStrict Proxy beforeMutate onAuthorized actors xs = do
  ids <- traverse genId xs
  let ys :: m [(Maybe Response, Maybe MutableAction)]
      ys = traverse go (zip xs ids)
       where
        go :: (Action, Maybe AnyId) -> m (Maybe Response, Maybe MutableAction)
        go (x, mId) = do
          mAuth <- makeResponse actors x mId
          case mAuth of
            Authorized y -> pure y
            _ -> throwM UnauthorizedAction
  (rs, as) <- fmap unzip . commit $ do
    when (any isMutable xs) beforeMutate
    ys
  onAuthorized (catMaybes as)
  pure rs

actStrict
  :: forall m n
   . (TerseDB n m, MonadIO n, MonadThrow m)
  => Proxy m
  -> m ()
  -> (MutableAction -> n ())
  -> NonEmpty ActorId
  -> Action
  -> n (Maybe Response)
actStrict Proxy beforeMutate onAuthorized actors x = do
  mId <- genId x
  let y :: m (Maybe Response, Maybe MutableAction)
      y = do
        mAuth <- makeResponse actors x mId
        case mAuth of
          Authorized z -> pure z
          _ -> throwM UnauthorizedAction
  (r, mA) <- commit $ do
    when (isMutable x) beforeMutate
    y
  for_ mA onAuthorized
  pure r

genId :: forall n. (MonadIO n) => Action -> n (Maybe AnyId)
genId action = case action of
  CreateAction x -> case x of
    CreateActor -> Just . AnyIdActor <$> gen
    CreateGroup -> Just . AnyIdGroup <$> gen
    CreateSpace -> Just . AnyIdSpace <$> gen
    CreateEntity _ _ -> Just <$> (AnyIdEntity <$> gen <*> gen)
    CreateVersion _ -> Just . AnyIdVersion <$> gen
    _ -> pure Nothing
  _ -> pure Nothing
 where
  gen :: forall a. (Uniform a) => n a
  gen = uniformM globalStdGen
