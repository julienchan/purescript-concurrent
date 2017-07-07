module Control.Concurrent.Internal.MVar where

import Prelude

import Control.Concurrent.Internal.Types (IO, Canceler, makeIO, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError, catchError)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Function.Uncurried (Fn5, Fn6, Fn7, runFn5, runFn6, runFn7)

foreign import data MVar :: Type -> Type

withMVar :: forall a b. MVar a -> (a -> IO b) -> IO b
withMVar v act = do
  a <- takeMVar v
  b <- act a `catchError` \e -> putMVar v a *> throwError e
  putMVar v a *> pure b

makeEmptyMVar :: forall a. IO (MVar a)
makeEmptyMVar = liftEff _makeEmptyMVar

makeMVar :: forall a. a -> IO (MVar a)
makeMVar a = liftEff (_makeMVar a)

putMVar :: forall a. MVar a -> a -> IO Unit
putMVar mv a = makeIO (\cb -> runFn6 _putMVar Left Right nonCanceler mv a cb)

tryPutMVar :: forall a. MVar a -> a -> IO Boolean
tryPutMVar mv a = makeIO (\cb -> runFn6 _tryPutMVar Left Right nonCanceler mv a cb)

takeMVar :: forall a. MVar a -> IO a
takeMVar mv = makeIO (\cb -> runFn5 _takeMVar Left Right nonCanceler mv cb)

tryTakeMVar :: forall a. MVar a -> IO (Maybe a)
tryTakeMVar mv = makeIO (\cb -> runFn7 _tryTakeMVar Nothing Just Left Right nonCanceler mv cb)

readMVar :: forall a. MVar a -> IO a
readMVar mv = makeIO (\cb -> runFn5 _readMVar Left Right nonCanceler mv cb)

swapMVar :: forall a. MVar a -> a -> IO a
swapMVar mv new = do
  old <- takeMVar mv
  _ <- putMVar mv new
  pure old

modifyMVar :: forall a b. MVar a -> (a -> IO (Tuple a b)) -> IO b
modifyMVar v io = do
  a <- takeMVar v
  Tuple a' b <- io a `catchError` \e -> putMVar v a *> throwError e
  putMVar v a' *> pure b

modifyMVar' :: forall a. MVar a -> (a -> IO a) -> IO Unit
modifyMVar' mv io = do
  a <- takeMVar mv
  a' <- io a `catchError` \e -> putMVar mv a *> throwError e
  putMVar mv a'

killMVar :: forall a. MVar a -> Error -> IO Unit
killMVar mv err = makeIO (\cb -> runFn6 _killMVar Left Right nonCanceler mv err cb)

foreign import _makeEmptyMVar :: forall eff a. Eff eff (MVar a)

foreign import _makeMVar :: forall eff a. a -> Eff eff (MVar a)

foreign import _takeMVar
    :: forall eff a
     . Fn5
        (forall x. Error -> Either Error x)
        (forall x. x -> Either Error x)
        Canceler
        (MVar a)
        (Either Error a -> Eff eff Unit)
        (Eff eff Canceler)

foreign import _tryTakeMVar
    :: forall eff a
     . Fn7
        (forall x. Maybe x)
        (forall x. x -> Maybe x)
        (forall x. Error -> Either Error x)
        (forall x. x -> Either Error x)
        Canceler
        (MVar a)
        (Either Error (Maybe a) -> Eff eff Unit)
        (Eff eff Canceler)

foreign import _readMVar
    :: forall eff a
     . Fn5
        (forall x. Error -> Either Error x)
        (forall x. x -> Either Error x)
        Canceler
        (MVar a)
        (Either Error a -> Eff eff Unit)
        (Eff eff Canceler)

foreign import _putMVar
    :: forall eff a
     . Fn6
        (forall x. Error -> Either Error x)
        (forall x. x -> Either Error x)
        Canceler
        (MVar a)
        a
        (Either Error Unit -> Eff eff Unit)
        (Eff eff Canceler)

foreign import _tryPutMVar
    :: forall eff a
     . Fn6
        (forall x. Error -> Either Error x)
        (forall x. x -> Either Error x)
        Canceler
        (MVar a)
        a
        (Either Error Boolean -> Eff eff Unit)
        (Eff eff Canceler)

foreign import _killMVar
    :: forall eff a
     . Fn6
        (forall x. Error -> Either Error x)
        (forall x. x -> Either Error x)
        Canceler
        (MVar a)
        Error
        (Either Error Unit -> Eff eff Unit)
        (Eff eff Canceler)
