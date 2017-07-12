module Control.Concurrent.MVar
  ( MVar
  , withMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , tryPutMVar
  , takeMVar
  , tryTakeMVar
  , readMVar
  , swapMVar
  , modifyMVar
  , modifyMVar'
  , killMVar ) where

import Prelude

import Control.Concurrent.Internal.Types (IO, Canceler, makeIO, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError, catchError)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Function.Uncurried (Fn3, Fn4, Fn5, Fn6, runFn3, runFn4, runFn5, runFn6)

-- | An `MVar` is a synchronising variable, used for communication between concurrent threads.
-- | It can be thought of as a a box, which may be empty or full. By ```threads``` here
-- | is thread created when you ```fork``` an IO monad.
foreign import data MVar :: Type -> Type

-- | 'withMVar' is an exception-safe wrapper for operating on the contents
-- | of an 'MVar'.
withMVar :: forall a b. MVar a -> (a -> IO b) -> IO b
withMVar v act = do
    a <- takeMVar v
    b <- act a `catchError` \e -> putMVar v a *> throwError e
    putMVar v a *> pure b

-- | Create an empty MVar
newEmptyMVar :: forall a. IO (MVar a)
newEmptyMVar = liftEff _makeEmptyMVar

-- | Create a MVar with an initial value
newMVar :: forall a. a -> IO (MVar a)
newMVar a = liftEff (_makeMVar a)

-- | Put a value to the MVar, if the MVar is full, it will be queued until it become
-- | empty
putMVar :: forall a. MVar a -> a -> IO Unit
putMVar mv a = makeIO (\cb -> runFn6 _putMVar Left Right nonCanceler mv a cb)

tryPutMVar :: forall a. MVar a -> a -> IO Boolean
tryPutMVar mv a = liftEff (runFn3 _tryPutMVar Right mv a)

takeMVar :: forall a. MVar a -> IO a
takeMVar mv = makeIO (\cb -> runFn5 _takeMVar Left Right nonCanceler mv cb)

tryTakeMVar :: forall a. MVar a -> IO (Maybe a)
tryTakeMVar mv = liftEff (runFn4 _tryTakeMVar Nothing Just Right mv)

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
killMVar mv err = liftEff (runFn3 _killMVar Left mv err)

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
     . Fn4
        (forall x. Maybe x)
        (forall x. x -> Maybe x)
        (forall x. x -> Either Error x)
        (MVar a)
        (Eff eff (Maybe a))

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
     . Fn3
        (forall x. x -> Either Error x)
        (MVar a)
        a
        (Eff eff Boolean)

foreign import _killMVar
    :: forall eff a
     . Fn3
        (forall x. Error -> Either Error x)
        (MVar a)
        Error
        (Eff eff Unit)
