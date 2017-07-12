module Control.Monad.IO
  ( runIO
  , unsafeRunIO
  , forkIO
  , killThread
  , joinThread
  , onError
  , finally
  , timeout
  , module ReExport ) where

import Prelude

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.Internal.Types
    (IO, ALL, Thread(..), delay, attempt, bracket, launchIO, unsafeLaunchIO)
import Control.Concurrent.Internal.Types
    ( IO, ParIO, Thread, Canceler(..), ALL, attempt, bracket, delay, launchIO, unsafeLaunchIO
    , makeIO, nonCanceler) as ReExport
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (catchError, throwError)

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)


-- | convert an IO action to Eff action by providing a callback when it complete.
runIO :: forall eff a. (Either Error a -> Eff eff Unit) -> IO a -> Eff (all :: ALL) Unit
runIO k io = void $ launchIO $ liftEff <<< k =<< attempt io

-- | convert an IO action to Eff action by providing a callback when it complete,
-- | this function is unsafe in sense it remove the effect row ```all```
unsafeRunIO :: forall eff a. (Either Error a -> Eff eff Unit) -> IO a -> Eff eff Unit
unsafeRunIO k io = void $ unsafeLaunchIO $ liftEff <<< k =<< attempt io

-- | fork an IO action
forkIO :: forall a. IO a -> IO (Thread a)
forkIO io = liftEff (launchIO io)

killThread :: forall a. Error -> Thread a -> IO Unit
killThread e (Thread { kill }) = kill e

joinThread :: forall a. Thread a -> IO a
joinThread (Thread { join }) = join

onError :: forall a b. IO a -> IO b -> IO a
onError io fi = io `catchError` \e -> do
    _ <- fi
    throwError e

finally :: forall a b. IO a -> IO b -> IO a
finally io seq = bracket (pure unit) (const seq) (const io)

timeout :: forall a. Milliseconds -> IO a -> IO (Maybe a)
timeout ms io = do
    mv <- newEmptyMVar
    t1 <- forkIO (io >>= putMVar mv <<< Just)
    t2 <- forkIO (delay ms >>= \_ -> putMVar mv Nothing)
    x <- takeMVar mv
    case x of
        Nothing      -> killThread (error "timeout") t1 *> pure Nothing
        res@(Just _) -> killThread (error "success") t2 *> pure res
