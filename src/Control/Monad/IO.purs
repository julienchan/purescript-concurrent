module Control.Monad.IO
  ( runIO
  , unsafeRunIO
  , forkIO
  , killThread
  , joinThread
  , onError
  , finally
  , module ReExport ) where

import Prelude

import Control.Concurrent.Internal.Types (IO, ALL, Thread(..), attempt, bracket, launchIO, unsafeLaunchIO)
import Control.Concurrent.Internal.Types
    ( IO, ParIO, Thread, Canceler(..), ALL, attempt, bracket, delay, launchIO, unsafeLaunchIO
    , makeIO, nonCanceler) as ReExport
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (catchError, throwError)

import Data.Either (Either)

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
