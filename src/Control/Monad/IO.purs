module Control.Monad.IO
    ( runIO
    , forkIO
    , killThread
    , joinThread
    , module ReExport
    ) where

import Prelude

import Control.Concurrent.Internal.Types (IO, ALL, Thread(..), attempt, launchIO)
import Control.Concurrent.Internal.Types
    ( IO, ParIO, Thread, Canceler(..), ALL, attempt, bracket, delay, launchIO, makeIO
    , nonCanceler) as ReExport
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)

import Data.Either (Either)

-- | convert an IO action to Eff action by providing a callback when it complete.
runIO :: forall eff a. (Either Error a -> Eff eff Unit) -> IO a -> Eff (all :: ALL) Unit
runIO k io = void $ launchIO $ liftEff <<< k =<< attempt io

-- | fork an IO action
forkIO :: forall a. IO a -> IO (Thread a)
forkIO io = liftEff (launchIO io)

killThread :: forall a. Error -> Thread a -> IO Unit
killThread e (Thread { kill }) = kill e

joinThread :: forall a. Thread a -> IO a
joinThread (Thread { join }) = join
