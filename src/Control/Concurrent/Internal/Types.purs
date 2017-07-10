module Control.Concurrent.Internal.Types
    ( IO
    , ALL
    , launchIO
    , unsafeLaunchIO
    , delay
    , bracket
    , attempt
    , ParIO(..)
    , makeIO
    , Canceler(..)
    , nonCanceler
    , Thread(..) ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Parallel (parSequence_)
import Control.Parallel.Class (class Parallel)
import Control.Plus (class Plus, empty)

import Data.Either (Either(..), isLeft)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))

import Partial.Unsafe (unsafeCrashWith)


-- | The IO monad, we don't use any effect row because it added more pain to manage
-- | and it doesn't have any algebraic basis.
foreign import data IO :: Type -> Type

-- | Parallel IO
newtype ParIO a = ParIO (IO a)

-- | Canceler
newtype Canceler = Canceler (Error -> IO Unit)

-- | Represent All possible effect
foreign import data ALL :: Effect

newtype Thread a = Thread
  { kill :: Error -> IO Unit
  , join :: IO a
  }

nonCanceler :: Canceler
nonCanceler = Canceler k
  where
    k _ = pure unit

delay :: Milliseconds -> IO Unit
delay (Milliseconds n) = Fn.runFn2 _delay Right n

-- | Convert an IO to Eff monad
launchIO :: forall a. IO a -> Eff (all :: ALL) (Thread a)
launchIO ft = Fn.runFn6 _launchIO isLeft unsafeFromLeft unsafeFromRight Left Right ft

unsafeLaunchIO :: forall eff a. IO a -> Eff eff (Thread a)
unsafeLaunchIO ft = unsafeCoerceEff (launchIO ft)

bracket :: forall a b. IO a -> (a -> IO Unit) -> (a -> IO b) -> IO b
bracket acquire release kleisli = Fn.runFn3 _bracket acquire release kleisli

instance functorIO :: Functor IO where
    map f io = Fn.runFn2 _map f io

instance applyIO :: Apply IO where
    apply = ap

instance applicativeIO :: Applicative IO where
    pure = _pure

instance bindIO :: Bind IO where
    bind ta k = Fn.runFn2 _bind ta k

instance monadIO :: Monad IO

instance altIO :: Alt IO where
    alt a1 a2 = do
        r <- attempt a1
        case r of
            Left _  -> a2
            Right a -> pure a

instance plusIO :: Plus IO where
    empty = throwError (error "Always fails")

instance alternativeIO :: Alternative IO

instance monadZeroIO :: MonadZero IO

instance monadPlusIO :: MonadPlus IO

instance semigroupIO :: Semigroup a => Semigroup (IO a) where
    append = lift2 append

instance monoidIO :: Monoid a => Monoid (IO a) where
    mempty = pure mempty

instance monadRecIO :: MonadRec IO where
    tailRecM k = go
      where
        go a = k a >>= case _ of
            Done r -> pure r
            Loop b -> go b

instance monadThrowIO :: MonadThrow Error IO where
    throwError = _throwError

instance monadErrorIO :: MonadError Error IO where
    catchError ft k = attempt ft >>= case _ of
        Left err -> k err
        Right a  -> pure a

instance monadEffIO :: MonadEff eff IO where
    liftEff = _liftEff

derive instance newtypeParIO :: Newtype (ParIO a) _
derive newtype instance functorParIO :: Functor ParIO

instance applyParIO :: Apply ParIO where
    apply (ParIO ff) (ParIO fa) = ParIO (makeIO go)
      where
        go k = do
            Thread t1 <- launchIO ff
            Thread t2 <- launchIO fa
            Thread t3 <- launchIO do
                f <- attempt t1.join
                a <- attempt t2.join
                liftEff (k (f <*> a))
            pure $ Canceler \err -> parSequence_
                [ t3.kill err
                , t1.kill err
                , t2.kill err
                ]

instance applicativeParIO :: Applicative ParIO where
    pure = ParIO <<< pure

instance semigroupParIO :: Semigroup a => Semigroup (ParIO a) where
    append = lift2 append

instance monoidParFuture :: Monoid a => Monoid (ParIO a) where
    mempty = pure mempty

instance altParIO :: Alt ParIO where
    alt (ParIO a1) (ParIO a2) = ParIO (makeIO go)
      where
        go k = do
            ref <- unsafeRunRef $ newRef Nothing
            Thread t1 <- launchIO a1
            Thread t2 <- launchIO a2
            let
              earlyError = error "Alt ParIO: early exit"
              runK t r = do
                res <- liftEff $ readRef ref
                case res, r of
                    Nothing, Left _  -> liftEff $ writeRef ref (Just r)
                    Nothing, Right _ -> t.kill earlyError *> liftEff (k r)
                    Just r', Left _  -> t.kill earlyError *> liftEff (k r') -- both error
                    Just _, Right _  -> t.kill earlyError *> liftEff (k r)
            Thread t3 <- launchIO $ runK t2 =<< attempt t1.join
            Thread t4 <- launchIO $ runK t1 =<< attempt t2.join
            pure $ Canceler \err -> parSequence_
                [ t3.kill earlyError
                , t4.kill earlyError
                , t1.kill earlyError
                , t2.kill earlyError
                ]

instance plusParIO :: Plus ParIO where
    empty = ParIO empty

instance alternativeParIO :: Alternative ParIO

instance parallelIO :: Parallel ParIO IO where
    parallel = ParIO
    sequential (ParIO io) = io

instance functorThread :: Functor Thread where
    map f (Thread { kill, join }) = Thread { kill, join: f <$> join }

foreign import _pure :: forall a. a -> IO a
foreign import _map :: forall a b. Fn.Fn2 (a -> b) (IO a) (IO b)
foreign import _bind :: forall a b. Fn.Fn2 (IO a) (a -> IO b) (IO b)
foreign import _throwError :: forall a. Error -> IO a
foreign import _liftEff :: forall eff a. Eff eff a -> IO a

-- | attempt computation of an IO
foreign import attempt :: forall a. IO a -> IO (Either Error a)

foreign import _bracket :: forall a b. Fn.Fn3 (IO a) (a -> IO Unit) (a -> IO b) (IO b)

foreign import makeIO :: forall eff a. ((Either Error a -> Eff eff Unit) -> Eff eff Canceler) -> IO a

foreign import _delay :: forall a. Fn.Fn2 (Unit -> Either a Unit) Number (IO Unit)

foreign import _launchIO
  :: forall a
  . Fn.Fn6
      (Either Error a -> Boolean)
      (Either Error a -> Error)
      (Either Error a -> a)
      (Error -> Either Error a)
      (a -> Either Error a)
      (IO a)
      (Eff (all :: ALL) (Thread a))

unsafeFromLeft :: forall x y. Either x y -> x
unsafeFromLeft = case _ of
  Left a   -> a
  Right  _ -> unsafeCrashWith "unsafeFromLeft: Right"

unsafeFromRight :: forall x y. Either x y → y
unsafeFromRight = case _ of
  Right a -> a
  Left  _ -> unsafeCrashWith "unsafeFromRight: Left"
