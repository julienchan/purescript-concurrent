module Test.Main
    ( main
    ) where

import Prelude

import Control.Alt ((<|>))
import Control.Concurrent.MVar (makeEmptyMVar, putMVar, takeMVar, tryTakeMVar, readMVar, killMVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, EXCEPTION, throwException, error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO
    ( IO, ALL, runIO, attempt, bracket, delay, makeIO, forkIO, launchIO
    , joinThread, killThread, nonCanceler)
import Control.Parallel (parallel, sequential)

import Data.Either (Either(..), isLeft)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (traverse)
import Data.Time.Duration (Milliseconds(..))

import Test.Assert (assert', ASSERT)

type TestEff = Eff (assert :: ASSERT, console :: CONSOLE, exception :: EXCEPTION)

timeout :: Milliseconds -> IO Unit -> IO Unit
timeout ms io = do
    exn <- makeEmptyMVar
    t1 <- forkIO (delay ms *> putMVar exn (Just "Timed out"))
    t2 <- forkIO (io *> putMVar exn Nothing)
    res <- takeMVar exn
    case res of
        Nothing -> void $ killThread (error "Done") t1
        Just e  -> void $ killThread (error "Done") t2 *> throwError (error e)

assertEff
    :: String
    -> Either Error Boolean
    -> TestEff Unit
assertEff s = case _ of
    Left err -> log ("[Error] " <> s) *> throwException err
    Right a  -> do
        _ <- assert' s a
        log ("[OK] " <> s)

runAssert :: String -> IO Boolean -> Eff (all :: ALL) Unit
runAssert s = runIO (assertEff s)

runAssertEq :: forall a. Eq a => String -> a -> IO a -> Eff (all :: ALL) Unit
runAssertEq s a = runIO (assertEff s <<< map (eq a))

assertEq :: forall a. Eq a => String -> a -> IO a -> IO Unit
assertEq s a io = liftEff <<< assertEff s <<< map (eq a) =<< attempt io

assert :: String -> IO Boolean -> IO Unit
assert s io = liftEff <<< assertEff s =<< attempt io

test_pure :: Eff (all :: ALL) Unit
test_pure = runAssertEq "pure" 42 (pure 42)

test_bind :: Eff (all :: ALL) Unit
test_bind = runAssertEq "bind" 44 do
    n1 <- pure 42
    n2 <- pure (n1 + 1)
    n3 <- pure (n2 + 1)
    pure n3

test_attempt :: Eff (all :: ALL) Unit
test_attempt = runAssert "attempt" do
    n <- attempt (pure 42)
    case n of
        Right 42 -> pure true
        _        -> pure false

test_throw :: Eff (all :: ALL) Unit
test_throw = runAssert "throw" do
    n <- attempt (throwError (error "Error"))
    pure $ isLeft n

test_delay :: IO Unit
test_delay = assert "delay" do
    _ <- delay (Milliseconds 1000.0)
    pure true

test_liftEff :: Eff (all :: ALL) Unit
test_liftEff = runAssertEq "liftEff" 42 do
    ref <- liftEff (Ref.newRef 0)
    liftEff do
        Ref.writeRef ref 42
        Ref.readRef ref

test_fork :: IO Unit
test_fork = assert "fork" do
    ref <- liftEff (Ref.newRef 0)
    _ <- forkIO do
        _ <- delay (Milliseconds 10.0)
        liftEff (Ref.modifyRef ref (_ + 1))
    _ <- liftEff (Ref.writeRef ref 42)
    _ <- delay (Milliseconds 20.0)
    _ <- liftEff (Ref.modifyRef ref (_ - 3))
    eq 40 <$> (liftEff (Ref.readRef ref))

test_join :: IO Unit
test_join = assert "join" do
    ref <- liftEff (Ref.newRef 1)
    t <- forkIO do
        _ <- delay (Milliseconds 10.0)
        _ <- liftEff (Ref.modifyRef ref (_ - 2))
        liftEff (Ref.readRef ref)
    liftEff (Ref.writeRef ref 42)
    eq 40 <$> joinThread t

test_join_throw :: IO Unit
test_join_throw = assert "join/throw" do
    t <- forkIO do
        _ <- delay (Milliseconds 10.0)
        throwError (error "No")
    isLeft <$> attempt (joinThread t)

test_join_throw_sync :: IO Unit
test_join_throw_sync = assert "join/throw sync" do
    t <- forkIO (throwError (error "No"))
    isLeft <$> attempt (joinThread t)

test_multi_join :: IO Unit
test_multi_join = assert "multi join" do
    ref <- liftEff (Ref.newRef 1)
    t1 <- forkIO do
        _ <- delay (Milliseconds 10.0)
        liftEff (Ref.modifyRef ref (_ + 1))
        pure 10
    t2 <- forkIO do
        _ <- delay (Milliseconds 20.0)
        liftEff (Ref.modifyRef ref (_ + 1))
        pure 20
    n1 <- sum <$> traverse joinThread
        [ t1
        , t1
        , t1
        , t2
        ]
    n2 <- liftEff (Ref.readRef ref)
    pure (n1 == 50 && n2 == 3)

test_makeIO :: IO Unit
test_makeIO = assert "makeIO" do
    ref1 <- liftEff (Ref.newRef Nothing)
    ref2 <- liftEff (Ref.newRef 0)
    t <- forkIO do
        n <- makeIO \cb -> do
            Ref.writeRef ref1 (Just cb)
            pure nonCanceler
        liftEff (Ref.writeRef ref2 n)
    cb <- liftEff (Ref.readRef ref1)
    case cb of
        Just k -> do
            liftEff $ k (Right 42)
            eq 42 <$> liftEff (Ref.readRef ref2)
        Nothing -> pure false

test_bracket :: IO Unit
test_bracket = assert "bracket" do
    ref <- liftEff (Ref.newRef [])
    let
      action s = do
          _ <- delay (Milliseconds 10.0)
          liftEff (Ref.modifyRef ref (_ <> [ s ]))
          pure s
    t <- forkIO do
        _ <- delay (Milliseconds 100.0)
        liftEff (Ref.readRef ref)
    _ <- bracket
        (action "foo")
        (\s -> void $ action (s <> "/release"))
        (\s -> action (s <> "/run"))
    joinThread t <#> eq
        [ "foo"
        , "foo/run"
        , "foo/release"
        ]

test_kill :: IO Unit
test_kill = assert "kill" do
    t <- forkIO $ makeIO \_ -> pure nonCanceler
    _ <- killThread (error "No") t
    isLeft <$> attempt (joinThread t)

test_parallel :: IO Unit
test_parallel = assert "parallel" do
    n <- sequential $ parallel (delay (Milliseconds 100.0) $> 42) <|>
        parallel (delay (Milliseconds 50.0) $> 50)
    pure (n == 50)

test_parallel_error :: IO Unit
test_parallel_error = assert "parallel/error" do
    n <- attempt $ sequential (parallel (throwError (error "Oh, noes")) *> pure unit)
    pure $ isLeft n

test_parallel_choose_success :: IO Unit
test_parallel_choose_success = assert "parallel/error multi" do
    n <- sequential $ parallel (delay (Milliseconds 100.0) *> throwError (error "Oh, noes")) <|>
        parallel (delay (Milliseconds 200.0) $> 50)
    pure (n == 50)

test_putTakeMVar :: IO Unit
test_putTakeMVar = timeout (Milliseconds 1000.0) $ assert "putTakeMVar" do
    v <- makeEmptyMVar
    _ <- forkIO (delay (Milliseconds 0.0) *> putMVar v 1.0)
    eq 1.0 <$> takeMVar v

test_readMVar :: IO Unit
test_readMVar = timeout (Milliseconds 1000.0) $ assert "readMVar" do
    v <- makeEmptyMVar
    _ <- forkIO (delay (Milliseconds 10.0) *> putMVar v 42)
    eq <$> readMVar v <*> takeMVar v

test_killMVar :: IO Unit
test_killMVar = assert "killMVar" do
    v <- makeEmptyMVar
    _ <- killMVar v (error "No")
    isLeft <$> attempt (takeMVar v)

test_tryTakeEmptyMVar :: IO Unit
test_tryTakeEmptyMVar = timeout (Milliseconds 1000.0) $ assert "tryTakeMVar/empty" do
    mv <- makeEmptyMVar
    isNothing <$> tryTakeMVar mv

test_tryTakeFullMVar :: IO Unit
test_tryTakeFullMVar = timeout (Milliseconds 1000.0) $ assert "tryTakeMVar/full" do
    mv <- makeEmptyMVar
    _ <- putMVar mv 43
    x <- tryTakeMVar mv
    case x of
        Nothing -> pure false
        Just y  -> pure (y == 43)

main :: Eff (all :: ALL) Unit
main = do
    test_pure
    test_bind
    test_attempt
    test_throw
    test_liftEff
    void $ launchIO do
        test_delay
        test_fork
        test_join
        test_join_throw
        test_join_throw_sync
        test_multi_join
        test_makeIO
        test_kill
        test_bracket
        test_parallel
        test_parallel_error
        test_parallel_choose_success
        test_putTakeMVar
        test_readMVar
        test_killMVar
        test_tryTakeEmptyMVar
        test_tryTakeFullMVar
