{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Retry where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Free (Free, MonadFree, iterM, liftF)
import Control.Monad.Free.TH (makeFreeCon, makeFreeCon_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Foldable (msum)
import Text.Read (readMaybe)

-- | A data type representing basic commands for a retriable eDSL.
data RetryF err next where
  Output    :: String -> next -> RetryF err next
  Input     :: forall err a next. Read a => (a -> next) -> RetryF err next
  WithRetry :: forall err a next. Retry err a -> (a -> next) -> RetryF err next
  Retry     :: RetryF err next

-- | Unfortunately this Functor instance cannot yet be derived
-- automatically by GHC.
instance Functor (RetryF err) where
  fmap f (Output s x) = Output s (f x)
  fmap f (Input g) = Input (f . g)
  fmap f (WithRetry block g) = WithRetry block (f . g)
  fmap _ Retry = Retry

-- | The monad for a retriable eDSL.
type Retry err = Free (RetryF err)

-- | Simple output command.
output :: MonadFree (RetryF err) m => String -> m ()
output s = liftF $ Output s ()

-- | Get anything readable from input.
-- makeFreeCon 'Input
input :: (MonadFree (RetryF err) m, Read a) => m a
input = liftF $ Input id

-- | Force retry command (retries innermost retriable block).
-- makeFreeCon 'Retry
retry :: MonadFree (RetryF err) m => m ()
retry = liftF Retry

withRetry :: MonadFree (RetryF err) m => Retry err a -> m a
withRetry ret = liftF $ WithRetry ret id

data RetryError
  = CouldNotRead
  | CalledRetry
  deriving (Eq, Show)

runRetry :: forall m a. (MonadError RetryError m, MonadIO m) => Retry RetryError a -> m a
runRetry = iterM run
  where
    run :: RetryF RetryError (m a) -> m a
    run (Output s (next :: m a)) = do
      liftIO $ putStrLn s
      next
    run (Input (next :: Read x => x -> m a)) = do
      s <- liftIO getLine
      case readMaybe s of
        Just x  -> next x
        Nothing -> throwError CouldNotRead
    run Retry = throwError CalledRetry
    run (WithRetry (block :: Retry RetryError b) (next :: b -> m a)) = do
      let (blockToRepeat :: ExceptT RetryError m b)  = runRetry block
          (firstRightBlock :: m b) = tryUntilRight blockToRepeat
      x <- firstRightBlock
      next x

runRetry' :: forall m a. (MonadIO m) => Retry RetryError a -> ExceptT RetryError m a
runRetry' = iterM run
  where
    run :: RetryF RetryError (ExceptT RetryError m a) -> ExceptT RetryError m a
    run (Output s (next :: ExceptT RetryError m a)) = do
      liftIO $ putStrLn s
      next
    run (Input (next :: Read x => x -> ExceptT RetryError m a)) = do
      s <- liftIO getLine
      case readMaybe s of
        Just x  -> next x
        Nothing -> throwError CouldNotRead
    run Retry = throwError CalledRetry
    run (WithRetry (block :: Retry RetryError b) (next :: b -> ExceptT RetryError m a)) = do
      let (blockToRepeat :: ExceptT RetryError m b)  = runRetry' block
          (firstRightBlock :: m b) = tryUntilRight blockToRepeat
      x <- lift firstRightBlock
      next x

-- runRetry'' :: forall m a. (MonadIO m) => Retry RetryError a -> m (Either RetryError a)
-- runRetry'' = iterM run
--   where
--     run :: RetryF RetryError (m (Either RetryError a)) -> m (Either RetryError a)
--     run (Output s (next :: m (Either RetryError a))) = do
--       liftIO $ putStrLn s
--       next
--     run (Input (next :: Read x => x -> m (Either RetryError a))) = do
--       s <- liftIO getLine
--       case readMaybe s of
--         Just x  -> next x
--         Nothing -> throwError CouldNotRead
--     run Retry = throwError CalledRetry
--     run (WithRetry (block :: Retry RetryError b) (next :: b -> m (Either RetryError a))) = do
--       let (blockToRepeat :: m (Either RetryError b)) = runRetry'' block
--       undefined
--           -- (firstRightBlock :: m b) = tryUntilRight blockToRepeat
--       -- x <- lift firstRightBlock
--       -- next x

tryUntilRight :: Monad m => ExceptT err m a -> m a
tryUntilRight exceptT = do
  eitherRes <- runExceptT exceptT
  case eitherRes of
    Right res -> pure res
    Left _ -> tryUntilRight exceptT

-- | Sample program.
testRetry :: Retry RetryError ()
testRetry = do
  n <- withRetry $ do
    output "Enter any positive number: "
    n <- input
    when (n <= 0) $ do
      output "The number should be positive."
      retry
    return n
  output $ "You've just entered " ++ show (n :: Int)
