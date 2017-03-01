{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import ClassyPrelude

import Control.Monad.Free (Free(..), MonadFree, iterM, liftF)
import Control.Monad.Free.TH (makeFree)

data MyF x
  = ReadText (Text -> x)
  | Write Text x
  deriving (Functor, Typeable)

$(makeFree ''MyF)

myAction :: MonadFree MyF m => m ()
myAction = do
  t <- readText
  if t == "hello" then write "hello yo" else write "something else yo"
  pure ()

interp :: forall m a . MonadIO m => Free MyF a -> m a
interp = iterM go
  where
    go :: MyF (m a) -> m a
    go (ReadText next) = do
      putStrLn "in read, sending yoyo bear"
      next "yoyo bear"
    go (Write text next) = do
      putStrLn $ "in write, got: " <> text
      next

testMyAction :: Maybe ()
testMyAction = do
  next1 <- testMyFReadText "hello"myAction
  (t, next2) <- testMyFWrite next1
  guard $ t == "hello yo"
  testPure next2

testMyAction' :: Maybe ()
testMyAction' = do
  (t, next2) <- testMyFWrite =<< testMyFReadText "oaoa" myAction
  guard $ t == "hello yo"
  testPure next2

testMyFReadText :: Text -> Free MyF a -> Maybe (Free MyF a)
testMyFReadText t (Free (ReadText f)) = Just $ f t
testMyFReadText _ _ = Nothing

testMyFWrite :: Free MyF a -> Maybe (Text, Free MyF a)
testMyFWrite (Free (Write t f)) = Just (t, f)
testMyFWrite _ = Nothing

testPure :: Free f a -> Maybe a
testPure (Pure a) = Just a
testPure _ = Nothing
