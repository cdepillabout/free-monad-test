{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module More where

import ClassyPrelude

import Control.Monad.Free (Free(..), MonadFree, iterM, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.Functor.Sum (Sum(..))

data FooF x
  = Foo Text x
  deriving (Functor, Show, Typeable)

data BarF x
  = Bar Text x
  deriving (Functor, Show, Typeable)

data BazF x
  = Baz Text x
  deriving (Functor, Show, Typeable)

class (Functor f, Functor g) => f :<: g where
  inject :: f a -> g a

instance Functor f => f:<: f where
  inject :: f a -> f a
  inject = id

instance (Functor f, Functor g) => f:<: Sum f g where
  inject :: f a -> Sum f g a
  inject = InL

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: h) => f:<: Sum g h where
  inject :: f a -> Sum g h a
  inject = InR . inject

type MyCoProd = Sum FooF (Sum BarF BazF)

foo :: (FooF :<: f, MonadFree f m) => Text -> m ()
foo t = liftF . inject $ Foo t ()

bar :: (BarF :<: f, MonadFree f m) => Text -> m ()
bar t = liftF . inject $ Bar t ()

baz :: (BazF :<: f, MonadFree f m) => Text -> m ()
baz t = liftF . inject $ Baz t ()

superDooper :: (FooF :<: f, BarF :<: f, MonadFree f m) => Text -> m ()
superDooper t = do
  foo t
  bar t

class Monad m => MonadFoo m
instance (FooF :<: f, MonadFree f m, Monad m) => MonadFoo m

class Monad m => MonadBar m
instance (BarF :<: f, MonadFree f m, Monad m) => MonadBar m

class Monad m => MonadBaz m
instance (BazF :<: f, MonadFree f m, Monad m) => MonadBaz m

-- what :: (MonadFoo m, MonadBaz m) => m ()
-- what = do
--   foo "hello"
--   bar "bye"

-- type MonadMyFoo m = forall f . (FooF :<: f, MonadFree f m, Monad m)
