{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Other where

import ClassyPrelude

import Control.Monad.Free (Free(..), MonadFree, iterM, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.Functor.Sum (Sum(..))

data BoolaF x
  = Boola Text x
  deriving (Functor, Show, Typeable)

data DooF x
  = Doo Text x
  deriving (Functor, Show, Typeable)

data SillyF x
  = Silly Text x
  deriving (Functor, Show, Typeable)

class (Functor f, Functor g) => f :<: g where
  inject :: f a -> g a

instance Functor f => f:<: f where
  inject :: f a -> f a
  inject = id

instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => f:<: Sum f g where
  inject :: f a -> Sum f g a
  inject = InL

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: h) => f:<: Sum g h where
  inject :: f a -> Sum g h a
  inject = InR . inject

type MyCoProd = Sum BoolaF (Sum DooF SillyF)

class Monad m => MonadBoola m where
  boola :: Text -> m ()

class Monad m => MonadDoo m where
  doo :: Text -> m ()

class Monad m => MonadSilly m where
  silly :: Text -> m ()

instance (Monad m, MonadFree f m, BoolaF :<: f) => MonadBoola m where
  boola :: Text -> m ()
  boola t = liftF $ inject $ Boola t ()

instance (Monad m, MonadFree f m, DooF :<: f) => MonadDoo m where
  doo :: Text -> m ()
  doo t = liftF $ inject $ Doo t ()

what :: (MonadBoola m, MonadDoo m) => m ()
what = do
  boola "hello"
  doo "bye"

interp :: forall m a . MonadIO m => Free MyCoProd a -> m a
interp = iterM go
  where
    go :: MyCoProd (m a) -> m a
    go (InL boolaF) = interpBoola boolaF
    go (InR (InL dooF)) = interpDoo dooF
    go (InR (InR sillyF)) = interpSilly sillyF

interpBoola :: MonadIO m => BoolaF (m a) -> m a
interpBoola (Boola t next) = do
  putStrLn t
  next

interpDoo :: MonadIO m => DooF (m a) -> m a
interpDoo (Doo t next) = do
  putStrLn t
  next

interpSilly :: MonadIO m => SillyF (m a) -> m a
interpSilly (Silly t next) = do
  putStrLn t
  next

dada :: IO ()
dada = interp what
