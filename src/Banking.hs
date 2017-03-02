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

module Banking where

import ClassyPrelude

import Control.Monad.Free (Free, liftF)

type Amount = Int
type Error = String

data From a = From a
data To a = To a

data Account = AliceAccount | BobAccount
  deriving (Eq, Read, Show)

type TransferResult = Either Error (From Amount, To Amount)

class Banking f where
  accounts :: f [Account]
  balance  :: Account -> f Amount
  transfer :: Amount -> From Account -> To Account -> f TransferResult
  withdraw :: Amount -> f Amount

data BankingF a
  = Accounts ([Account] -> a)
  | Balance Account (Amount -> a)
  | Transfer Amount (From Account) (To Account) (TransferResult -> a)
  | Withdraw Amount (Amount -> a)
  deriving Functor

instance Banking BankingF where
  accounts :: BankingF [Account]
  accounts = Accounts id

  balance :: Account -> BankingF Amount
  balance a = Balance a id

  transfer :: Amount -> From Account -> To Account -> BankingF TransferResult
  transfer a f t = Transfer a f t id

  withdraw :: Amount -> BankingF Amount
  withdraw a = Withdraw a id

instance (Banking f, Functor f) => Banking (Free f) where
  accounts :: Free f [Account]
  accounts = liftF accounts

  balance :: Account -> Free f Amount
  balance a = liftF (balance a)

  transfer :: Amount -> From Account -> To Account -> Free f TransferResult
  transfer a f t = liftF (transfer a f t)

  withdraw :: Amount -> Free f Amount
  withdraw a = liftF (withdraw a)


-- example :: forall f. (Inject BankingF f) => Free f Amount
-- example = do
--   as <- accounts
--   b  <- balance (head as)
--   return b

type Interpreter f g = forall a. f a -> Free g a
type f ~< g = Interpreter f g
infixr 4 ~<

type Nat f g = forall a. f a -> g a
type f ~> g = Nat f g

type Halt f a = f ()

-- bankingLogging :: BankingF ~< Halt LoggingF
-- bankingProtocol :: BankingF ~< ProtocolF
-- protocolSocket :: ProtocolF ~< SocketF
-- loggingFile :: LoggingF ~< FileF
-- execFile :: FileF ~> IO
-- execSocket :: SocketF ~> IO
