{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Bound
import Control.Applicative
import Control.Monad
import Data.Functor.Classes
import Data.Foldable
import Data.Traversable
import Data.Eq.Deriving (deriveEq1)      -- these two are from the
import Text.Show.Deriving (deriveShow1)  -- deriving-compat package
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Error

data Exp a = V a | App (Exp a) (Exp a) | Lam (Scope () Exp a)
  deriving (Functor, Foldable, Traversable)


instance Applicative Exp where pure = V; (<*>) = ap

instance Monad Exp where
  V a      >>= f = f a
  (App x y) >>= f = App (x >>= f) (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)

deriveEq1 ''Exp
deriveShow1 ''Exp

deriving instance Show a => Show (Exp a)
deriving instance Eq a => Eq (Exp a)

main :: IO ()
main = putStrLn "Hello, Haskell!"
