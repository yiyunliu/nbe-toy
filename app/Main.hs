{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Bound
import Control.Monad
import Data.Eq.Deriving (deriveEq1)      -- these two are from the
import Text.Show.Deriving (deriveShow1)  -- deriving-compat package
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh
import Data.HashMap.Strict

type Env a = HashMap Symbol a
type Symbol = Int

-- Passing () to scope because it is a single binder
-- If it is a binding form for two vars simultaneously,
-- we need to replace () with Bool or any type with two inhabitants
data Exp a = EV a | EApp (Exp a) (Exp a) | ELam (Scope () Exp a)
  deriving (Functor, Foldable, Traversable)

data Value a =
    VCLOS (Scope () Exp a)
  | VNeutral (Neutral a)
  deriving (Functor, Foldable, Traversable)

data Neutral a =
    NV a
  | NApp (Neutral a) (Value a)
  deriving (Functor, Foldable, Traversable)

-- newtype CLOS a = CLOS
--   { body :: Scope () Exp a }

instance Applicative Exp where pure = EV; (<*>) = ap

instance Monad Exp where
  EV a      >>= f = f a
  (EApp x y) >>= f = EApp (x >>= f) (y >>= f)
  ELam e    >>= f = ELam (e >>>= f)

deriveEq1 ''Exp
deriveShow1 ''Exp

deriving instance Show a => Show (Exp a)
deriving instance Eq a => Eq (Exp a)

deriving instance Show a => Show (Neutral a)
deriving instance Eq a => Eq (Neutral a)

deriving instance Show a => Show (Value a)
deriving instance Eq a => Eq (Value a)

embedValue :: Value a -> Exp a
embedValue (VCLOS body) = ELam body
embedValue (VNeutral n) = embedNeutral n

embedNeutral :: Neutral a -> Exp a
embedNeutral (NV a) = EV a
embedNeutral (NApp n v) = EApp (embedNeutral n) (embedValue v)

val :: (Show a, Member (Error String) effs) => Exp a -> Eff effs (Value a)
val (ELam body) = pure $ VCLOS body
val (EV a) = pure $ VNeutral (NV a)
val (EApp x y) = join $ doAp <$> val x <*> val y

doAp :: (Show a, Member (Error String) effs) => Value a -> Value a -> Eff effs (Value a)
doAp (VCLOS body) arg =
  val (instantiate1 (embedValue arg) body)
doAp (VNeutral v) arg = pure $ VNeutral (NApp v arg)

lam :: Eq a => a -> Exp a -> Exp a
lam v b = ELam (abstract1 v b)

-- The type doesn't tell you whether idTm contains fvs
idTm :: Exp Char
idTm = lam 'x' (EV 'x')

idTm' :: Exp a
idTm' = ELam (Scope (EV (B ())))

constTm :: Exp Char
constTm = lam 'x' (lam 'y' (EV 'x'))
-- ELam (Scope (ELam (Scope (EV (F (EV (B ())))))))

constTm' :: Exp a
constTm' = ELam (Scope (ELam (Scope (EV (F (EV (B ())))))))

readBackNeutral ::
  Members '[Fresh, Error String] effs
  => Neutral Int -> Eff effs (Exp Int)
readBackNeutral (NV v) = pure $ EV v
readBackNeutral (NApp n v) =
  EApp <$>
  readBackNeutral n <*>
  readBackValue v

readBackValue ::
  Members '[Fresh, Error String] effs
  => Value Int -> Eff effs (Exp Int)
readBackValue (VCLOS b) = do
  v  <- fresh
  bv  <- val (instantiate1 (EV v) b :: Exp Int)
  be  <- readBackValue bv
  pure $ ELam (abstract1 v be)

readBackValue (VNeutral n) = readBackNeutral n

norm :: Exp Int -> Either String (Exp Int)
norm e = fst $ run (runFresh 0 (runError (val e >>= readBackValue)))

-- >>> norm (EApp (EApp constTm' idTm') idTm')
-- Right (ELam (Scope (EV (B ()))))



main :: IO ()
main = putStrLn "Hello, Haskell!"
