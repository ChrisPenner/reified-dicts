{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Reify where

import Control.Lens
import Data.Kind
import Data.Typeable
import Data.GenericLens.Internal
import Data.Constraint
import GHC.TypeLits hiding (someNatVal)
import GHC.TypeNats
import Data.Generics.Product
import GHC.Natural
import GHC.Generics
import Control.Applicative
import Data.Maybe

type family AllC (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllC c '[] =  ()
  AllC c (x : rest) = (c x, AllC c rest)

type family Contained x xs where
  Contained x '[] = False ~ True
  Contained x (x:_) = ()
  Contained x (_:xs) = Contained x xs


class Reified k where
  type Val k = result | result -> k
  type Proofs k :: (k -> Constraint)
  reify :: Val k -> (forall (s :: k). (Proofs k s) => Proxy s -> r) -> r
  rEq :: (Proofs k a, Proofs k b) => Proxy (a :: k) -> Proxy (b :: k) -> Maybe (a :~: b)

instance Reified Symbol where
  type Val Symbol = String
  type Proofs Symbol = KnownSymbol
  reify :: String -> (forall s. KnownSymbol s => Proxy s -> r) -> r
  reify s f = go (someSymbolVal s)
    where
      go (SomeSymbol p) = f p
  rEq = sameSymbol

instance Reified Nat where
  type Val Nat = Natural
  type Proofs Nat = KnownNat
  reify :: Natural -> (forall n. KnownNat n => Proxy n -> r) -> r
  reify n f = go (someNatVal n)
    where
      go (SomeNat p) = f p
  rEq = sameNat


checkMatch :: forall o r. Reified k => (Proofs k o) => Proxy (o :: k) -> Val k -> Bool
checkMatch o s = reify @k s go
  where
    go :: forall s. Proofs k s => Proxy s -> Bool
    go p = isJust $ rEq p o

class (Reified k) => CheckEach (c :: k -> Constraint) (xs :: [k]) where
  findMatch :: Alternative f => (forall x. c x => Proxy x -> r) -> Val k -> f r

instance Reified k => CheckEach c ('[] :: [k]) where
  findMatch _ _ = empty

instance (CheckEach c xs, AllC (Proofs k) (x:xs), AllC c (x:xs)) => CheckEach c ((x:xs) :: [k]) where
  findMatch f s =
      if checkMatch (Proxy @x) s
         then pure $ f (Proxy @x)
         else findMatch @k @c @xs f s
