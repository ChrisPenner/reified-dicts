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

module Lib where

import Reify
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
-- HasField' o MyType String

checkMatch :: forall o r. Reified k => (Proofs k o) => Proxy (o :: k) -> Val k -> Bool
checkMatch o s = reify @k s go
  where
    go :: forall s. Proofs k s => Proxy s -> Bool
    go p = isJust $ rEq p o

class (HasField' fieldName t String) => HasFieldX t (fieldName :: Symbol)
instance (HasField' fieldName t String) => HasFieldX t fieldName

class (Reified k) => CheckEach (c :: k -> Constraint) (xs :: [k]) where
  findMatch :: Alternative f => (forall x. c x => Proxy x -> r) -> Val k -> f r

instance Reified k => CheckEach c ('[] :: [k]) where
  findMatch _ _ = empty

instance (CheckEach c xs, AllC (Proofs k) (x:xs), AllC c (x:xs)) => CheckEach c ((x:xs) :: [k]) where
  findMatch f s =
      if checkMatch (Proxy @x) s
         then pure $ f (Proxy @x)
         else findMatch @k @c @xs f s

data MyType = MyType {name :: String, blah :: String} deriving Generic

exType :: MyType
exType = MyType "my name" "my blah"

mkGetter :: String -> Maybe (MyType -> String)
mkGetter s = findMatch @Symbol @(HasFieldX MyType) @'["name"] getField' s
  where
    getField' :: forall x. (HasFieldX MyType x) => Proxy x -> (MyType -> String)
    getField' _ = getField @x
