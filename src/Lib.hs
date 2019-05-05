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

class (HasField' fieldName t String) => HasFieldX t (fieldName :: Symbol)
instance (HasField' fieldName t String) => HasFieldX t fieldName

data MyType = MyType {name :: String, blah :: String} deriving Generic

exType :: MyType
exType = MyType "my name" "my blah"

mkGetter :: String -> Maybe (MyType -> String)
mkGetter s = findMatch @Symbol @(HasFieldX MyType) @'["name"] getField' s
  where
    getField' :: forall x. (HasFieldX MyType x) => Proxy x -> (MyType -> String)
    getField' _ = getField @x
