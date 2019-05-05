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

import Control.Lens
import Data.Kind
import Data.Typeable
import Data.GenericLens.Internal
import Data.Constraint
import GHC.TypeLits
import Data.Generics.Product
import GHC.Generics
import Control.Applicative

type family AllC (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllC c '[] =  ()
  AllC c (x : rest) = (c x, AllC c rest)

class (KnownSymbol o) => GetProof typ o where
  getProof :: forall fieldName. KnownSymbol fieldName => Proxy fieldName -> Proxy o -> Maybe (fieldName :~: o)

instance ( KnownSymbol o
         , KnownSymbol fieldName
         , Contained o (CollectFieldsOrdered (Rep MyType))
         ) => GetProof MyType o where
    getProof = sameSymbol

type family Contained x xs where
  Contained x '[] = False ~ True
  Contained x (x:_) = ()
  Contained x (_:xs) = Contained x xs

run :: forall o. (KnownSymbol o, HasField' o MyType String) => Proxy o -> String -> Maybe (MyType -> String)
run o s = case someSymbolVal s of
    SomeSymbol x -> case sameSymbol x o of
        Just Refl -> Just (getField @o)
        Nothing -> Nothing

r = run (Proxy @"name") "name"

class (AllC KnownSymbol xs) => TryAll xs where
  tryIt :: String -> Maybe (MyType -> String)

instance TryAll '[] where
  tryIt _ = Nothing

instance (TryAll xs, AllC KnownSymbol (x:xs), HasField' x MyType String) => TryAll (x:xs) where
  tryIt s = run (Proxy @x) s <|> tryIt @xs s

data MyType = MyType {name :: String, blah :: String} deriving Generic

exType = MyType "my name" "my blah"

result :: String -> Maybe (MyType -> String)
result = tryIt @(CollectFieldsOrdered (Rep MyType))

getter ::  MyType -> String -> Maybe String
getter t s = fmap ($t) (tryIt @(CollectFieldsOrdered (Rep MyType)) s)
