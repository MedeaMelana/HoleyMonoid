{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module ContSt (
  -- * The state continuation
  ContSt, runContSt,
  
  -- * Building continuations
  contId, contArr, mapContSt
  
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Monoid



-- Functor building blocks.

newtype Id a = Id a
instance Functor Id where
  fmap f (Id x) = Id (f x)

newtype Arr a b = Arr (a -> b)
instance Functor (Arr a) where
  fmap f (Arr g) = Arr (f . g)

newtype (:.:) f g a = Comp (f (g a))
instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp fga) = Comp (fmap (fmap f) fga)



-- Unwrapping of functors.

class (Functor f) => Apply f a b | f -> a b where
  apply :: f a -> b

instance Apply Id a a where
  apply (Id a) = a

instance Apply (Arr a) b (a -> b) where
  apply (Arr f) = f

instance (Apply f b c, Apply g a b) => Apply (f :.: g) a c where
  apply (Comp fga) = apply (fmap apply fga)



-- The Format category.

-- | A continuation-style monoid wrapper. Composition is possible through the ('.') operator in type class 'Category'.
data ContSt m a b where
  ContSt :: Apply f a b => f m -> ContSt m a b

instance Monoid m => Monoid (ContSt m b b) where
  mempty = id
  mappend = (.)

instance Monoid m => Category (ContSt m) where
  id = ContSt (Id mempty)
  ContSt f . ContSt g = ContSt (f <> g)

(<>) :: (Functor f, Functor g, Monoid m) => f m -> g m -> (f :.: g) m
f <> g = Comp (fmap (\s -> fmap (mappend s) g) f)

-- | Wrap a constant.
contId :: m -> ContSt m b b
contId = ContSt . Id

-- | Expect an argument.
contArr :: (a -> m) -> ContSt m b (a -> b)
contArr = ContSt . Arr

-- | Map a continuation to a different monoid.
mapContSt :: (m -> n) -> ContSt m a b -> ContSt n a b
mapContSt g (ContSt f) = ContSt (fmap g f)

-- | Run the continutation, producing the resulting monoid.
runContSt :: ContSt m m b -> b
runContSt (ContSt f) = apply f
