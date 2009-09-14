{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}


module ContSt (
  -- * The state continuation
  ContSt, runContSt,
  
  -- * Building continuations
  now, later, mapContSt
  
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Monoid



-- Functor building blocks.

newtype Id a = Id { runId :: a }
instance Functor Id where
  fmap f (Id x) = Id (f x)

newtype Arr a b = Arr { runArr :: a -> b }
instance Functor (Arr a) where
  fmap f (Arr g) = Arr (f . g)

newtype (:.:) f g a = Comp { runComp :: f (g a) }
instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp fga) = Comp (fmap (fmap f) fga)



-- Unwrapping of functors.

data Apply f a b where
  ApplyId   :: Apply Id b b
  ApplyArr  :: Apply (Arr a) b (a -> b)
  ApplyComp :: Functor f => Apply f b c -> Apply g a b -> Apply (f :.: g) a c

apply :: Apply f a b -> f a -> b
apply ApplyId         = runId
apply ApplyArr        = runArr
apply (ApplyComp f g) = apply f . fmap (apply g) . runComp



-- The Format category.

-- | A continuation-style monoid wrapper. Composition is possible through the ('.') operator in type class 'Category'.
data ContSt m a b where
  ContSt :: Functor f => Apply f a b -> f m -> ContSt m a b

instance Monoid m => Category (ContSt m) where
  id = ContSt ApplyId (Id mempty)
  ContSt af f . ContSt ag g = ContSt (ApplyComp af ag) (f <> g)

(<>) :: (Functor f, Functor g, Monoid m) => f m -> g m -> (f :.: g) m
f <> g = Comp (fmap (\s -> fmap (mappend s) g) f)

-- | Wrap a constant.
now :: m -> ContSt m b b
now = ContSt ApplyId . Id

-- | Expect an argument.
later :: (a -> m) -> ContSt m b (a -> b)
later = ContSt ApplyArr . Arr

-- | Map a continuation to a different monoid.
mapContSt :: (m -> n) -> ContSt m a b -> ContSt n a b
mapContSt g (ContSt af f) = ContSt af (fmap g f)

-- | Run the continutation, producing the resulting monoid.
runContSt :: ContSt m m b -> b
runContSt (ContSt af f) = apply af f
