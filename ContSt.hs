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



-- The Format category.

-- | A continuation-style monoid wrapper. Composition is possible through the ('.') operator in type class 'Category'.
data ContSt m r a where
  ContSt :: Functor f => (f r -> a) -> f m -> ContSt m r a

instance Monoid m => Category (ContSt m) where
  id = now mempty
  ContSt apf f . ContSt apg g =
    ContSt (apf . fmap apg . runComp) (f <> g)

(<>) :: (Functor f, Functor g, Monoid m) => f m -> g m -> (f :.: g) m
f <> g = Comp (fmap (\s -> fmap (mappend s) g) f)

-- | Wrap a constant.
now :: m -> ContSt m r r
now = ContSt runId . Id

-- | Expect an argument.
later :: (a -> m) -> ContSt m r (a -> r)
later = ContSt runArr . Arr

-- | Map a continuation to a different monoid.
mapContSt :: (m -> n) -> ContSt m r a -> ContSt n r a
mapContSt g (ContSt ap f) = ContSt ap (fmap g f)

-- | Run the continuation, producing the resulting monoid.
runContSt :: ContSt m m a -> a
runContSt (ContSt af f) = af f
