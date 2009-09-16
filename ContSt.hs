module ContSt (
  -- * The state continuation
  ContSt, runContSt,
  
  -- * Building continuations
  now, later, mapContSt
  
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Monoid



-- The Format category.

newtype ContSt m r a = Cont { runCont :: (m -> r) -> a }

now :: m -> ContSt m r r
now a = Cont ($ a)

bind :: ContSt m b c -> (m -> ContSt n a b) -> ContSt n a c
m `bind` f = Cont $ \k -> runCont m (\a -> runCont (f a) k)

later :: (a -> m) -> ContSt m r (a -> r)
later f = Cont (. f)

instance Monoid m => Category (ContSt m) where
  id = now mempty
  f . g = f `bind` \a -> g `bind` \b -> now (a `mappend` b)

mapContSt :: (m -> n) -> ContSt m r a -> ContSt n r a
mapContSt g m = Cont (\k -> runCont m (k . g))

runContSt :: ContSt m m a -> a
runContSt m = runCont m id
