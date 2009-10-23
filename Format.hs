{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Format (
  -- * The Format type
  Format, showsf, showf,
  lits, args, mapFormat,
  
  -- * Derived formatters
  lit, str, arg, num, sh,
  Dir(..), align,
  
  -- * Format-like types
  FormatLike(..), (%),
  
  ) where


import qualified Data.HoleyMonoid as HM

import Prelude hiding (id, (.))
import Control.Category
import Data.Monoid



-- The Format category.

newtype Format a b = Format { runFormat :: HM.HoleyMonoid (Endo String) a b }
  deriving Category

-- | Compose two 'FormatLike's.
(%) :: (FormatLike x b c, FormatLike y a b) => x -> y -> Format a c
f % g = toFormat f . toFormat g

-- | Format according to formatter @x@, producing a 'ShowS'.
showsf :: FormatLike x ShowS b => x -> b
showsf = HM.run . HM.map appEndo . runFormat . toFormat

-- | Format according to formatter @x@, producing a 'String'.
showf :: FormatLike x String b => x -> b
showf = HM.run . HM.map (($ "") . appEndo) . runFormat . toFormat



-- Fundamental format combinators.

-- | Output a constant string.
lits :: ShowS -> Format b b
lits = Format . HM.now . Endo

-- | Expect an argument that can be converted to a 'ShowS'.
args :: (a -> ShowS) -> Format b (a -> b)
args f = Format (HM.later (Endo . f))

-- | Transform the output of a formatter.
mapFormat :: (ShowS -> ShowS) -> Format a b -> Format a b
mapFormat g (Format f) = Format (HM.map (Endo . g . appEndo) f)



-- Derived combinators.

-- | Output a constant string.
lit :: String -> Format b b
lit = lits . showString

-- | Expect an argument that can be converted to a string.
arg :: (a -> String) -> Format b (a -> b)
arg f = args (showString . f)

-- | Expect a string.
str :: Format b (String -> b)
str = arg id

-- | Expect a number.
num :: Num a => Format b (a -> b)
num = sh

-- | Expect a 'show'able argument.
sh :: Show a => Format b (a -> b)
sh = args shows

-- | Direction (left or right) used for 'AlignF'.
data Dir = L | R

align' :: Dir -> Int -> ShowS -> ShowS
align' dir wid f =
  case dir of
    L -> f . g
    R -> g . f
  where
    len = length (f "")
    g = if len < wid then showString (replicate (wid - len) ' ') else id

-- | Print a format aligned left or right within a column of the given width.
align :: FormatLike x a b => Dir -> Int -> x -> Format a b
align dir wid = mapFormat (align' dir wid) . toFormat


-- Format-like types.

-- | Captures types that are convertible to a 'Format'.
class FormatLike x a b | x -> a b where
  toFormat :: x -> Format a b

instance FormatLike (Format a b) a b where
  toFormat = id

instance FormatLike Char a a where
  toFormat c = lit [c]

instance FormatLike x a a => FormatLike [x] a a where
  toFormat = foldr ((.) . toFormat) id

instance (FormatLike t1 b c, FormatLike t2 a b) =>
    FormatLike (t1, t2) a c where
  toFormat (x, y) = toFormat x . toFormat y

instance (FormatLike t1 c d, FormatLike t2 b c, FormatLike t3 a b) =>
    FormatLike (t1, t2, t3) a d where
  toFormat (x, y, z) = toFormat x . toFormat y . toFormat z

instance (FormatLike t1 d e, FormatLike t2 c d, FormatLike t3 b c, FormatLike t4 a b) =>
    FormatLike (t1, t2, t3, t4) a e where
  toFormat (x1, x2, x3, x4) = toFormat x1 . toFormat x2 . toFormat x3 . toFormat x4
