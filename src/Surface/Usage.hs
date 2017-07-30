module Surface.Usage where

import Control.Exception
import Data.Semigroup
import Data.Semiring

data Usage = Zero | One | Multiple
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Num Usage where
  Zero + a = a
  a + Zero = a
  _ + _ = Multiple

  Zero * _ = Zero
  _ * Zero = Zero
  One * One = One
  _ * _ = Multiple

  abs = id

  signum Zero = Zero
  signum _ = One

  negate Zero = Zero
  negate _ = throw Underflow

  fromInteger n | n < 0 = throw Underflow
                | n == 0 = Zero
                | n == 1 = One
                | otherwise = Multiple

instance Semigroup Usage where
  (<>) = (+)

instance Monoid Usage where
  mempty = Zero
  mappend = (+)

instance Semiring Usage where
  one = One
  (><) = (*)
