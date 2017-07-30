module Surface.Usage where

import Data.Semigroup
import Data.Semiring

data Usage = Zero | One | Multiple
  deriving (Bounded, Enum, Eq, Ord, Show)


instance Semigroup Usage where
  Zero <> a = a
  a <> Zero = a
  _ <> _ = Multiple

instance Monoid Usage where
  mempty = Zero
  mappend = (<>)

instance Semiring Usage where
  one = One
  Zero >< _ = Zero
  _ >< Zero = Zero
  One >< One = One
  _ >< _ = Multiple
