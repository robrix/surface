module Surface.Usage where

import Data.Semigroup
import Data.Semiring

data Usage = Zero | One | More
  deriving (Bounded, Enum, Eq, Ord, Show)


instance Semigroup Usage where
  Zero <> a = a
  a <> Zero = a
  _ <> _ = More

instance Monoid Usage where
  mempty = Zero
  mappend = (<>)

instance Semiring Usage where
  one = One
  Zero >< _ = Zero
  _ >< Zero = Zero
  One >< One = One
  _ >< _ = More
