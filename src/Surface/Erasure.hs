module Surface.Erasure where

import Data.Semigroup
import Data.Semiring

data Erasure = Erased | Present
  deriving (Bounded, Enum, Eq, Ord, Show)


instance Semigroup Erasure where
  Erased <> a = a
  a <> Erased = a
  _ <> _ = Present

instance Monoid Erasure where
  mempty = Erased
  mappend = (<>)

instance Semiring Erasure where
  one = Present
  Present >< a = a
  a >< Present = a
  _ >< _ = Erased
