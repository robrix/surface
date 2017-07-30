module Surface.Erasure where

import Control.Exception

data Erasure = Erased | Present
  deriving (Eq, Ord, Show)

instance Num Erasure where
  Erased + a = a
  a + Erased = a
  _ + _ = Present

  Present * a = a
  a * Present = a
  _ * _ = Erased

  abs = id

  signum = id

  negate Erased = Erased
  negate Present = throw Underflow

  fromInteger n | n == 0 = Erased
                | n == 1 = Present
                | otherwise = throw Underflow
