module Data.FixedOps where

import Clash.Prelude

pow
  :: forall (n :: Nat) a
  . (KnownNat n, 1 <= n, Num a)
  => a -> a
pow x = product (repeat x :: Vec n a)

tanh :: Fractional a => a -> a
tanh x = x - pow @3 x / 3 + 2 * pow @5 x / 15 - 17 * pow @7 x / 315

log :: Fractional a => a -> a
log x = 2 * x + 2 * pow @3 x / 3 + 2 * pow @5 x / 5 + 2 * pow @7 x / 7 + 2 * pow @9 x / 9 + 2 * pow @11 x / 11
