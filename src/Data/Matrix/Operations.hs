module Data.Matrix.Operations where

import Clash.Prelude

type Mat ma na a = Vec ma (Vec na a)

mmult
  :: (Num a)

  => na ~ mb
  => 1 <= mb

  => KnownNat mb
  => KnownNat nb

  => Mat ma na a
  -> Mat mb nb a
  -> Mat ma nb a
mmult mA mB = result
  where
    mBT      = transpose mB
    dot a b  = sum $ zipWith (*) a b
    result   = map (\ar -> dot ar <$> mBT) mA

over :: (a -> c) -> Mat m n a -> Mat m n c
over = fmap fmap fmap

kronekerProd :: Num a => Mat ma na a -> Mat ma na a -> Mat ma na a
kronekerProd = zipWith $ zipWith (*)
