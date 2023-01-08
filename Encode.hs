module Encode where

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

encode
  :: na ~ mb
  => 1 <= mb

  => KnownNat mb
  => KnownNat nb

  => Vec na Int
  -> Mat mb nb Int
  -> Vec nb Int
encode msg genMat = head $ (msg :> Nil) `mmult` genMat
