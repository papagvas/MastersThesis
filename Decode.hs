{-# LANGUAGE ScopedTypeVariables #-}

module Decode where

import Clash.Prelude
import Encode

decode
  :: forall na mb nb a . na ~ nb
  => Fractional a

  => KnownNat nb
  => KnownNat mb

  => Vec na a
  -> Mat mb nb Integer
  -> Mat mb nb a
decode r h = undefined
  where
--    e = 
    m = kronekerProd r' h'
    r' = (transpose ones) `mmult` (r :> Nil)
    h' = fmap fmap fmap fromInteger h
    ones = (repeat 1.0 :: Vec mb a) :> Nil

    kronekerProd = zipWith $ zipWith (*)

rowV2C
  :: forall i j a na . (Eq a, Num a)

  => 1 <= na
  => j <= na
  => KnownNat j
  => KnownNat na
  
  => SNat j
  -> Vec na a
  -> a
rowV2C j row = product $ smap (\j' el -> filterOut j j' el) row

filterOut
  :: (Eq a, Num a)

  => SNat j
  -> SNat j'
  -> a
  -> a
filterOut _ _   0 = 1
filterOut j j' el = if snatToNum j == snatToNum j' then 1 else el
