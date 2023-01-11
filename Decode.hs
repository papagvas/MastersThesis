{-# LANGUAGE ScopedTypeVariables #-}

module Decode where

import Clash.Prelude
import Encode

decode
  :: forall na mb nb a . na ~ nb
  => 1 <= nb
  => (Eq a, Floating a)
  => KnownNat nb
  => KnownNat mb

  => Vec na a
  -> Mat mb nb Integer
  -> Mat mb nb a
decode r h = e
  where
    l = zipWith (+) r (map sum $ transpose e)
    e = over (\el -> log $ (1 + el) / (1 - el)) $ map (\row -> smap (\j el' -> if el' /= 0 then rowV2C j row else 0) row) m'
    m' = over (tanh . (/ 2)) m
    m = kronekerProd r' h'
    r' = (transpose ones) `mmult` (r :> Nil)
    h' = over fromInteger h
    ones = (repeat 1.0 :: Vec mb a) :> Nil

    kronekerProd = zipWith $ zipWith (*)

rowV2C
  :: forall i j a na . (Eq a, Num a)

  => 1 <= na
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

over :: (a -> c) -> Mat m n a -> Mat m n c
over = fmap fmap fmap
