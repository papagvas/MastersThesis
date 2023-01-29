module LDPC.Decode where

import Clash.Prelude

import Data.Matrix.Operations qualified as M
import Data.FixedOps qualified as F

decode
  :: forall na mb nb a . na ~ nb
  => 1 <= nb
  => 1 <= mb
  => (Ord a, Eq a, Fractional a)
  => KnownNat nb
  => KnownNat mb
  => Vec na a
  -> M.Mat mb nb Integer
  -> Vec na Int
decode r h = nrz l
  where
    l = zipWith (+) r (map sum $ transpose e)
    e = M.over (\el -> F.log el) $ map (\row -> smap (\j el' -> if el' /= 0 then rowV2C j row else 0) row) m'
    m' = M.over (F.tanh . (/ 2)) m
    m = M.kronekerProd r' h'
    r' = (transpose ones) `M.mmult` (r :> Nil)
    h' = M.over fromInteger h
    ones = (repeat 1.0 :: Vec mb a) :> Nil

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

nrz
  :: (Ord a, Num a)
  => Vec na a
  -> Vec na Int
nrz = map (\el -> if el < 0 then 1 else 0)
