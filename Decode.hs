{-# LANGUAGE ScopedTypeVariables #-}

module Decode where

import Clash.Prelude
import Encode

decode
  :: forall na mb nb a . na ~ nb
  => (Fractional a)
  
  => KnownNat nb
  => KnownNat mb
  
  => Vec na a
  -> Mat mb nb Integer
  -> Mat mb nb a
decode r h = kronekerProd m h'
  where
    m = transpose $ (transpose $ r :> Nil) `mmult` ones 
    ones = (repeat 1.0 :: Vec mb a) :> Nil
    kronekerProd = zipWith $ zipWith (*)
    h' = fmap fmap fmap fromInteger h
