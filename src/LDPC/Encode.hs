module LDPC.Encode where

import Clash.Prelude

import Data.Matrix.Operations qualified as M

encode
  :: na ~ mb
  => 1 <= mb

  => KnownNat mb
  => KnownNat nb

  => Vec na Int
  -> Mat mb nb Int
  -> Vec nb Int
encode msg genMat = head $ (msg :> Nil) `M.mmult` genMat
