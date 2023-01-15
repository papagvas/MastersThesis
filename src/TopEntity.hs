module TopEntity (topEntity)
  where

import Clash.Prelude
import Clash.Annotations.TH

import LDPC.Decode
import Data.Matrix.Operations

h :: Mat 4 6 Integer
h = (1 :> 1 :> 0 :> 1 :> 0 :> 0 :> Nil)
  :> (0 :> 1 :> 1 :> 0 :> 1 :> 0 :> Nil)
  :> (1 :> 0 :> 0 :> 0 :> 1 :> 1 :> Nil)
  :> (0 :> 0 :> 1 :> 1 :> 0 :> 1 :> Nil)
  :> Nil

topEntity
  :: "demod" ::: Signal System (Vec 6 Float)
  -> "msg" ::: Signal System (Vec 6 Int)
topEntity demoded = fmap (`decode` h) demoded

makeTopEntity 'topEntity
