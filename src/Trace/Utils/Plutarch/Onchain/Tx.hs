
module Trace.Utils.Plutarch.Onchain.Tx where

import Plutarch.Prelude
import Plutarch.Api.V1.Tx (PTxOutRef)

instance PEq PTxOutRef where
    ref1' #== ref2' = 
        pletFields @'["id","idx"] ref1' $ \ref1 ->
            pletFields @'["id","idx"] ref2' $ \ref2 ->
                hrecField @"idx" ref1 #== hrecField @"idx" ref2 #&&
                hrecField @"id" ref1 #== hrecField @"id" ref2
