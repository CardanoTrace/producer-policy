{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Trace.MinterContract.Redeemer (
        MinterContractRedeemer(..)
    ) where

import Plutarch.Prelude
import Plutarch.Internal.PlutusType
import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Plutarch.DataRepr

data MinterContractRedeemer (s :: S)
    = UpdatePrice 
    | IncrementCounter
    deriving stock (GHC.Generic)
    deriving anyclass (Generic, PlutusType)