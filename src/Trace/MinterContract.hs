{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Trace.MinterContract (
    minterContractHash
) where

import qualified Ledger
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger (
        validatorHash, mkValidatorScript,
        scriptAddress
    )
import qualified Ledger.Contexts as Ctx


type RawDatum =  BuiltinData
type RawRedeemer =  BuiltinData
type RawCtx = BuiltinData


minterContractValidator :: Ledger.Validator
minterContractValidator = mkValidatorScript
    $$(PlutusTx.compile [|| minterContractValidator_logic ||])

    where

        {-# INLINABLE minterContractValidator_logic #-}
        minterContractValidator_logic :: RawDatum -> RawRedeemer -> RawCtx -> ()
        minterContractValidator_logic _datum _redeemer _ctx = ()


minterContractAddress :: Ledger.Address
minterContractAddress = scriptAddress minterContractValidator

minterContractHash :: Ledger.ValidatorHash
minterContractHash = validatorHash minterContractValidator