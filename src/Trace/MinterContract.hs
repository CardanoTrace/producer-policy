{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use ++" #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Trace.MinterContract where

import qualified    Ledger
import qualified    PlutusTx
import qualified    PlutusTx.Builtins           as Builtins
import qualified    PlutusTx.Builtins.Class     as Builtins.Class
import              Plutus.V1.Ledger.Api        as LedgerApi hiding ( UnsafeFromData )
import              PlutusTx.IsData.Class ( UnsafeFromData )
import              PlutusTx.Prelude            as PTxP hiding (check)
import              Ledger (
        validatorHash, mkValidatorScript,
        scriptAddress
    )
import qualified    Ledger.Contexts             as Ctx hiding ( UnsafeFromData )
import qualified    Ledger.Typed.Scripts        as Scripts
import qualified    Plutus.V1.Ledger.Value      as Value

import qualified Data.ByteString.Char8          as BS8
import Trace.Plutus.Utils (showToTokenName)
import qualified PlutusCore
import qualified Plutus.V1.Ledger.Ada as Ada
import Ledger.Typed.Scripts (mkForwardingMintingPolicy)


-- must use ```data``` since it is an entirely new type
-- needs {-# LANGUAGE TypeFamilies #-}
data TypedContractValidator
instance Scripts.ValidatorTypes     TypedContractValidator where
    type instance DatumType         TypedContractValidator = Integer
    type instance RedeemerType      TypedContractValidator = ()

{-# INLINABLE check #-}
-- | Checks a 'Bool' and aborts if it is false.
check :: Bool -> ()
check b = if b then () else error () -- checkHasFailedError

{-
mkTypedValidator

:: CompiledCode (ValidatorType a)	-- compiled validator script

-- a **compiled function** that goes from the typed validator to the untyped with ```BuiltinData``` as args
-> CompiledCode (ValidatorType a -> WrappedValidatorType)

-- finally returns a ```TypedValidator a``` where ```a``` is our own data-type
-> TypedValidator a	 
-}

{--
IN ORDER TO PARAMETRIZE UNTYPED

$$(PlutusTx.compile [|| someParam' -> typedToUntyped ( minterContractValidator_logic someParam' ) ||])
`PlutusTx.applyCode` PlutusTx.liftCode someParam

--}
minterContractTypedValidator :: Value.AssetClass -> Scripts.TypedValidator TypedContractValidator
minterContractTypedValidator threadTokenAssetClass' = Scripts.mkTypedValidator @TypedContractValidator

    (
        $$(PlutusTx.compile [|| minterContractValidator_logic ||])
        -- ```Value.AssetClass``` has already an instatnce for ```PlutusTx.LiftCode```
        `PlutusTx.applyCode` PlutusTx.liftCode threadTokenAssetClass'
    )

    -- wrapper to ( BuiltinData ->  BuiltinData ->  BuiltinData -> () )
    $$(PlutusTx.compile [|| typedToUntyped ||])

    where

        {-# INLINABLE minterContractValidator_logic #-}
        minterContractValidator_logic :: Value.AssetClass -> ( Integer -> () -> Ctx.ScriptContext -> Bool )
        minterContractValidator_logic threadTokenAssetClass nftToMint_number producerName ctx =
                traceIfFalse "thread-token not kept"    isThreadTokenKept           PTxP.&&
                traceIfFalse "datum not incrementing"   isDatumIncrementing
                -- the Validator does not check for the minted value, that's job for the minting policy
                -- singleMintedValue 
            where

                outputFromSelfToSelf :: TxOut
                outputFromSelfToSelf = case Ctx.getContinuingOutputs ctx of
                    [ singleOutput ] -> singleOutput
                    _multipleOuts    -> traceError "expected 1 input and 1 output to self"


                isThreadTokenKept :: PTxP.Bool
                isThreadTokenKept =
                    Value.assetClassValueOf -- Get the quantity of the given AssetClass class in the Value
                        ( txOutValue outputFromSelfToSelf ) -- pass the output form-self-to-self value
                        threadTokenAssetClass -- checks for the thread-token in the passed value
                    == 1 -- the resulting value found must be equal to 1 (NFT)

                nextDatumHash :: DatumHash
                nextDatumHash = case Ledger.txOutDatumHash outputFromSelfToSelf of
                    Just datumHash -> datumHash
                    Nothing -> traceError "datum not preserved, missing in output"

                isDatumIncrementing :: PTxP.Bool
                isDatumIncrementing = case Ctx.findDatum (nextDatumHash) (scriptContextTxInfo ctx) of
                  Just datum -> PlutusTx.toBuiltinData datum == Builtins.mkI ( nftToMint_number + 1 )
                  Nothing -> traceError "datumHash found but no Datum present"
                


        {-# INLINEABLE typedToUntyped #-}
        typedToUntyped ::
                ( Integer     -> ()                -> Ctx.ScriptContext -> Bool)
             -> ( BuiltinData -> BuiltinData       -> BuiltinData       -> () )
        typedToUntyped = Scripts.wrapValidator

{--
Added additional step in oreder to get the ```Validator``` type starting from ```TypedValidator a``` when using the typed version
                   -}
-- add a space here ^ to comment out
minterContractValidator :: Value.AssetClass -> Scripts.Validator
minterContractValidator = Scripts.validatorScript . minterContractTypedValidator
--}

minterContractAddress :: Value.AssetClass -> Ledger.Address
minterContractAddress = scriptAddress . minterContractValidator

minterContractHash :: Value.AssetClass -> Ledger.ValidatorHash
minterContractHash = validatorHash . minterContractValidator