{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}

-- allows PlutusTx.makeLift to work
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- allows TemplateHaskell to work well with Plutus
{-# LANGUAGE DataKinds #-}

module Trace.Policy where

import Trace.MinterContract ( minterContractHash )


import qualified    Ledger
import qualified    Ledger.Contexts             as Ctx

import qualified    Plutus.V1.Ledger.Scripts    as Scripts
import qualified    Ledger.Typed.Scripts.MonetaryPolicies as Scripts
import qualified    Plutus.V1.Ledger.Value      as Value
import qualified    Plutus.V1.Ledger.Api        as LedgerApi

import qualified    PlutusTx
import qualified    PlutusTx.Builtins           as Builtins
import              PlutusTx.Prelude            as P
import              PlutusTx.Builtins.Class     as Builtins ( stringToBuiltinString, stringToBuiltinByteString )

import qualified Ledger.Value as Value
import qualified PlutusTx.Builtins.Internal as InBuiltins


data TokenData = TokenData
    {
        tdValidatorHash :: !Scripts.ValidatorHash,
        tdThreadSymbol :: !Value.CurrencySymbol
    }
PlutusTx.makeLift ''TokenData

PlutusTx.makeIsDataIndexed ''TokenData
    [( 'TokenData, 0 )]

{--
data Action = Mint | Burn
PlutusTx.makeIsDataIndexed ''Action 
    [
        ( 'Mint, 0 ),
        ( 'Burn, 1 )
    ]
--}

-- the policy script that actually represents the NFT policy
nftPolicy_logic :: TokenData -> () -> Ctx.ScriptContext -> P.Bool
nftPolicy_logic tokenData _producerName ctx =
        P.traceIfFalse "validator not running" isValidatorRunning P.&&
        P.traceIfFalse "mint failed" checkMint
    where
        txInfo :: Ledger.TxInfo
        txInfo = Ctx.scriptContextTxInfo ctx

        flattenMint :: [(Value.CurrencySymbol, Value.TokenName, P.Integer)]
        flattenMint = Value.flattenValue (LedgerApi.txInfoMint txInfo)


        checkMint :: P.Bool
        checkMint =
            check flattenMint nftNumber
            where
                check :: [(Value.CurrencySymbol, Value.TokenName, P.Integer)] -> P.Integer -> P.Bool
                check mintedValue nftNum =
                    case mintedValue of
                        [] -> P.trace "empty minted value, returns True" P.True

                        [( mintedCurrencySym , mintedTokenName, mintedAmount )] ->

                            P.traceIfFalse "single value minted is different than expected"

                            ( P.traceIfFalse "MW: currencySymbol" (mintedCurrencySym   == Ctx.ownCurrencySymbol ctx) ) &&
                            
                                P.traceIfFalse "MW: tokenName"
                                ( P.trace
                                    (
                                        "got minted token name " <> InBuiltins.decodeUtf8 (Value.unTokenName mintedTokenName) <>
                                        "should be " <> {- producerName <> " - -}"Trace identifier #" <> InBuiltins.decodeUtf8 (integerToByteString nftNumber)
                                    )
                                    (
                                        mintedTokenName == Value.TokenName
                                            ({-producerName <> " - -}"Trace identifier #" P.<> integerToByteString nftNumber)
                                    )
                                ) &&
                            P.traceIfFalse "MW: amount" ( mintedAmount == 1 )

                        _ -> P.traceError "minting more than one nft"


        integerToByteString :: Integer -> LedgerApi.BuiltinByteString
        integerToByteString n
            | n == 0 = "0"
            | n == 1 = "1"
            | n == 2 = "2"
            | n == 3 = "3"
            | n == 4 = "4"
            | n == 5 = "5"
            | n == 6 = "6"
            | n == 7 = "7"
            | n == 8 = "8"
            | n == 9 = "9"
            | otherwise = integerToByteString (n `P.divide` 10) P.<> integerToByteString (n `P.modulo` 10)


        isValidatorRunning :: P.Bool
        isValidatorRunning = not $ null validatorInputs

        validatorInputs :: [LedgerApi.TxInInfo]
        validatorInputs =
            let
                txInputs = Ctx.txInfoInputs txInfo

                isOutputOfValidator :: LedgerApi.TxOut -> P.Bool
                isOutputOfValidator LedgerApi.TxOut{
                    LedgerApi.txOutAddress = LedgerApi.Address{ -- get the address o a TxOut
                            LedgerApi.addressCredential = LedgerApi.ScriptCredential vHash --checks that the credentials ha
                        }
                    } = vHash == tdValidatorHash tokenData

                isOutputOfValidator _ = P.False
            in
                filter (isOutputOfValidator . Ctx.txInInfoResolved) txInputs

        -- checkBurn :: P.Bool
        -- checkBurn = all (\(cs,_,am) -> cs == Ctx.ownCurrencySymbol ctx && am == -1) flattenMint

        nftNumber :: P.Integer
        nftNumber = findNftNumber [
                        (
                            Ledger.txOutDatumHash input,
                            Ledger.txOutValue input
                        ) | input <- map Ctx.txInInfoResolved validatorInputs
                    ]
            where

                findNftNumber [] = traceError "no nftNumber found"
                findNftNumber (( Nothing       , _val  ) : restList ) = P.trace "input with no datum hash" $ findNftNumber restList
                findNftNumber (( Just datumHash, value ) : restList ) =
                    -- only takes the validator input that has the thread token for data
                    if not $ isValueTrusted value then findNftNumber restList
                    else
                        case Ledger.findDatum datumHash txInfo of
                            P.Nothing -> P.trace "datum hash with no datum" $ findNftNumber restList
                            P.Just (LedgerApi.Datum datum) ->
                                -- if the ```Data``` is not constructed with ```I``` we should fail the computation anyway
                                ( LedgerApi.unsafeFromBuiltinData datum :: P.Integer )

                isValueTrusted :: Value.Value -> P.Bool
                isValueTrusted val =
                    P.any (\ ( currSym , _tn, amount ) -> currSym == tdThreadSymbol tokenData && amount == 1 ) (Value.flattenValue val)



nftPolicy :: TokenData -> Ledger.MintingPolicy
nftPolicy tokenData = Ledger.mkMintingPolicyScript
    (
        $$(PlutusTx.compile [|| \liftedData -> Scripts.wrapMintingPolicy ( nftPolicy_logic liftedData ) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode tokenData
    )