{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
{-# LANGUAGE BlockArguments #-}


module Trace.ThreadTokenPolicy where

import Plutarch ( compile )
import           Plutarch.Prelude as Pre

import qualified Plutarch.Api.V1 as V1
import qualified Plutarch.Api.V1.Tx as Tx

import Plutus.V1.Ledger.Scripts ( Script, MintingPolicy( MintingPolicy ) )

import Ledger (TxOutRef)
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.Api.V1 (PTxInfo(PTxInfo), PTxInInfo)
import Trace.Utils.Plutarch.Onchain.Ctx (pisUTXOSpent)
import Trace.Utils.Plutarch.Onchain.Value (pvalueOf, pownCurrencySymbol)
import Ledger.Value (TokenName(TokenName))
import qualified Plutarch.Api.V1.Value as Value
import qualified Data.ByteString.Char8 as BS8


threadTokenPolicy ::
    TxOutRef -> TokenName -> Script
threadTokenPolicy outRef' tName' = compile $ threadTokenPolicy_logic # pconstant outRef' # pconstant tName'
    where
        threadTokenPolicy_logic ::
            forall s.
            Term s (
                Tx.PTxOutRef :--> V1.PTokenName
                :--> Pre.PData
                :--> V1.PScriptContext
                :--> PBool
            )
        threadTokenPolicy_logic =
            -- plam defines a plutarch function
            plam $ \ utxoRef tName _redeemer pctx ->
                -- both fields at once
                pletFields @'["txInfo","purpose"] pctx $
                    -- only ```txInfoInputs``` and ```txInfoMint``` needed, extraces those only
                    \ ctx -> pletFields @'["inputs","mint"] ( hrecField @"txInfo" ctx ) $
                        \ txInfos ->

                            pisUTXOSpent
                                # utxoRef 
                                -- get the inputs
                                # pfromData ( hrecField @"inputs" txInfos ) #&&

                            -- IMPORTANT other tokens could be minted in the same transaction
                            -- to the policy this is irrelevant
                            -- the policy just cares that the minted amount of this specific NFT is 1
                            pvalueOf
                                -- get minted value
                                # pfromData ( hrecField @"mint" txInfos ) 
                                # ( 
                                    -- get currecySymbol of this policy
                                    pownCurrencySymbol 
                                        # pfromData ( hrecField @"purpose" ctx )
                                )
                                -- hard-coded constant token name
                                # tName -- pcon (Value.PTokenName (pconstant (BS8.pack "TraceCounterOracle") )) 
                            #== pconstant 1

threadTokenPolicyAsPlutus :: TxOutRef -> TokenName -> MintingPolicy
threadTokenPolicyAsPlutus outRef = MintingPolicy . threadTokenPolicy outRef
