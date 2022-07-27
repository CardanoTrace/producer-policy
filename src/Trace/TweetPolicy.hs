{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}

module Trace.TweetPolicy where

import qualified Plutus.V1.Ledger.Value as Value
    ( CurrencySymbol, TokenName )
import qualified Plutus.V1.Ledger.Scripts as Scripts
    ( ValidatorHash )
import Plutus.V1.Ledger.Scripts ( Script )


import Plutarch
import qualified Plutarch.Api.V1 as V1
import           Plutarch.Prelude as Pre
import Trace.Utils.Plutarch.Onchain.Lists
import Trace.Utils.Plutarch.Onchain.Maybe (pfromJustOrThrow)
import Trace.Utils.Plutarch.Onchain.Value (pvalueOf, pownCurrencySymbol, psingletonValue)
import Trace.Utils.Plutarch.Onchain.Address (pisAddressOfValidator)
import Trace.Utils.Plutarch.Onchain.Require (prequire, prequireContOrThrow, prequireExsitstsDataContOrThrow, prequireExsitstsDataOrThrow, prequireOrThrow)
import qualified Plutarch.Api.V1.Value as Value
import qualified Data.ByteString.Char8 as BS8
import Trace.Utils.Plutarch.Onchain.Ctx.Datum (pfindDatumOrThrow, pDatumToData, PUnsafeSilentFromDatum (punsafeSilentFromDatum))
import Trace.MinterContract.Datum
import Plutarch.Api.V1.Scripts (PDatum)
import Trace.Utils.Plutarch.Onchain.Integer (pintegerToByteString)


tweetPolicyCont ::
    Value.CurrencySymbol -> Value.TokenName -> Scripts.ValidatorHash -> Script
tweetPolicyCont paramThreadTokenSymbol paramThreadTokenName paramValHash =
    compile ( tweetPolicyCont_logic # pconstant paramThreadTokenSymbol # pconstant paramThreadTokenName # pconstant paramValHash )
    where
        tweetPolicyCont_logic ::
            forall s.
            Term
            s
            (
                V1.PCurrencySymbol :--> V1.PTokenName :--> V1.PValidatorHash
                :--> Pre.PData
                :--> V1.PScriptContext
                :--> PBool
            )
        tweetPolicyCont_logic =
            plam $ \ threadTokenSymbol threadTokenName valHash _redeemer pctx -> unTermCont $ do

                ctx  <- tcont $ pletFields @'["txInfo","purpose"] pctx

                txInfos <- tcont $ pletFields @'["inputs","mint","data"] ( hrecField @"txInfo" ctx )

                txInputs <- tcont $ plet (pfromData $ hrecField @"inputs" txInfos)

                validatorInputResolved <- tcont . pletFields @'["value","datumHash"] $
                    -- extracts the input if present, fails if not
                    pfromJustOrThrow "no validator input found" #$
                        -- single input form validator expected
                        pfirstJust
                            # plam (\ elem -> plet ( pfromData $ pfield @"resolved" #$  pfromData elem)
                                (\ inputResolved -> pif ( pisAddressOfValidator # pfromData (pfield @"address" # inputResolved) # valHash )
                                    (pcon . PJust $ inputResolved)
                                    (pcon PNothing)
                                )
                            )
                            # txInputs

                -- extracts the datum on the validator, if there is none fails
                valInDatumHash <- tcont $ prequireExsitstsDataOrThrow "invalid validator input, no datum hash" (pfromData $ hrecField @"datumHash" validatorInputResolved )

                -- checks that the input as the NFT identifier, so that we are sure the given datum hash is from a trusted source
                _ <- tcont $ prequireOrThrow "invalid validator input, no NFT oracle" (
                        plet (pfromData $ hrecField @"value" validatorInputResolved ) $
                            \inputValue ->
                                pvalueOf
                                    -- check the value on the validator input
                                    # inputValue
                                    # threadTokenSymbol
                                    -- hard coded constant as in the thread token minting policy
                                    # threadTokenName -- pcon (Value.PTokenName (pconstant (BS8.pack "TraceCounterOracle") ))
                                #== pconstant 1
                    )

                nftNumber <- tcont. plet . pfromData $ pfield @"nftNumber" #
                        pfromData ((punsafeSilentFromDatum :: Term s PDatum -> Term s ( PAsData MinterContractDatum )) $
                            pfindDatumOrThrow "no datum corresponding to hash" # valInDatumHash #
                                pfromData (hrecField @"data" txInfos))

                mintedVal <- tcont . plet . pfromData $ hrecField @"mint" txInfos

                return . ptraceIfFalse "minted value not singleton" $
                    mintedVal #==
                        psingletonValue
                            # ( pownCurrencySymbol # pfromData (hrecField @"purpose" ctx) )
                            # pcon 
                                ( Value.PTokenName 
                                    (pconstant 
                                        (BS8.pack "NFTweet #") <> (pintegerToByteString # nftNumber) 
                                    ) 
                                )
                            # pconstant 1
{--
tweetPolicy ::
    Value.CurrencySymbol -> Scripts.ValidatorHash -> Script
tweetPolicy paramThreadTokenSymbol paramValHash =
    compile ( tweetPolicy_logic # pconstant paramThreadTokenSymbol # pconstant paramValHash )
    where
        tweetPolicy_logic ::
            forall s.
            Term
            s
            (
                V1.PCurrencySymbol :--> V1.PValidatorHash 
                :--> Pre.PData
                :--> V1.PScriptContext
                :--> PBool
            )
        tweetPolicy_logic =
            plam $ \ threadTokenSymbol valHash _redeemer pctx ->
                pletFields @'["txInfo","purpose"] pctx $
                    -- only ```txInfoInputs``` and ```txInfoMint``` needed, extraces those only
                    \ ctx -> 
                        pletFields @'["inputs","mint"] ( hrecField @"txInfo" ctx ) $
                        \ txInfos ->
                            plet (hrecField @"inputs" txInfos) $
                            \ txInputs -> 
--}