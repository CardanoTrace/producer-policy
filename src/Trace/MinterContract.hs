
module Trace.MinterContract where

import Plutarch
import           Plutarch.Prelude as Pre

import qualified Plutus.V1.Ledger.Value as Value
import qualified Ledger
import           Plutus.V1.Ledger.Scripts ( Script )
import qualified Plutarch.Api.V1 as V1
import Trace.MinterContract.Datum (MinterContractDatum(MinterContractDatum))
import Trace.MinterContract.Redeemer (MinterContractRedeemer(..))
import Trace.Utils.Plutarch.Onchain.Require (prequireOrThrow)
import Trace.Utils.Plutarch.Onchain.Ctx (ptxSignedByOnly, pfindOwnInput, pvalHashFromAddressOrThrow, pvalHashFromAddress, ppubKeyHashFromAddress)
import Trace.Utils.Plutarch.Onchain.Tx
import Plutarch.List (pfoldl')
import Trace.Utils.Plutarch.Onchain.Lists (pfindOrThrow, pextractSingleElemOrThrow)
import Plutarch.Api.V1 (PCredential(..), PScriptPurpose (..))
import Trace.Utils.Plutarch.Onchain.Value (pvalueOf)
import Plutarch.Api.V1.Value (PCurrencySymbol(..))
import Plutarch.Api.V1.Value (PTokenName(..))
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Trace.Utils.Plutarch.Onchain.Ctx.Datum (pfindDatumOrThrow, PUnsafeSilentFromDatum (..))
import Plutarch.Api.V1.Maybe (PMaybeData(..))
import Plutarch.Api.V1.Scripts (PDatum)

minterContractCont ::
    Value.CurrencySymbol -> Value.TokenName -> Ledger.PubKeyHash -> Script
minterContractCont threadTokenSym' threadTokenName' owner_phk' =
    compile
        ( minterContractCont_logic
            # pconstant threadTokenSym'
            # pconstant threadTokenName'
            # pconstant owner_phk'
        )
    where

        minterContractCont_logic ::
            forall s.
            Term
            s
            (
                V1.PCurrencySymbol :--> V1.PTokenName :--> V1.PPubKeyHash
                :--> PAsData MinterContractDatum
                :--> MinterContractRedeemer
                :--> V1.PScriptContext
                :--> PBool
            )
        minterContractCont_logic =
            plam $ \
                threadTokenSymbol threadTokeName owner_pkh
                minContrDatum
                minContrRedeemer
                pctx -> unTermCont $ do

                ctx <- tcont . pletFields @'["txInfo","purpose"] $ pctx
                txInfos <- tcont . pletFields @'["inputs", "outputs", "signatories", "data"] $ pfromData ( hrecField @"txInfo" ctx )

                let txOuts = pfromData $ hrecField @"outputs" txInfos
                txInsResolved <- tcont . plet $
                    pfoldr
                        # plam (\ listElem transformedList ->
                                pcons
                                    # ( pfield @"resolved" #  pfromData listElem )
                                    # transformedList
                            )
                        # pcon PNil
                        # pfromData (hrecField @"inputs" txInfos)

                ownInputResolved <- tcont . pletFields @'["address","value"] . pfromData $ pfield @"resolved" #$
                    pfindOwnInput # pfromData (hrecField @"purpose" ctx) # pfromData (hrecField @"inputs" txInfos)

                -- common to both redemers, 
                -- makes sure the input has the NFT identifier, otherwise is not trusted
                _ <- tcont . prequireOrThrow "input not trusted" $
                    -- input amount (from self) of the NFT is 1
                    (pvalueOf # pfromData (hrecField @"value" ownInputResolved)
                        # threadTokenSymbol
                        # threadTokeName
                        #== 1
                    )

                -- this is not supposed to fail since we got the "ownInputResolved" using "pfindOwnInput"
                -- so the address is supposed to be of the validator
                ownHash <- tcont . plet $ pvalHashFromAddressOrThrow "" # pfromData ( hrecField @"address" ownInputResolved )

                outputValueToSelf <- tcont . pletFields @'["value","datumHash"] $ pfromData (pextractSingleElemOrThrow "expected single output to self" #$ pfilter
                        # plam (\txOut ->
                            -- get the validator hash of this tx output
                            pmatch (pvalHashFromAddress # pfromData ( pfield @"address" # pfromData txOut ) ) $ \case
                                PJust otherValHash -> otherValHash #== ownHash -- is the same valdiator hash of our one?
                                PNothing -> pcon PFalse -- not an output to a validator
                        )
                        # pfromData (hrecField @"outputs" txInfos)
                    )

                -- common to both redemers, 
                -- makes sure the NFT identifier never goes out
                _ <- tcont . prequireOrThrow "spending NFT identifier!!!" $
                    -- keep NFT
                    (pvalueOf # pfromData (hrecField @"value" outputValueToSelf)
                        # threadTokenSymbol
                        # threadTokeName
                        #== 1)
                    #&&
                    -- keep at least 5 ADA with it
                    (
                        5_000_000 #<= pvalueOf # pfromData (hrecField @"value" outputValueToSelf)
                        # pconstant adaSymbol
                        # pconstant adaToken
                    )

                -- the input datum is argument of the validator !!! 

                -- pfromData (hrecField @"datumHash" outputValueToSelf)
                outOutDatumHash <- tcont . plet . pfromData $ pmatch (pfromData $ hrecField @"datumHash" outputValueToSelf) (\case
                        PDJust datumHash -> pfield @"_0" # datumHash
                        PDNothing _ -> ptraceError "datumHash not present on input"
                    )


                -- needed in both cases
                inputDatum <- tcont $ pletFields @'["nftNumber","price"] minContrDatum

                -- all haskell word
                -- used once but in both cases
                let inputDatumNftNumCounter = pfromData $ hrecField @"nftNumber" inputDatum

                
                -- used once, inlined by haskell
                let rawOutputDatum = pfindDatumOrThrow "no datum provided" 
                        # outOutDatumHash
                        # pfromData ( hrecField @"data" txInfos ) 

                outputDatum <- tcont . pletFields @'["nftNumber","price"] $ 
                            pfromData ((punsafeSilentFromDatum :: Term s PDatum -> Term s ( PAsData MinterContractDatum )) rawOutputDatum)

                pureRedeemer <- tcont . pmatch $ minContrRedeemer
                case pureRedeemer of
                    -- owner only
                    UpdatePrice -> do
                        -- makes sure the owner (and the owner only) is signing the transaction
                        _ <- tcont . prequireOrThrow "only the owner can update the price" $
                            ptxSignedByOnly
                                # pfromData ( hrecField @"signatories" txInfos )
                                # pdata owner_pkh

                        -- all haskell work
                        let outputDatumNftNum       = pfromData $ hrecField @"nftNumber" outputDatum

                        -- makes sure the datum counter is not modified
                        -- price can be anything, if negative (for whatever reason) is assumed 0
                        return . ptraceIfFalse "modifying the counter" $ 
                            outputDatumNftNum #== inputDatumNftNumCounter

                    -- minting
                    IncrementCounter -> do

                        {- 
                            makes sure
                            
                            1. the counter is incremented by one
                            2. the datum "price" field is not modified
                        -}
                        _ <- tcont . prequireOrThrow "modifying the price OR wrong increment" $
                            -- 1. the counter is incremented by one
                            ( pfromData (hrecField @"nftNumber" outputDatum) #== (inputDatumNftNumCounter + 1) )
                            #&&
                            -- 2. the datum "price" field is not modified
                            ( pfromData (hrecField @"price" outputDatum) #== pfromData (hrecField @"price" inputDatum) )

                        -- makes sure a tx output with a minimum value of `nftPrice` lovelaces goes to the owner
                        outputValueToOwner <- tcont .plet . pfromData $ pfield @"value" # pfromData (pextractSingleElemOrThrow "expected single output to self" #$ pfilter
                                # plam (\txOut ->
                                    -- get the validator hash of this tx output
                                    pmatch (ppubKeyHashFromAddress # pfromData ( pfield @"address" # pfromData txOut ) ) $ \case
                                        PJust outputPkh -> outputPkh #== owner_pkh -- is this output going to the owner?
                                        PNothing -> pcon PFalse -- not an output to a validator
                                )
                                # pfromData (hrecField @"outputs" txInfos)
                            )

                        return . ptraceIfFalse "NFT not paid" $ 
                            pfromData (hrecField @"price" inputDatum) #<= pvalueOf # outputValueToOwner
                            # pconstant adaSymbol
                            # pconstant adaToken