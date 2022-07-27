
module Trace.Utils.Plutarch.Onchain.Ctx where

import Plutarch.Prelude
    ( 
        phoistAcyclic,
        plet,
        (#),
        (#$),
        type (:-->),
        Term,
        plam,
        PCon(pcon),
        PMatch(pmatch),
        PIsData(pdata, pfromData),
        PAsData,
        PBuiltinList,
        PEq((#==)),
        PBool,
        pif,
        pfield,
        PMaybe(..) 
    )

import Plutarch.Api.V1.Tx (PTxOutRef, PTxInInfo(..))

import Plutarch.Prelude

import Trace.Utils.Plutarch.Onchain.Lists (pfirstJust, pisSingleElemList, pfindOrThrow)
import Trace.Utils.Plutarch.Onchain.Maybe (pisJust)
import Plutarch.Api.V1
import Plutarch.List (pelem)
import Plutarch.Bool ((#&&))
import Plutarch.TermCont (unTermCont)

import Trace.Utils.Plutarch.Onchain.Tx ()
import Data.Text (Text)

{-
checks the given UTxO to be present in the given TxInInfo (```PBuiltinList (PAsData PTxInInfo)```)
-}
pisUTXOSpent :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
pisUTXOSpent = phoistAcyclic $
    plam $ \oref inputs -> pisJust #$ pfindTxInByTxOutRef # oref # inputs


pfindTxInByTxOutRef :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PMaybe PTxInInfo)
pfindTxInByTxOutRef = phoistAcyclic $
    plam $ \txOutRef inputs ->
        pfirstJust
            # plam -- :: Term s (PAsData PTxInInfo :--> PMaybe PTxInInfo)
                ( \txInInfo' ->
                    plet (pfromData txInInfo') $ \r ->
                        pmatch r $ \(PTxInInfo txInInfo) ->
                            pif
                                (pdata txOutRef #== pfield @"outRef" # txInInfo)
                                (pcon (PJust r))
                                (pcon PNothing)
                )
            #$ inputs

ptxSignedBy :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
    plam $ \sigs sig -> pelem # sig # sigs

ptxSignedByOnly :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PAsData PPubKeyHash :--> PBool)
ptxSignedByOnly = phoistAcyclic $
    plam $ \sigs sig -> (pisSingleElemList # sigs) #&& (pelem # sig # sigs)


pfindOwnInput :: Term s (PScriptPurpose :--> PBuiltinList (PAsData PTxInInfo) :--> PTxInInfo)
pfindOwnInput = phoistAcyclic $ 
    plam (\ purpose txInInfos -> pfromData (pfindOrThrow "unable to find own input"
            (\txIn -> unTermCont $ do

                PSpending fieldTxOutRef <- tcont . pmatch $ purpose
                
                return $ pfromData (pfield @"outRef" # pfromData txIn) #== pfromData ( pfield @"_0" # fieldTxOutRef )
            )
            # txInInfos
        )
    )

punsafeValHashFromAddress :: Term s ( PAddress :--> PValidatorHash )
punsafeValHashFromAddress = phoistAcyclic $ plam $ \ addr -> 
    pmatch (pfromData $ pfield @"credential" # addr ) (\case
        PScriptCredential dataRecordValHash -> pfromData $ pfield @"_0" # dataRecordValHash
        PPubKeyCredential _ -> perror
    )

pvalHashFromAddressOrThrow :: Text -> Term s ( PAddress :--> PValidatorHash )
pvalHashFromAddressOrThrow errorTrace = phoistAcyclic $ plam $ \ addr -> 
    pmatch (pfromData $ pfield @"credential" # addr ) (\case
        PScriptCredential dataRecordValHash -> pfromData $ pfield @"_0" # dataRecordValHash
        PPubKeyCredential _ -> ptraceError $ pconstant errorTrace
    )

pvalHashFromAddress :: Term s ( PAddress :--> PMaybe PValidatorHash )
pvalHashFromAddress = phoistAcyclic $ plam $ \ addr -> 
    pmatch (pfromData $ pfield @"credential" # addr ) (\case
        PScriptCredential dataRecordValHash -> pcon . PJust . pfromData $ pfield @"_0" # dataRecordValHash
        PPubKeyCredential _ -> pcon PNothing
    )

ppubKeyHashFromAddress :: Term s ( PAddress :--> PMaybe PPubKeyHash )
ppubKeyHashFromAddress = phoistAcyclic $ plam $ \ addr -> 
    pmatch (pfromData $ pfield @"credential" # addr ) (\case
        PPubKeyCredential dataRecordPkh -> pcon . PJust . pfromData $ pfield @"_0" # dataRecordPkh
        PScriptCredential _ -> pcon PNothing
    )


