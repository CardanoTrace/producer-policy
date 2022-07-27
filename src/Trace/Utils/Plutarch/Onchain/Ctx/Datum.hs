
module Trace.Utils.Plutarch.Onchain.Ctx.Datum where

import Plutarch.Prelude
import Plutarch.Api.V1 (
        PTuple
        , PDatumHash
        , PDatum (..)
    )
import Data.Text (Text)
import Trace.Utils.Plutarch.Onchain.Lists (pfindOrThrow)
import Plutarch.Unsafe (punsafeCoerce)


hDatumToData :: Term s PDatum -> Term s PData
hDatumToData datum = pmatch datum (\case PDatum innerData -> innerData )

pDatumToData :: Term s ( PDatum :--> PData )
pDatumToData = plam hDatumToData

class PUnsafeSilentFromDatum (a :: PType) where

    punsafeSilentFromDatum :: 
        forall (s :: S). 
        Term s PDatum -> Term s (PAsData a)
    punsafeSilentFromDatum datum = punsafeCoerce ( hDatumToData datum )


{- |
    ```PBuiltinList (PAsData (PTuple PDatumHash PDatum))``` si the type of the "data" field in a PTxInfo
-}
type PTxInfoData = PBuiltinList (PAsData (PTuple PDatumHash PDatum))

pfindDatumOrThrow :: Text -> Term s (PDatumHash :--> PTxInfoData :--> PDatum)
pfindDatumOrThrow errorTrace = phoistAcyclic $
    plam $ \datumHash datums ->
        pfromData $ pfield @"_1" 
            # pfromData (
                pfindOrThrow errorTrace 
                    (\ elem -> pfromData (pfield @"_0" # elem) #== datumHash ) 
                    # datums
            )


