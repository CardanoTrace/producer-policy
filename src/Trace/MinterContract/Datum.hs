{-
{-# LANGUAGE UndecidableInstances #-}
needed to allow

    deriving
        (PlutusType, PIsData, PDataFields)
        via (PIsDataReprInstances (Triplet a))
--}
{-# LANGUAGE UndecidableInstances #-}
--}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Trace.MinterContract.Datum where

import Plutarch.Prelude
import Plutarch.DataRepr
import qualified GHC.Generics as GHC
import Generics.SOP as GSOP (Generic, I (I))

import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Api.V1.Scripts (PDatum)
import Trace.Utils.Plutarch.Onchain.Ctx.Datum (PUnsafeSilentFromDatum (punsafeSilentFromDatum))

{- |
  We can defined a data-type using PDataRecord, with labeled fields.
  With an appropriate instance of 'PIsDataRepr', we can automatically
  derive 'PDataFields'.
-}
newtype MinterContractDatum (s :: S)
    = MinterContractDatum
        ( Term
            s
            ( PDataRecord
                '[ "nftNumber" ':= PInteger
                , "price" ':= PInteger
                ]
            )
        )
    deriving stock (GHC.Generic)
    deriving anyclass (GSOP.Generic)
    deriving anyclass (PIsDataRepr)
    deriving
        (PIsData, PDataFields)
        via (PIsDataReprInstances MinterContractDatum)
    deriving anyclass (PUnsafeSilentFromDatum)


deriving via (DerivePNewtype MinterContractDatum ( PDataRecord
                '[ "nftNumber" ':= PInteger
                , "price" ':= PInteger
                ]
            )) instance PlutusType MinterContractDatum

h_unsafeSilentDatumToMinterContract :: Term s PDatum -> Term s ( PAsData MinterContractDatum )
h_unsafeSilentDatumToMinterContract = punsafeSilentFromDatum

punsafeSilentDatumToMinterContract :: Term s (PDatum :--> PAsData MinterContractDatum )
punsafeSilentDatumToMinterContract = plam h_unsafeSilentDatumToMinterContract