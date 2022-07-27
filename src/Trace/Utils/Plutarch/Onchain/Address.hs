
module Trace.Utils.Plutarch.Onchain.Address where

import Plutarch.Prelude
import Plutarch.Api.V1

pisAddressOfValidator :: Term s ( PAddress :--> PValidatorHash :--> PBool )
pisAddressOfValidator =
    plam $ \ addr valHash -> pmatch (pfromData $ pfield @"credential" # addr) $ \case
        PScriptCredential credentialValHash -> pfromData (pfield @"_0" # credentialValHash) #== valHash
        _ -> pcon PFalse
