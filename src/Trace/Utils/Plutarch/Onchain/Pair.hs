
module Trace.Utils.Plutarch.Onchain.Pair where

import Plutarch.Prelude

isKeyOfPair ::
    (PIsData v, PIsData k, PEq k) =>
    Term s k -> (Term s (PBuiltinPair (PAsData k) (PAsData v)) -> Term s PBool)
isKeyOfPair key builtinPair = key #== pfromData (pfstBuiltin # builtinPair )
