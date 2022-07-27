
module Trace.Utils.Plutarch.Onchain.Value where


import qualified Trace.Utils.Plutarch.Onchain as Map

import Plutarch.Prelude
import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName, PValue (..))
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Api.V1.AssocMap (PMap(PMap))
import Trace.Utils.Plutarch.Onchain.Map (punsafeLookup)
import Plutarch.Api.V1 (PScriptPurpose (PMinting), PScriptContext (PScriptContext))
import qualified Trace.Utils.Plutarch.Onchain.Map as Map


pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

psingletonValue ::
    Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue )
psingletonValue = phoistAcyclic $
    plam $ \cs tn i -> unTermCont $ do
        innerPair <- pletC (ppairDataBuiltin # pdata tn # pdata i)
        inner <- pletC (pcon . PMap $ psingleton # innerPair)
        outerPair <- pletC (ppairDataBuiltin # pdata cs # pdata inner)
        outer <- pletC (pcon . PMap $ psingleton # outerPair)
        pure . pcon . PValue $ outer

pownCurrencySymbol :: Term s ( PScriptPurpose :--> PCurrencySymbol )
pownCurrencySymbol = phoistAcyclic $ plam $
    \ purpose -> pmatch purpose
        ( \case
            PMinting ownCurrSym -> pfromData $ pfield @"_0" # ownCurrSym
            _ -> perror
        )

{- | prefer ```pownCurrencySymbol``` if you need to use also the txInfo field

-}
pownCurrencySymbolFromCtx :: Term s ( PScriptContext :--> PCurrencySymbol )
pownCurrencySymbolFromCtx = phoistAcyclic $ plam $
    \ ctx -> pownCurrencySymbol # pfromData ( pfield @"purpose" # ctx )


pvalueOf ::
    Term s (PValue :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $ plam $
    \ value currSym tName -> -- pconstant 1
        pmatch value
        $ \case (PValue map) -> punsafeLookup # tName # (punsafeLookup # currSym # map)

pvalueToMap :: Term s PValue -> Term s (PMap PCurrencySymbol (PMap PTokenName PInteger))
pvalueToMap value = pmatch value (\case PValue inner -> inner)


instance PEq PValue where
    termValA #== termValB = pvalueToMap termValA #== pvalueToMap termValB

{- | Combine two 'PValue's applying the given function to any pair of
 quantities with the same asset class. Note that the result is _not_
 'normalize'd and may contain zero quantities.
-}
punionWith ::
  Term
    s
    ( (PInteger :--> PInteger :--> PInteger) :--> PValue :--> PValue
        :--> PValue
    )
punionWith = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      Map.punionWith
        # plam (\x y -> Map.punionWith # combine # x # y)
        # pto x
        # pto y