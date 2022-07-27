
module Trace.Utils.Plutarch.Onchain.Require where

import Plutarch.Prelude
import Plutarch.Internal ( Term(Term), TermResult(..) )
import Data.Text (Text)
import Plutarch.Api.V1.Maybe (PMaybeData(..))

type ContFunc (r :: PType) (s :: S) (partialRes :: PType) = 
    (Term s partialRes -> Term s r) -> Term s r

{- | 
    IMPORTANT this dealys the continuation
-}
prequire :: Term s PBool -> ContFunc b s PUnit
prequire condition cont = pif condition ( plet (pcon PUnit) cont ) perror

{- | 
    IMPORTANT this dealys the continuation
-}
prequireOrThrow :: Text -> Term s PBool -> ContFunc b s PUnit
prequireOrThrow errorTrace condition cont = pif condition ( plet (pcon PUnit) cont ) (ptraceError $ pconstant errorTrace)

{- | 
    IMPORTANT this dealys the continuation
-}
prequireCont :: Term s PBool -> TermCont s (Term s PUnit)
prequireCont = tcont . prequire

{- | 
    IMPORTANT this dealys the continuation
-}
prequireContOrThrow :: Text -> Term s PBool -> TermCont s (Term s PUnit)
prequireContOrThrow errorTrace condition = tcont $ prequireOrThrow errorTrace condition

{- | 
    IMPORTANT this dealys the continuation
-}
prequireExsitsts :: Term s (PMaybe a) -> ContFunc b s a
prequireExsitsts maybeStuff cont = pmatch maybeStuff $ \case
   PJust stuff -> cont stuff
   PNothing -> perror

{- | 
    IMPORTANT this dealys the continuation
-}
prequireExsitstsOrThrow :: Text -> Term s (PMaybe a) -> ContFunc b s a
prequireExsitstsOrThrow errorTrace maybeStuff cont = pmatch maybeStuff $ \case
   PJust stuff -> cont stuff
   PNothing -> ptraceError $ pconstant errorTrace

{- | 
    IMPORTANT this dealys the continuation
-}
prequireExsitstsCont :: Term s (PMaybe a) -> TermCont s (Term s a)
prequireExsitstsCont = tcont . prequireExsitsts

{- | 
    IMPORTANT this dealys the continuation
-}
prequireExsitstsContOrThrow :: Text -> Term s (PMaybe a) -> TermCont s (Term s a)
prequireExsitstsContOrThrow errorTrace cont = tcont $ prequireExsitstsOrThrow errorTrace cont


{- | 
    IMPORTANT this dealys the continuation
-}
prequireExsitstsData :: PIsData a => Term s (PMaybeData a) -> ContFunc b s a
prequireExsitstsData maybeStuff cont = pmatch maybeStuff $ \case
   PDJust stuff -> cont . pfromData $ pfield @"_0" # stuff
   PDNothing _ -> perror

{- | 
    IMPORTANT this dealys the continuation
-}
prequireExsitstsDataOrThrow :: PIsData a => Text -> Term s (PMaybeData a) -> ContFunc b s a
prequireExsitstsDataOrThrow errorTrace maybeStuff cont = pmatch maybeStuff $ \case
   PDJust stuff -> cont . pfromData $ pfield @"_0" # stuff
   PDNothing _ -> ptraceError $ pconstant errorTrace

{- | 
    IMPORTANT this dealys the continuation
-}
prequireExsitstsDataCont :: PIsData a => Term s (PMaybeData a) -> TermCont s (Term s a)
prequireExsitstsDataCont = tcont . prequireExsitstsData

{- | 
    IMPORTANT this dealys the continuation
-}
prequireExsitstsDataContOrThrow :: PIsData a => Text -> Term s (PMaybeData a) -> TermCont s (Term s a)
prequireExsitstsDataContOrThrow errorTrace cont = tcont $ prequireExsitstsDataOrThrow errorTrace cont