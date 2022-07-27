
module Trace.Utils.Plutarch.Onchain.Maybe where

import Plutarch.Prelude
    (
        Type,
        phoistAcyclic,
        type (:-->),
        S,
        Term,
        plam,
        PMatch(pmatch),
        PBool,
        pconstant,
        PMaybe(..),
        ptraceError,
        PString, perror
    )
import Data.Text (Text)


pfromJust ::
    forall (a :: S -> Type) (s :: S).
    Term s ( PMaybe a :--> a :--> a)
pfromJust = phoistAcyclic $
    plam $ \ maybeStuff deflt -> pmatch maybeStuff $ \case
        PNothing -> deflt
        PJust x -> x

punsafeFromJust ::
    forall (a :: S -> Type) (s :: S).
    Term s (PMaybe a :--> a)
punsafeFromJust = phoistAcyclic $
    plam $ \ maybeStuff -> pmatch maybeStuff $ \case
        PNothing -> perror
        PJust x -> x

pfromJustOrThrow ::
    forall (a :: S -> Type) (s :: S).
    Text ->
    Term s (PMaybe a :--> a)
pfromJustOrThrow errorTrace = phoistAcyclic $
    plam $ \ maybeStuff -> pmatch maybeStuff $ \case
        PNothing -> ptraceError $ pconstant errorTrace
        PJust x -> x


ptraceIfNothing ::
    forall (a :: S -> Type) (s :: S).
    -- | The custom error message.
    Term s PString ->
    Term s (PMaybe a) ->
    Term s a
ptraceIfNothing err maybeStuff = pmatch maybeStuff $ \case
    PNothing -> ptraceError err
    PJust x -> x

pisJust ::
    forall (a :: S -> Type) (s :: S).
    Term s (PMaybe a :--> PBool)
pisJust = phoistAcyclic $
    plam $ \v' ->
        pmatch v' $ \case
            PJust _ -> pconstant True
            _ -> pconstant False