{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Trace.Utils.Plutarch.Onchain.Lists where

import Plutarch.Prelude hiding (PBuiltinList)

import Plutarch.Builtin (PBuiltinList)
import Plutarch.Lift (PUnsafeLiftDecl)
import Data.Text (Text)


pfirstJust ::
    forall (a :: PType) (b :: PType) (s :: S) list.
    PIsListLike list a =>
    Term s ((a :--> PMaybe b) :--> list a :--> PMaybe b)
pfirstJust =
    phoistAcyclic $
        plam $ \predicate ->
            precList
                ( \self x xs ->
                    pmatch (predicate # x) $ \case
                        PNothing -> self # xs -- call recoursively on the rest of the list
                        PJust v -> pcon (PJust v) -- found
                )
                (const $ pcon PNothing) -- not found

pfind ::
    forall (a :: S -> Type) (s :: S) list.
    PIsListLike list a =>
    (Term s a -> Term s PBool) ->
    Term s (list a :--> PMaybe a)
pfind p =
    precList
        (\self x xs -> pif (p x) (pcon (PJust x)) (self # xs))
        (const $ pcon PNothing)

punsafeFind ::
    forall (a :: S -> Type) (s :: S) list.
    PIsListLike list a =>
    (Term s a -> Term s PBool) ->
    Term s (list a :--> a)
punsafeFind predicate =
    precList
        (\self elem xs -> pif (predicate elem) elem (self # xs))
        (const perror)

pfindOrThrow ::
    forall (a :: S -> Type) (s :: S) list.
    PIsListLike list a =>
    Text ->
    (Term s a -> Term s PBool) ->
    Term s (list a :--> a)
pfindOrThrow errorTrace predicate =
    precList
        (\self elem rest -> pif (predicate elem) elem (self # rest))
        (const . ptraceError $ pconstant errorTrace )


pisEmpty ::
    forall (a :: S -> Type) (s :: S) list.
    PIsListLike list a =>
    Term s (list a :--> PBool)
pisEmpty = pnull

pisSingleElemList ::
    PUnsafeLiftDecl a =>
    Term s (PBuiltinList a :--> PBool)
pisSingleElemList =
    plam $ \ list -> pmatch list $ \case
        PNil -> pcon PFalse
        PCons _elem rest -> pisEmpty # rest

{-  | ideally the same of
    |
    | > pif (pisSingleElemList # list) (phead # list) (perror)
    |
    | but more efficient
-}
punsafeExtractSingleElem ::
    PUnsafeLiftDecl a =>
    Term s (PBuiltinList a :--> a)
punsafeExtractSingleElem = phoistAcyclic $
    plam $ \ list -> pmatch list $ \case
        PCons elem rest -> pif (pnull # rest) elem perror
        PNil -> perror

pextractSingleElemOrThrow ::
    PUnsafeLiftDecl a =>
    Text ->
    Term s (PBuiltinList a :--> a)
pextractSingleElemOrThrow errorTrace = phoistAcyclic $
    plam $ \ list -> pmatch list $ \case
        PCons elem rest -> 
            pif (pnull # rest) -- if tail is null then is single element list, success 
                elem -- success
                (ptraceError $ pconstant errorTrace) -- more than one elment
        PNil -> ptraceError $ pconstant errorTrace -- if empty list throws since is not a single list


areOfSameLength :: [a] -> [b] -> Bool
areOfSameLength [] [] = True
areOfSameLength ( _aElem : _aRest ) [] = False
areOfSameLength [] ( _bElem : _bRest ) = False
areOfSameLength ( _aElem : aRest ) ( _bElem : bRest ) = areOfSameLength aRest bRest

pareOfSameLength ::
    (PLift a, PLift b) =>
    Term s ( PBuiltinList a :--> PBuiltinList b :--> PBool )
pareOfSameLength = pfix #$ plam $ \self listA listB ->
    pmatch listA (\case
            PCons _elemA restA ->
                pforce $ pmatch listB (\case
                    PCons _elemB restB -> pdelay $ self # restA # restB
                    PNil -> pdelay $ pcon PFalse
                )
            PNil ->
                pmatch listB (\case
                    PCons _elemB restB -> pcon PFalse
                    PNil -> pcon PTrue
                )
        )

pappendBuiltinList :: PLift a => Bool -> Term s (PBuiltinList a :--> PBuiltinList a :--> PBuiltinList a )
pappendBuiltinList assumeFirstShorter =
    if assumeFirstShorter
    then plam $ \listA listB -> pappendBuiltinListFirstShorter # listA # listB
    else plam $ \listA listB -> pappendBuiltinListFirstShorter # listB # listA

pappendBuiltinListFirstShorter :: PLift a => Term s (PBuiltinList a :--> PBuiltinList a :--> PBuiltinList a )
pappendBuiltinListFirstShorter = plam (\ listA listB ->
        pfoldr # (plam $ \elemA accumListB -> pcons # elemA # accumListB ) # listB # listA
    )