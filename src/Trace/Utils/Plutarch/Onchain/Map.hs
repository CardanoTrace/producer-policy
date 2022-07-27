{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Trace.Utils.Plutarch.Onchain.Map where

import Plutarch.Internal

import Plutarch.Prelude
import Plutarch.Api.V1.AssocMap ( PMap(..) )
import GHC.Base (Any)
import Trace.Utils.Plutarch.Onchain.Lists (pfirstJust, punsafeFind, pfindOrThrow, pfind, pareOfSameLength)
import Plutarch.Builtin (PBuiltinMap, ppairDataBuiltin)
import Trace.Utils.Plutarch.Onchain.Pair ( isKeyOfPair )
import Data.Text (Text)
import Plutarch.List (pfoldl')

--{--        
instance (
        PIsData k, PEq k,
        PIsData v, PEq v
    ) =>(PEq (PBuiltinPair (PAsData k) (PAsData v))) where
    bPairA #== bPairB =
        (pfromData (pfstBuiltin # bPairA) #== pfromData (pfstBuiltin # bPairB)) #&&
        (pfromData (psndBuiltin # bPairA) #== pfromData (psndBuiltin # bPairB))

instance (
        PIsData k, PEq k,
        PIsData v, PEq v
    ) => PEq (PMap k v) where
    termMapA #== termMapB = pmapToBuiltin termMapA #== pmapToBuiltin termMapB
--}

instance
    (
        PEq k, PLift k ,
        PIsData k,
        PIsData v,
        Semigroup (Term s v)
    ) => Semigroup (Term s (PMap k v)) where
  mapA <> mapB = punionWith # plam (<>) # mapA # mapB

{- | Get the keys of a given map, the order of the keys is preserved.
      @since 1.1.0
-}
pkeys ::
    forall (k :: S -> Type) (v :: S -> Type) (s :: S).
    Term s (PMap k v :--> PBuiltinList (PAsData k))
pkeys = phoistAcyclic $
    plam $ \m -> unTermCont $ do
        PMap kvs <- tcont . pmatch $ m
        pure $ pmap # pfstBuiltin # kvs


pmapToBuiltin :: Term s (PMap k v) -> Term s (PBuiltinMap k v)
pmapToBuiltin {-pmap-} = pto {- pmatch pmap
    (\case PMap builtin -> builtin) --}


plookupBuiltin ::
    (PIsData v, PIsData k, PEq k) =>
    Term s (k :--> PBuiltinMap k v :--> PMaybe v)
plookupBuiltin = phoistAcyclic $
    plam $ \ key pmap ->
        -- ```BuiltinMap k v``` is a valid instance for the ```PIsListLike``` typeclass
        pmatch (pfind (isKeyOfPair key) # pmap) (\case
            PJust bPair -> pcon . PJust . pfromData $ psndBuiltin # bPair
            PNothing -> pcon PNothing
        )

plookup ::
    (PIsData v, PIsData k, PEq k) =>
    Term s (k :--> PMap k v :--> PMaybe v)
plookup = phoistAcyclic $
    plam $ \ key pmap ->
        -- ```BuiltinMap k v``` is a valid instance for the ```PIsListLike``` typeclass
        pmatch (pfind (isKeyOfPair key) # pmapToBuiltin pmap) (\case
            PJust bPair -> pcon . PJust . pfromData $ psndBuiltin # bPair
            PNothing -> pcon PNothing
        )

plookupPairBuiltin ::
    (PIsData v, PIsData k, PEq k) =>
    Term s (k :--> PBuiltinMap k v :--> PMaybe (PBuiltinPair (PAsData k) (PAsData v)))
plookupPairBuiltin = phoistAcyclic $
    plam $ \ key pmap ->
        -- ```BuiltinMap k v``` is a valid instance for the ```PIsListLike``` typeclass
        pmatch (pfind (isKeyOfPair key) # pmap) (\case
            PJust bPair -> pcon . PJust $ bPair
            PNothing -> pcon PNothing
        )

punsafeLookupBultin ::
    (PIsData v, PIsData k, PEq k) =>
    Term s (k :--> PBuiltinMap k v :--> v)
punsafeLookupBultin = phoistAcyclic $
    plam $ \ key pmap ->
        pfromData $ psndBuiltin # (
            punsafeFind (isKeyOfPair key) #
            -- ```BuiltinMap k v``` is a valid instance for the ```PIsListLike``` typeclass
            pmap
        )

punsafeLookup ::
    (PIsData v, PIsData k, PEq k) =>
    Term s (k :--> PMap k v :--> v)
punsafeLookup = phoistAcyclic $
    plam $ \ key pmap ->
        pfromData $ psndBuiltin # (
            punsafeFind (isKeyOfPair key) #
                -- ```BuiltinMap k v``` is a valid instance for the ```PIsListLike``` typeclass
                pmapToBuiltin pmap
        )


pbuiltinLookupOrThrow ::
    (PIsData v, PIsData k, PEq k) =>
    Text ->
    Term s (k :--> PBuiltinMap k v :--> v)
pbuiltinLookupOrThrow errorTrace = phoistAcyclic $
    plam $ \ key pmap ->
        pfromData $ psndBuiltin # (
            pfindOrThrow errorTrace (isKeyOfPair key) #
            -- ```BuiltinMap k v``` is a valid instance for the ```PIsListLike``` typeclass
            pmap
        )

plookupOrThrow ::
    (PIsData v, PIsData k, PEq k) =>
    Text -> Term s (k :--> PMap k v :--> v)
plookupOrThrow errorTrace = phoistAcyclic $
    plam $ \ key pmap ->
        pfromData $ psndBuiltin # (
            pfindOrThrow errorTrace (isKeyOfPair key) #
            -- ```BuiltinMap k v``` is a valid instance for the ```PIsListLike``` typeclass
            pmapToBuiltin pmap
        )


punionWith :: 
    (
        PEq k, PLift k ,
        PIsData k,
        PIsData v
    ) =>
    Term s (
        ( v :-->  v :-->  v) 
        :--> PMap k v 
        :--> PMap k v 
        :--> PMap k v
    )
punionWith = plam $ \combineValues mapA mapB -> pcon . PMap $ pbuiltinUnionWith # combineValues # (pmapToBuiltin mapA) # (pmapToBuiltin mapB)

{- |
    extremely inefficient; O(n + m^2), pass the shorte list as second
-}
pbuiltinUnionWith ::
    (
        PEq k, PLift k ,
        PIsData k,
        PIsData v
    ) =>
    Term s (
        ( v :-->  v :-->  v)
        :--> PBuiltinMap k v
        :--> PBuiltinMap k v
        :--> PBuiltinMap k v
    )
pbuiltinUnionWith = phoistAcyclic $ plam $ \(combineValues :: Term s ( v :--> v :--> v )) mapA mapB -> 
    ( paddMissingPairsFrom # mapB # 
        (pbuiltinUnionMatchingWith # combineValues # mapA # mapB) 
    )

pbuiltinUnionMatchingWith :: 
    (
        PEq k, PLift k ,
        PIsData k,
        PIsData v
    ) =>
    Term s (
        ( v :-->  v :-->  v)
        :--> PBuiltinMap k v
        :--> PBuiltinMap k v
        :--> PBuiltinMap k v
    )
pbuiltinUnionMatchingWith = phoistAcyclic $ plam $ \combineValues mapA mapB ->
    (pfoldr
        # plam
            (\aPair accum -> unTermCont $ do

                keyAsData <- tcont . plet $ pfstBuiltin # aPair

                -- search for the same key in map B
                maybeBVal <- tcont . pmatch $ plookupBuiltin # pfromData keyAsData # mapB
                case maybeBVal of
                    -- if key is present
                    PJust bValue -> 
                        return . pcon $ 
                            PCons -- add at the beginning ( of the accumulated list )
                                ( ppairDataBuiltin -- a newly constructed pair (necessary to update with a new value)
                                    # keyAsData -- with the key used to find the common vaue
                                    # pdata (
                                        combineValues -- and a new value obtained using the passed funciton
                                            # pfromData (psndBuiltin # aPair)
                                            # bValue
                                    )
                                )
                                accum
                    PNothing -> return . pcon $ PCons aPair accum
            )
        # pcon PNil -- start from empty PBuiltinList
        # mapA
    )

paddMissingPairsFrom ::
    (
        PEq k, PLift k ,
        PIsData k,
        PIsData v
    ) =>
    Term s (
        PBuiltinMap k v
        :--> PBuiltinMap k v
        :--> PBuiltinMap k v
    )
paddMissingPairsFrom = phoistAcyclic $ plam $ \ mapA mapB ->
    pfoldr
        # plam 
            (\ aPair accum -> unTermCont $ do
                
                maybeBValue <- tcont . pmatch $ plookupPairBuiltin # pfromData ( pfstBuiltin # aPair ) # mapB -- searches for key in mapB
                case maybeBValue of
                    PJust bValue -> -- if found
                        return accum -- everything fine
                    PNothing -> -- if missing 
                        return . pcon $ PCons aPair accum -- add this pair to the accumulated value
            )
        # mapB -- keep the list
        # mapA