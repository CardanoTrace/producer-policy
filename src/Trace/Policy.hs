
module Trace.Policy where

import Trace.MinterContract (
        minterContractHash
    )
import Ledger.Typed.Scripts.MonetaryPolicies (
        mkForwardingMintingPolicy
    )
import Plutus.V1.Ledger.Scripts (
        MintingPolicyHash, MintingPolicy
    )
import Ledger (
        mintingPolicyHash
    )


{-
    makes a minting policy which only succeds if the given validator is running in the same transaction
    therefore the whole transaction passes if the validator accepts it
-}
traceProducerPolicy :: MintingPolicy
traceProducerPolicy = mkForwardingMintingPolicy minterContractHash

traceProducerPolicyHash :: MintingPolicyHash
traceProducerPolicyHash = mintingPolicyHash traceProducerPolicy