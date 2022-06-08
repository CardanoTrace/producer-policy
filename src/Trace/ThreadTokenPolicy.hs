{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Trace.ThreadTokenPolicy where

import qualified PlutusTx
import PlutusTx.Prelude as PTxP
    ( Bool(False), (&&), any, traceIfFalse, Eq((==)), traceError )
import Ledger
    ( scriptCurrencySymbol,
      mkMintingPolicyScript,
      ScriptContext(scriptContextTxInfo),
      TxInInfo(txInInfoOutRef),
      TxInfo(txInfoInputs, txInfoMint),
      TxOutRef,
      CurrencySymbol,
      TokenName )
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value (flattenValue)
import           Prelude                (IO, Semigroup (..), Show (..), String)

-- OFFCHAIN STUFF
-- import           Control.Monad          hiding (fmap)
-- import           Data.Aeson             (FromJSON, ToJSON)
-- import qualified Data.Map               as Map
-- import           Data.Text              (Text)
-- import           Text.Printf            (printf)
-- import           Data.Void              (Void)

-- import           GHC.Generics           (Generic)

-- import           Ledger.Constraints     as Constraints
-- import           Plutus.Contract        as Contract
-- import           Plutus.Trace.Emulator  as Emulator
-- import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not spent"      isUTxOConsumed     PTxP.&&
                          traceIfFalse "only 1 NFT per call" singleTokenMinted
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    isUTxOConsumed :: PTxP.Bool
    isUTxOConsumed = any (\i -> txInInfoOutRef i == oref) (txInfoInputs info)

    singleTokenMinted :: PTxP.Bool
    singleTokenMinted = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> traceError "more than 1 AssetClass minting"

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript (
        $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy ( mkPolicy oref' tn' ) ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode oref
        `PlutusTx.applyCode`
        PlutusTx.liftCode tn
    )

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = Ledger.scriptCurrencySymbol ( policy oref tn )

-- OFFCHAIN STUFF
{-
dta NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
            let val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
-}