{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Trace.Utils where

import Cardano.Api as API
    (
        PlutusScriptV1, PlutusScript,
        writeFileTextEnvelope,
        FileError,
        ScriptData(..),
        SlotNo (SlotNo), AddressAny (AddressByron, AddressShelley), SerialiseAddress (deserialiseAddress), AsType (AsAddressAny, AsAssetName), scriptDataToJson, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), serialiseToRawBytesHex, SerialiseAsRawBytes (deserialiseFromRawBytes)
    )
import           Cardano.Api.Shelley                        ( PlutusScript (..) )
import qualified Ledger.Typed.Scripts           as Scripts  ( MintingPolicy )
import qualified Ledger

import           Codec.Serialise                            ( serialise )
import qualified Data.ByteString.Short          as SBS
import qualified Data.ByteString.Lazy           as LBS


import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import qualified Cardano.Ledger.Credential   as Credentials
        (
            Credential(..),
            StakeReference(..),
            Ptr (Ptr)
        )
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Codec.Serialise             (serialise)
import           Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.String                 (IsString (..))
import           Data.Text                   (pack)
import           Plutus.PAB.Webserver.Types  (ContractActivationArgs (..))
import Plutus.V1.Ledger.Credential as PlutusCredentials
    ( StakingCredential(..), Credential(..) )
import           Plutus.V1.Ledger.Value      (TokenName (..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import qualified Ledger
import           Wallet.Emulator.Wallet      (WalletId (..), Wallet (..))
import           Wallet.Types                (ContractInstanceId (..))
import qualified Plutus.V1.Ledger.Value    as Value

import qualified Data.ByteString.Char8 as BS

dataToScriptData :: Data -> API.ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

credentialLedgerToPlutus :: Credentials.Credential a StandardCrypto -> PlutusCredentials.Credential
credentialLedgerToPlutus (Credentials.ScriptHashObj (ScriptHash h)) = ScriptCredential $ Ledger.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (Credentials.KeyHashObj (KeyHash h))       = PubKeyCredential $ Ledger.PubKeyHash $ toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Credentials.StakeReference StandardCrypto -> Maybe StakingCredential
stakeReferenceLedgerToPlutus (Credentials.StakeRefBase x)                   = Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (Credentials.StakeRefPtr (Credentials.Ptr (API.SlotNo x) y z)) = Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus Credentials.StakeRefNull                       = Nothing

tryReadAddress :: String -> Maybe Ledger.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ pack x of
        Nothing                                      -> Nothing
        Just (AddressByron _)                        -> Nothing
        Just (AddressShelley (ShelleyAddress _ p s)) -> Just Ledger.Address
                { Ledger.addressCredential        = credentialLedgerToPlutus p
                , Ledger.addressStakingCredential = stakeReferenceLedgerToPlutus s
                }

tryReadWalletId :: String -> Maybe WalletId
tryReadWalletId = decode . encode


unsafeReadWalletId :: String -> WalletId
unsafeReadWalletId s = fromMaybe (error $ "can't parse " ++ s ++ " as a WalletId") $ tryReadWalletId s

unsafeReadAddress :: String -> Ledger.Address
unsafeReadAddress s = fromMaybe (error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

unsafeReadTxOutRef :: String -> Ledger.TxOutRef
unsafeReadTxOutRef s =
    let
        (x, _ : y) = span (/= '#') s
    in
        Ledger.TxOutRef
                { Ledger.txOutRefId  = fromString x
                , Ledger.txOutRefIdx = read y
                }

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

contractActivationArgs :: WalletId -> a -> ContractActivationArgs a
contractActivationArgs wid a = ContractActivationArgs
        { caID = a
        , caWallet = Just $ Wallet {getWalletId = wid}
        }

getCredentials :: Ledger.Address -> Maybe (Ledger.PaymentPubKeyHash, Maybe Ledger.StakePubKeyHash)
getCredentials (Ledger.Address x y) = case x of
        ScriptCredential _   -> Nothing
        PubKeyCredential pkh ->
            let
                ppkh = Ledger.PaymentPubKeyHash pkh
            in
                case y of
                        Nothing                        -> Just (ppkh, Nothing)
                        Just (StakingPtr _ _ _) -> Nothing
                        Just (StakingHash h)           -> case h of
                                ScriptCredential _    -> Nothing
                                PubKeyCredential pkh' -> Just (ppkh, Just $ Ledger.StakePubKeyHash pkh')

unsafePaymentPubKeyHash :: Ledger.Address -> Ledger.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (error $ "script address " ++ show addr ++ " does not contain a payment key") fst $ getCredentials addr

unsafeStakePubKeyHash :: Ledger.Address -> Ledger.StakePubKeyHash
unsafeStakePubKeyHash addr = case getCredentials addr of
        Nothing           -> error $ "unexpected script address " ++ show addr
        Just (_, Nothing) -> error $ "addres " ++ show addr ++ " contains no stake component"
        Just (_, Just x)  -> x

cidToString :: ContractInstanceId -> String
cidToString = show . unContractInstanceId


unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex = BS8.unpack . serialiseToRawBytesHex . fromJust . deserialiseFromRawBytes AsAssetName . getByteString . unTokenName
    where
        getByteString (BuiltinByteString bs) = bs

writeMintingPolicy :: FilePath -> Scripts.MintingPolicy -> IO (Either (FileError ()) ())
--                                                PlutusScript PlutusScriptV1 instance of HasTextEnvelope  
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file -- filepath 
    Nothing -- Maybe TextEnvelopeDescr -> default
    . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.getMintingPolicy

strToTokenName :: String -> Value.TokenName
{--
```BS``` is Char8, therfore ASCII encoding, which is a valid ```[Char]``` aka ```String```
--}
strToTokenName = Value.tokenName . BS.pack