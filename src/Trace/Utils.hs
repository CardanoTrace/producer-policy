{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}


module Trace.Utils where

import Cardano.Api as API
    (
        PlutusScriptV1, PlutusScript,
        writeFileTextEnvelope,
        FileError,
        ScriptData(..),
        SlotNo (SlotNo), AddressAny (AddressByron, AddressShelley), SerialiseAddress (deserialiseAddress), AsType (AsAddressAny, AsAssetName), scriptDataToJson, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), serialiseToRawBytesHex, SerialiseAsRawBytes (deserialiseFromRawBytes)
    )
import Cardano.Api.Shelley
    ( PlutusScript(..), Address(..), PlutusScript(..) )
import qualified Ledger.Typed.Scripts           as Scripts  ( MintingPolicy, Validator, ValidatorTypes, TypedValidator, validatorScript, validatorHash )
import qualified Ledger

import           Codec.Serialise                            ( serialise )
import qualified Data.ByteString.Short          as SBS
import qualified Data.ByteString.Lazy           as LBS
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
import           Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS8
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
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..), appendByteString)
import           Wallet.Emulator.Wallet      (WalletId (..), Wallet (..))
import           Wallet.Types                (ContractInstanceId (..))
import qualified Plutus.V1.Ledger.Value    as Value

import qualified Data.ByteString as BS
import PlutusTx.Builtins.Class (stringToBuiltinByteString, )
import qualified PlutusTx.Prelude as PTxP
import qualified PlutusTx.Builtins as Builtins
import qualified Numeric
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Ledger.Scripts as UScripts
import qualified Plutus.V1.Ledger.Api as Ledger
import Ledger (Script(unScript))
import Plutarch (printScript)
import qualified Plutarch.Internal.Other as PPretty

------------------------------ getting .plutus files ------------------------------

getAndWriteScript :: ( willBeScript -> Ledger.Script ) -> FilePath -> willBeScript -> IO (Either (FileError ()) ())
getAndWriteScript getScript filePath =
--                          PlutusScript PlutusScriptV1 instance of HasTextEnvelope  
    writeFileTextEnvelope @(PlutusScript PlutusScriptV1) filePath
    Nothing -- Maybe TextEnvelopeDescr ( Nothing => default )
    . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . getScript

writeMintingPolicy :: FilePath -> Scripts.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy = getAndWriteScript Ledger.getMintingPolicy

writeValidator :: FilePath -> Scripts.Validator -> IO (Either (FileError ()) ())
writeValidator = getAndWriteScript Ledger.unValidatorScript

writeTypedValidator :: Scripts.ValidatorTypes validator => FilePath -> Scripts.TypedValidator validator -> IO (Either (FileError ()) ())
writeTypedValidator = getAndWriteScript (Ledger.unValidatorScript . Scripts.validatorScript)


makeAssetClass :: BS8.ByteString -> BS8.ByteString -> Value.AssetClass
makeAssetClass currencySym tName = Value.AssetClass ( Value.currencySymbol currencySym , Value.tokenName tName )

makeAssetClass' :: Value.CurrencySymbol -> BS8.ByteString -> Value.AssetClass
makeAssetClass' currencySym tName = Value.AssetClass ( currencySym , Value.tokenName tName )

makeTokenName :: BS.ByteString -> TokenName
makeTokenName = Value.tokenName

makeCurrencySymbol :: BS.ByteString -> Value.CurrencySymbol
makeCurrencySymbol = Value.currencySymbol

getCurrencySymbol :: Scripts.MintingPolicy -> Value.CurrencySymbol
getCurrencySymbol = Ledger.scriptCurrencySymbol

getPolicyHash :: UScripts.MintingPolicy -> UScripts.MintingPolicyHash
getPolicyHash = UScripts.mintingPolicyHash

getTypedValidatorHash :: Scripts.ValidatorTypes validator => Scripts.TypedValidator validator-> Scripts.ValidatorHash
getTypedValidatorHash = Scripts.validatorHash

makeValidatorHash :: String -> Scripts.ValidatorHash
makeValidatorHash = fromString

unsafeReadTxOutRef :: String -> Ledger.TxOutRef
unsafeReadTxOutRef s =
    let
        (x, _ : y) = span (/= '#') s
    in
        Ledger.TxOutRef
                { Ledger.txOutRefId  = fromString x
                , Ledger.txOutRefIdx = read y
                }

isHex :: String -> Bool
isHex = all (`elem` ("1234567890abcdef" :: String))

unsafeHexToInt :: String -> Integer
unsafeHexToInt hex
    | hex == "0" || hex == "" = 0
    | hex == "1" = 1
    | hex == "2" = 2
    | hex == "3" = 3
    | hex == "4" = 4
    | hex == "5" = 5
    | hex == "6" = 6
    | hex == "7" = 7
    | hex == "8" = 8
    | hex == "9" = 9
    | hex == "a" = 10
    | hex == "b" = 11
    | hex == "c" = 12
    | hex == "d" = 13
    | hex == "e" = 14
    | hex == "f" = 15
    | otherwise = if not . isHex $ hex then undefined
        else (unsafeHexToInt . init $ hex) * 16 + unsafeHexToInt [ last hex ]

strToPhk :: String -> Ledger.PubKeyHash
strToPhk = Ledger.PubKeyHash . foldr PTxP.consByteString ("" :: BuiltinByteString) . hexToUint8Arr

hexToUint8Arr :: String -> [Integer]
hexToUint8Arr str =
    if not ( isHex str && even (length str)) then undefined
    else hexToUint8Arr' str
    where
        hexToUint8Arr' :: String -> [Integer]
        hexToUint8Arr' [] = []
        hexToUint8Arr' [_singleElem] = undefined
        hexToUint8Arr' ( a:b: rest) = unsafeHexToInt [a,b] : hexToUint8Arr' rest



makeFullAddress :: String -> String -> Ledger.Address
makeFullAddress paymentHash stakeHash  = Ledger.Address ( strToCredential paymentHash ) ( Just $ Ledger.StakingHash $ strToCredential stakeHash )
    where
        strToCredential = Ledger.PubKeyCredential . strToPhk


typedValidatorToScript = Ledger.unScript . Ledger.unValidatorScript . Scripts.validatorScript

scriptToString :: Script -> String
scriptToString = printScript

prettyScriptToString :: Script -> String
prettyScriptToString = PPretty.printScript

{--
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


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

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


strToTokenName :: String -> Value.TokenName
{--
```BS``` is Char8, therfore ASCII encoding, which is a valid ```[Char]``` aka ```String```
--}
strToTokenName = Value.tokenName . BS8.pack

showToTokenName :: Show showable => showable -> Value.TokenName
showToTokenName = strToTokenName . show

--}
