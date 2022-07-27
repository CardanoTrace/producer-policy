module Main where

import Trace.ThreadTokenPolicy ( threadTokenPolicy )

import Trace.Utils (
        getAndWriteScript,
        writeMintingPolicy,
        unsafeReadTxOutRef,
        makeTokenName,
        writeTypedValidator,
        getTypedValidatorHash,
        getCurrencySymbol,
        makeAssetClass, makeAssetClass',
        getPolicyHash,
        strToPhk, makeFullAddress,
        typedValidatorToScript
        , prettyScriptToString
    )
import System.Environment ( getArgs )
import Plutus.V1.Ledger.Api (Script, PubKeyHash (PubKeyHash), toBuiltin)
import Cardano.Api (FileError)

import qualified Plutus.V1.Ledger.Scripts as Plutus
import Ledger.Scripts (mintingPolicyHash, validatorHash)
import Ledger.Value (mpsSymbol, tokenName)
import Trace.MinterContract
import qualified Data.ByteString.Char8 as BS8
import Trace.TweetPolicy (tweetPolicyCont)

ttPolicyFilePath :: FilePath
ttPolicyFilePath = "./testnet/thread-token-policy.plutus.json"

minterContractFilePath :: FilePath
minterContractFilePath = "./testnet/minter-contract.plutus.json"

TracePolicyFilePath :: FilePath
TracePolicyFilePath = "./testnet/Trace-policy.plutus.json"

writeScriptTo :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScriptTo = getAndWriteScript id

main :: IO ()
main = do
    [ utxoStr, owner_pkh_str ] <- getArgs

    let utxo = unsafeReadTxOutRef utxoStr

    let threadTokenName = tokenName "TraceCounterOracle"

    let fixedThreadTokenPolicy = threadTokenPolicy utxo threadTokenName

    threadPolicyWriteRes <- writeScriptTo ttPolicyFilePath fixedThreadTokenPolicy
    case threadPolicyWriteRes of
      Left err -> print err
      Right _ -> putStrLn $ "\nthreadTokenPolicy succesfully written at: " ++ ttPolicyFilePath

    -- input of validator and Trace policy
    let threadTokenPolicyCurrencySymbol = mpsSymbol . mintingPolicyHash $ Plutus.MintingPolicy fixedThreadTokenPolicy

    let owner_pkh = PubKeyHash . toBuiltin . BS8.pack $ owner_pkh_str

    print owner_pkh

    let fixedMinterContractValidator = minterContractCont threadTokenPolicyCurrencySymbol threadTokenName owner_pkh 

    contractWriteRes <- writeScriptTo minterContractFilePath fixedMinterContractValidator
    case contractWriteRes of
        Left err -> print err
        Right _ -> putStrLn $ "\nminterContractValidator succesfully written at: " ++ minterContractFilePath


    let minterContractValidatorHash = validatorHash . Plutus.Validator $ fixedMinterContractValidator

    let fixedTracePolicy = tweetPolicyCont threadTokenPolicyCurrencySymbol threadTokenName minterContractValidatorHash

    policyWriteRes <- writeScriptTo TracePolicyFilePath fixedTracePolicy
    case contractWriteRes of
        Left err -> print err
        Right _ -> putStrLn $ "\nTracePolicy succesfully written at: " ++ TracePolicyFilePath

    return ()



parseArgs :: IO ( String, Maybe String, Maybe String)
parseArgs = do
    (phk_str : args) <- getArgs
    if null args then return ( phk_str, Nothing, Nothing)
    else if length args == 1 then return ( phk_str, Just $ head args, Nothing )
    else return ( phk_str, Just $ head args, Just $ args !! 1 )

askUntilPresent :: String -> ( String -> Bool ) -> Maybe String -> IO String
askUntilPresent question isValid prevUtxo =
    case prevUtxo of
        Nothing ->
            do
                putStrLn $ '\n' : question
                answer <- getLine
                if isValid answer then return answer
                else do
                    putStrLn "Invalid!\n"
                    askUntilPresent question isValid prevUtxo
        Just utxo -> return utxo