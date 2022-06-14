{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use dropWhile" #-}
module Main where


import Trace.Plutus.Utils (
        writeMintingPolicy,
        unsafeReadTxOutRef,
        makeTokenName,
        writeTypedValidator,
        getTypedValidatorHash,
        getCurrencySymbol,
        makeAssetClass, makeAssetClass',
        getPolicyHash,
        strToPhk
    )

import Trace.ThreadTokenPolicy (policy)
import Trace.Policy (nftPolicy, TokenData(..) )

import qualified Data.ByteString.Char8 as BS8
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Trace.MinterContract (minterContractTypedValidator)


threadTokenOutPath :: String
threadTokenOutPath = "./testnet/thread-token-policy.plutus.json"

threadTokenHashOutPath :: String
threadTokenHashOutPath = "./testnet/thread-token-policy.hash.txt"

validatorOutPath :: String
validatorOutPath = "./testnet/counter-validator.plutus.json"

policyOutPath :: String
policyOutPath = "./testnet/producer-policy.plutus.json"

main :: IO ()
main = do
    ( addr_publicKeyHash, maybeUtxoToSpend, maybeTokenName ) <- parseArgs

    utxoToSpend <-  askUntilPresent "utxo to spend missing, please insert a valid utxo:"    isUtxo       maybeUtxoToSpend
    tName <-        askUntilPresent "thread token name missing, please insert a name:"      (const True) maybeTokenName

    let ttPolicy = policy ( unsafeReadTxOutRef utxoToSpend ) ( makeTokenName $ BS8.pack tName )

    writeFile threadTokenHashOutPath $ show $ getPolicyHash ttPolicy

    threadTokenWriteResult <- writeMintingPolicy threadTokenOutPath ttPolicy
    case threadTokenWriteResult of
        Left err -> print err
        Right _  -> putStrLn $ "\nthread-token cli-input created correctly at \"" ++ threadTokenOutPath ++ "\""


    let validator = minterContractTypedValidator $ {-( strToPhk addr_publicKeyHash ,-} makeAssetClass' ( getCurrencySymbol ttPolicy ) (BS8.pack tName ) --)

    validatorWriteResult <- writeTypedValidator validatorOutPath validator
    case validatorWriteResult of
        Left err -> print err
        Right _  -> putStrLn $ "\nvalidator cli-input created correctly at \"" ++ validatorOutPath ++ "\""

    policyWriteResult <- writeMintingPolicy policyOutPath $ nftPolicy ( TokenData (getTypedValidatorHash validator) ( getCurrencySymbol ttPolicy ) )
    case policyWriteResult of
        Left err -> print err
        Right _  -> putStrLn $ "\nthread-token cli-input created correctly at \"" ++ policyOutPath ++ "\""
    
    
    where
        isUtxo :: String -> Bool
        isUtxo str = 
            let 
                ( hexStr , _ : numStr) = span (/= '#') str
            in
                if snd ( span (/= '#') str ) == [] then False
                else case (readMaybe numStr :: Maybe Integer) of
                    Nothing -> False
                    Just _num -> isHex hexStr

        isHex :: String -> Bool
        isHex= all (\ ch -> elem ch "abcdef1234567890")
                

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