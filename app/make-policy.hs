{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use dropWhile" #-}

module Main where

import Trace.Policy (nftPolicy, TokenData(..) )
import Trace.Plutus.Utils (writeMintingPolicy, makeCurrencySymbol, makeValidatorHash)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS8

outPath :: String
outPath = "./testnet/producer-policy.plutus.json"

main :: IO ()
main = do
    ( maybeValHash, maybeThreadTokenSym ) <- parseArgs

    valHash <-  askUntilPresent "counter Validator's hash missing, please insert a valid ValidatorHash:"  isHex  maybeValHash
    threadTokenSym <-    askUntilPresent "thread token NFT missing, please insert a CurrencySymbol:"      isHex  maybeThreadTokenSym

    writeResult <- writeMintingPolicy outPath $ nftPolicy ( TokenData (makeValidatorHash valHash) (makeCurrencySymbol $ BS8.pack threadTokenSym) )
    case writeResult of
        Left err -> putStrLn "Error!"
        Right _  -> putStrLn $ "thread-token cli-input created correctly at \"" ++ outPath ++ "\""
    
    where
        isHex :: String -> Bool
        isHex= all (\ ch -> elem ch "abcdef1234567890")
                

parseArgs :: IO (Maybe String, Maybe String)
parseArgs = do
    args <- getArgs
    if null args then return (Nothing, Nothing)
    else if length args == 1 then return ( Just $ head args, Nothing )
    else return ( Just $ head args, Just $ args !! 1 )

askUntilPresent :: String -> ( String -> Bool ) -> Maybe String -> IO String
askUntilPresent question isValid prevUtxo =
    case prevUtxo of
        Nothing ->
            do
                putStrLn question
                answer <- getLine
                if isValid answer then return answer
                else do
                    putStrLn "Invalid!\n"
                    askUntilPresent question isValid prevUtxo
        Just utxo -> return utxo