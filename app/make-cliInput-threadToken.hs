{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use dropWhile" #-}
module Main where

import Trace.Plutus.Utils (writeMintingPolicy, unsafeReadTxOutRef, makeTokenName)
import Trace.ThreadTokenPolicy (policy)
import qualified Data.ByteString.Char8 as BS8
import Text.Read (readMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
    ( maybeUtxoToSpend, maybeTokenName ) <- parseArgs

    utxoToSpend <-  askUntilPresent "utxo to spend missing, please insert a valid utxo:"    isUtxo       maybeUtxoToSpend
    tName <-        askUntilPresent "token name missing, please insert a name:"             (const True) maybeTokenName

    writeResult <- writeMintingPolicy "./testnet/thread-token-policy.plutus" $ policy ( unsafeReadTxOutRef utxoToSpend ) ( makeTokenName $ BS8.pack tName )
    case writeResult of
        Left err -> putStrLn "Error!"
        Right _  -> putStrLn $ "thread-token cli-input created correctly at \"" ++ "./testnet/thread-token-policy.plutus" ++ "\""
    
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