
module Main where

import Trace.Plutus.Utils (writeTypedValidator, makeAssetClass)
import Trace.MinterContract (minterContractTypedValidator)
import qualified Data.ByteString.Char8 as BS8

outPath :: String
outPath = "./testnet/counter-validator.plutus.json"

main :: IO ()
main = do
    writeResult <- writeTypedValidator outPath $ minterContractTypedValidator (makeAssetClass (BS8.pack "deadbeef") (BS8.pack "fake" ) )
    case writeResult of
        Left err -> putStrLn "Error!"
        Right _  -> putStrLn $ "validator cli-input created correctly at \"" ++ outPath ++ "\""
    