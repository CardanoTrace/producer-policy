
module Main where

import Trace.Plutus.Utils (writeTypedValidator, makeAssetClass)
import Trace.MinterContract (minterContractTypedValidator)
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do

    writeResult <- writeTypedValidator "./testnet/counter-validator.plutus" $ writeMintingPolicy (makeAssetClass (BS8.pack "deadbeef") (BS8.pack "fake" ) )
    case writeResult of
        Left err -> putStrLn "Error!"a
        Right _  -> putStrLn "validator cli-input created correctly at \"" ++ "./testnet/counter-validator.plutus" ++ "\""
    