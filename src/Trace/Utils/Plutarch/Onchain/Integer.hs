
module Trace.Utils.Plutarch.Onchain.Integer where

import Plutarch.Prelude

import qualified PlutusCore as PLC
import Plutarch.Unsafe (punsafeBuiltin)

import Plutarch.Bool (
          pif' -- pif' is strict
        , pif  -- pif  is lazy
    )
import Data.String (IsString)
import GHC.Exts (IsString(..))
import qualified Data.ByteString.Char8 as BS8


-- allow (Term s PByteString) values as string literals (thx OverloadedStrings)
instance IsString (Term s PByteString) where
    fromString = pconstant . BS8.pack

--
pintegerToByteString :: Term s (PInteger :--> PByteString)
pintegerToByteString =
    pfix #$ plam $ \ self int ->
        pif (int #<  0) ( "-" <> self # abs int ) $
        pif (int #== 0 ) "0" $
        pif (int #== 1 ) "1" $
        pif (int #== 2 ) "2" $
        pif (int #== 3 ) "3" $
        pif (int #== 4 ) "4" $
        pif (int #== 5 ) "5" $
        pif (int #== 6 ) "6" $
        pif (int #== 7 ) "7" $
        pif (int #== 8 ) "8" $
        pif (int #== 9 ) "9"
        ( (self # (pdiv # int # 10)) <> (self # (pmod # int # 10)) )
        
--}

{--
            _ -> self # (pdiv # int # 10) <> self # (pmod # int # 10)
| n == 0 = "0"
| n == 1 = "1"
| n == 2 = "2"
| n == 3 = "3"
| n == 4 = "4"
| n == 5 = "5"
| n == 6 = "6"
| n == 7 = "7"
| n == 8 = "8"
| n == 9 = "9"
| otherwise = integerToByteString (pdiv # n # 10) <> integerToByteString (pmod # n # 10)
--}