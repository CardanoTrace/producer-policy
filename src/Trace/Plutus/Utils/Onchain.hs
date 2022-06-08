module Trace.Plutus.Utils.Onchain where

import qualified PlutusTx.Prelude     as P
import PlutusTx.Builtins ( BuiltinByteString )
import PlutusTx.Builtins.Class ( stringToBuiltinByteString )

integerToByteString :: Integer -> BuiltinByteString
integerToByteString n
    | n == 0 = stringToBuiltinByteString "0"
    | n == 1 = stringToBuiltinByteString "1"
    | n == 2 = stringToBuiltinByteString "2"
    | n == 3 = stringToBuiltinByteString "3"
    | n == 4 = stringToBuiltinByteString "4"
    | n == 5 = stringToBuiltinByteString "5"
    | n == 6 = stringToBuiltinByteString "6"
    | n == 7 = stringToBuiltinByteString "7"
    | n == 8 = stringToBuiltinByteString "8"
    | n == 9 = stringToBuiltinByteString "9"
    | otherwise = integerToByteString (n `P.divide` 10) P.<> integerToByteString (n `P.modulo` 10)