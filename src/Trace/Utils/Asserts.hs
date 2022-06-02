
module Trace.Utils.Asserts where

import qualified PlutusTx.Prelude as PTxP

assertTrue :: Bool -> PTxP.BuiltinString  -> Bool 
assertTrue = flip PTxP.traceIfFalse

assertFalse :: Bool -> PTxP.BuiltinString  -> Bool
assertFalse = flip PTxP.traceIfTrue