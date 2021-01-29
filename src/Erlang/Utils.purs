-- | Utilities and helpers that are basic extensions of other independent
-- | libraries. This module is unrelated to Erlang.
module Erlang.Utils where

import Data.String.CodePoints as DSCP
import Unsafe.Coerce
import Data.BigInt as DBI
import Data.Maybe as DM
import Data.Int as Int
import Effect.Unsafe(unsafePerformEffect)
import Effect.Exception(throw)

-- | Converts CodePoint to Int (for some reason removed from original lib)
codePointToInt :: DSCP.CodePoint -> Int
codePointToInt = unsafeCoerce -- it's just a newtype

-- | Converts BigInt to Int. Will be provided in a future release of BigInt.
bigIntToInt :: DBI.BigInt -> DM.Maybe Int
bigIntToInt bi = Int.fromNumber (DBI.toNumber bi)

-- | Backdoor to throwing runtime errors
runtimeError :: forall a. String -> a
runtimeError e = unsafePerformEffect (throw e)
