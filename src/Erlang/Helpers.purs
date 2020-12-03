module Erlang.Helpers where

import Erlang.Type
import Erlang.Exception as EXC
import Control.Monad
import Data.Maybe as DM
import Data.List as DL
import Data.Array as DA
import Data.Map as Map
import Data.String as Str
import Data.BigInt as DBI
import Data.Int as DI
import Data.String.CodePoints as StrCP
import Data.Foldable

import Partial.Unsafe
import Unsafe.Coerce
import Effect
import Effect.Unsafe(unsafePerformEffect)
import Effect.Exception(throw, catchException)
import Prelude
import Control.Semigroupoid((<<<), (>>>))

foreign import falsifyErrorsImpl :: ErlangTerm -> (Unit -> ErlangTerm) -> ErlangTerm

falsifyErrors :: (Unit -> ErlangTerm) -> ErlangTerm
falsifyErrors = falsifyErrorsImpl (ErlangAtom "false")

error :: forall a. String -> a
error = throw >>> unsafePerformEffect

isEList :: ErlangTerm -> Boolean
isEList ErlangEmptyList = true
isEList (ErlangCons _ _) = true
isEList _ = false

isENum :: ErlangTerm -> Boolean
isENum (ErlangInt _) = true
isENum _ = false

isEAtom :: ErlangTerm -> Boolean
isEAtom (ErlangAtom _) = true
isEAtom _ = false

isEBinary :: ErlangTerm -> Boolean
isEBinary (ErlangBinary _) = true
isEBinary _ = false

isETuple :: ErlangTerm -> Boolean
isETuple (ErlangTuple _) = true
isETuple _ = false

isEFun :: ErlangTerm -> Boolean
isEFun (ErlangFun _ _) = true
isEFun _ = false

isEFunA :: ErlangTerm -> ErlangTerm -> Boolean
isEFunA (ErlangFun a0 _) (ErlangInt a1) = DBI.fromInt a0 == a1
isEFunA _ _ = false

isEMap :: ErlangTerm -> Boolean
isEMap (ErlangMap _) = true
isEMap _ = false

erlToInt :: ErlangTerm -> DBI.BigInt
erlToInt (ErlangInt x) = x
erlToInt _ = error "bad int"

-- They removed support of it. CodePoint is just a newtype for Int.
codePointToInt :: StrCP.CodePoint -> Int
codePointToInt = unsafeCoerce

make_string :: String -> ErlangTerm
make_string str = arrayToErlangList (map (ErlangInt <<< DBI.fromInt <<< codePointToInt) (Str.toCodePointArray str))

flmap :: (Partial => ErlangTerm -> ErlangTerm) -> ErlangTerm -> ErlangTerm
flmap f list = unsafePartial $ erflat (ermap list ErlangEmptyList) ErlangEmptyList where
  ermap :: Partial => ErlangTerm -> ErlangTerm -> ErlangTerm
  ermap ErlangEmptyList acc = acc
  ermap (ErlangCons h t) acc = ermap t (ErlangCons (f h) acc)

  erflat :: Partial => ErlangTerm -> ErlangTerm -> ErlangTerm
  erflat ErlangEmptyList acc = acc
  erflat (ErlangCons ErlangEmptyList rest) acc = erflat rest acc
  erflat (ErlangCons (ErlangCons h t) rest) acc = erflat (ErlangCons t rest) (ErlangCons h acc)

bigIntToInt :: DBI.BigInt -> DM.Maybe Int
bigIntToInt = DBI.toNumber >>> DI.fromNumber

findMissingKey :: ErlangTerm -> Array ErlangTerm -> DM.Maybe ErlangTerm
findMissingKey (ErlangMap m) keys =
  DA.find (\key -> not (Map.member key m)) keys
findMissingKey t _ = DM.Just (EXC.badmap t)
