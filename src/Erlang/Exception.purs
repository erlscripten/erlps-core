-- | Functions responsible for exception building and handling
module Erlang.Exception
  ( tryCatchFinally
  , tryOfCatchFinally
  , tryCatch
  , tryOfCatch
  , raise
  , throw, error, exit
  , function_clause, case_clause, try_clause, if_clause
  , badarity, badmatch, badarg, badarg1, badrecord, bad_generator, badmap, badkey
  ) where

import Prelude (Unit, unit, ($))
import Erlang.Type (ErlangTerm(..), toErl)


buildException :: String -> ErlangTerm -> String -> ErlangTerm
buildException exType exPayload exStack = ErlangTuple
  [ ErlangAtom exType
  , exPayload
  , ErlangAtom exStack
  ]

foreign import raise :: forall a. ErlangTerm -> a

foreign import getStack :: Unit -> String

foreign import tryCatchFinally
  :: forall a. (Unit -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> (Unit -> a)
  -> ErlangTerm

foreign import tryOfCatchFinally
  :: forall a. (Unit -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> (Unit -> a)
  -> ErlangTerm


foreign import tryCatch
  :: (Unit -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> ErlangTerm

foreign import tryOfCatch
  :: (Unit -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> ErlangTerm

throw :: forall a. ErlangTerm -> a
throw term =
  raise $ buildException "throw" term (getStack unit)

error :: forall a. ErlangTerm -> a
error term =
  raise $ buildException "error" term (getStack unit)

exit :: forall a. ErlangTerm -> a
exit term =
  raise $ buildException "exit" term (getStack unit)


function_clause :: Unit -> ErlangTerm
function_clause _ = error (ErlangAtom "function_clause")

case_clause :: ErlangTerm -> ErlangTerm
case_clause term = error (ErlangTuple [ErlangAtom "case_clause", term])

if_clause :: Unit -> ErlangTerm
if_clause _ = error (ErlangAtom "if_clause")

try_clause :: ErlangTerm -> ErlangTerm
try_clause term = error (ErlangTuple [ErlangAtom "try_clause", term])

badmatch :: ErlangTerm -> ErlangTerm
badmatch term = error (ErlangTuple [ErlangAtom "badmatch", term])

badarg :: forall a. Unit -> a
badarg _ = error (ErlangAtom "badarg")

badarg1 :: forall a. ErlangTerm -> a
badarg1 term = error (ErlangTuple [ErlangAtom "badarg", term])

badarity :: ErlangTerm -> Array ErlangTerm -> ErlangTerm
badarity fun args =
  error (ErlangTuple [ErlangAtom "badarity", ErlangTuple [fun, toErl args]])

badrecord :: ErlangTerm -> ErlangTerm
badrecord _ = error (ErlangAtom "TODO: PROPER BADRECORD ERROR")

bad_generator :: ErlangTerm -> ErlangTerm
bad_generator term = error (ErlangTuple [ErlangAtom "bad_generator", term])

badmap :: ErlangTerm -> ErlangTerm
badmap term = error (ErlangTuple [ErlangAtom "badmap", term])

badkey :: ErlangTerm -> ErlangTerm
badkey term = error (ErlangTuple [ErlangAtom "badkey", term])
