module Erlang.Ioserver where

import Prelude (map, show, ($), (<>))
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)

import Data.Maybe as DM
import Partial.Unsafe (unsafePartial)
import Erlang.Type (ErlangTerm(..), fromErl)
import Data.String.CodePoints as StrCP
import Data.String as Str
import Unsafe.Coerce (unsafeCoerce)
import Erlang.Builtins as BIF

wololo_codepoint :: Partial => ErlangTerm -> StrCP.CodePoint
wololo_codepoint (ErlangInt res) = unsafeCoerce res

toString :: ErlangTerm -> String
toString x = unsafePartial $ Str.fromCodePointArray $ map wololo_codepoint $ DM.fromJust $ fromErl x

erlps__request__2 :: Array ErlangTerm -> ErlangTerm
erlps__request__2 [io_server@(ErlangPID _), ErlangTuple [ErlangAtom "io_request", from@(ErlangPID _), replyAs, request]] =
    let
        a = unsafePerformEffect (log (show request))
    in
        ErlangTuple [ErlangAtom "io_reply", replyAs, handle_request request]
erlps__request__2 args = ErlangAtom "ok"

handle_request :: ErlangTerm -> ErlangTerm
handle_request (ErlangTuple [ErlangAtom "put_chars", ErlangAtom "unicode", iolist]) =
    let
        str = BIF.erlang__binary_to_list__1 [BIF.erlang__iolist_to_binary__1 [iolist]]
        a = unsafePerformEffect (log ((toString str)))
    in
          ErlangAtom "ok"
handle_request (ErlangTuple [ErlangAtom "put_chars", ErlangAtom "unicode", ErlangAtom "io_lib", ErlangAtom "format", ErlangCons format (ErlangCons ErlangEmptyList ErlangEmptyList)]) =
    let
        a = unsafePerformEffect (log ((toString format)))
    in
          ErlangAtom "ok"
handle_request (ErlangTuple [ErlangAtom "put_chars", ErlangAtom "unicode", ErlangAtom "io_lib", ErlangAtom "format", ErlangCons (ErlangAtom "user") (ErlangCons format ErlangEmptyList)]) =
    let
        a = unsafePerformEffect (log ((toString format)))
    in
          ErlangAtom "ok"
handle_request (ErlangTuple [ErlangAtom "put_chars", ErlangAtom "unicode", ErlangAtom "io_lib", ErlangAtom "format", ErlangCons format (ErlangCons terms ErlangEmptyList)]) =
    let
        a = unsafePerformEffect (log ((toString format) <> " " <> show terms))
    in
          ErlangAtom "ok"
handle_request _ = ErlangAtom "ok"
