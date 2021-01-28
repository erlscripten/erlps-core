module Erlang.Io(erlps__put_chars__1, erlps__put_chars__2,
                 erlps__nl__0, erlps__nl__1, erlps__get_chars__2,
                 erlps__get_chars__3, erlps__get_line__1, erlps__get_line__2,
                 erlps__get_password__0, erlps__get_password__1,
                 erlps__setopts__1, erlps__setopts__2, erlps__getopts__0,
                 erlps__getopts__1, erlps__write__1, erlps__write__2,
                 erlps__read__1, erlps__read__2, erlps__read__3, erlps__read__4,
                 erlps__columns__0, erlps__columns__1, erlps__rows__0,
                 erlps__rows__1, erlps__fwrite__1, erlps__fwrite__2,
                 erlps__fwrite__3, erlps__fread__2, erlps__fread__3,
                 erlps__format__1, erlps__format__2, erlps__format__3,
                 erlps__request__1, erlps__request__2, erlps__requests__1,
                 erlps__requests__2, erlps__printable_range__0) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.0.2
-}

import Prelude (unit, (==))
import Data.BigInt as DBI
import Erlang.Builtins as BIF
import Erlang.Helpers (falsifyErrors)
import Erlang.Exception as EXC
import Erlang.Type (ErlangFun, ErlangTerm(..), isEAtom, isEList, isETuple, toErl)


erlps__to_tuple__1 :: ErlangFun
erlps__to_tuple__1 [t_0] | (isETuple t_0) = t_0
erlps__to_tuple__1 [t_0] = (ErlangTuple [t_0])
erlps__to_tuple__1 [arg_2] = (EXC.function_clause unit)
erlps__to_tuple__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__o_request__3 :: ErlangFun
erlps__o_request__3 [io_0, request_1, func_2] =
  let case_3 = (erlps__request__2 [io_0, request_1])
  in
    case case_3 of
      (ErlangTuple [(ErlangAtom "error"), reason_6]) ->
        let    arg_7 = (erlps__to_tuple__1 [request_1])
        in let match_expr_11 = (BIF.erlang__tuple_to_list__1 [arg_7])
        in
          case match_expr_11 of
            (ErlangCons _name_9 args_10) ->
              let
                match_expr_27 =
                  (EXC.tryCatch (\ _ -> (BIF.erlang__error__1 [(ErlangAtom "get_stacktrace")]))
                     (\ ex_14 ->
                        case ex_14 of
                          (ErlangTuple [(ErlangAtom "throw"), payload_15, _]) ->
                            payload_15
                          (ErlangTuple [(ErlangAtom "error"), payload_16,
                                        stack_17]) ->
                            let tup_el_19 = (ErlangTuple [payload_16, stack_17])
                            in (ErlangTuple [(ErlangAtom "EXIT"), tup_el_19])
                          (ErlangTuple [(ErlangAtom "exit"), payload_22, _]) ->
                            (ErlangTuple [(ErlangAtom "EXIT"), payload_22])
                          ex -> (EXC.raise ex)))
              in
                case match_expr_27 of
                  (ErlangTuple [(ErlangAtom "EXIT"),
                                (ErlangTuple [(ErlangAtom "get_stacktrace"),
                                              (ErlangCons _current_25 mfas_26)])]) ->
                    let    arg_29 = (erlps__conv_reason__2 [func_2, reason_6])
                    in let
                      head_33 =
                        (ErlangTuple
                           [(ErlangAtom "io"), func_2, (ErlangCons io_0 args_10)])
                    in
                      (BIF.erlang__raise__3
                         [(ErlangAtom "error"), arg_29, (ErlangCons head_33 mfas_26)])
                  _ -> (EXC.badmatch match_expr_27)
            _ -> (EXC.badmatch match_expr_11)
      other_40 -> other_40
erlps__o_request__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__printable_range__0 :: ErlangFun
erlps__printable_range__0 [] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undefined")])
erlps__printable_range__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__put_chars__1 :: ErlangFun
erlps__put_chars__1 [chars_0] =
  let arg_1 = (erlps__default_output__0 [])
  in (erlps__put_chars__2 [arg_1, chars_0])
erlps__put_chars__1 [arg_3] = (EXC.function_clause unit)
erlps__put_chars__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__put_chars__2 :: ErlangFun
erlps__put_chars__2 [io_0, chars_1] =
  (erlps__put_chars__3 [io_0, (ErlangAtom "unicode"), chars_1])
erlps__put_chars__2 [arg_5, arg_6] = (EXC.function_clause unit)
erlps__put_chars__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__put_chars__3 :: ErlangFun
erlps__put_chars__3 [io_0, encoding_1, chars_2] =
  let arg_4 = (ErlangTuple [(ErlangAtom "put_chars"), encoding_1, chars_2])
  in (erlps__o_request__3 [io_0, arg_4, (ErlangAtom "put_chars")])
erlps__put_chars__3 [arg_9, arg_10, arg_11] =
  (EXC.function_clause unit)
erlps__put_chars__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__nl__0 :: ErlangFun
erlps__nl__0 [] =
  let arg_0 = (erlps__default_output__0 [])
  in (erlps__nl__1 [arg_0])
erlps__nl__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__nl__1 :: ErlangFun
erlps__nl__1 [io_0] =
  (erlps__o_request__3 [io_0, (ErlangAtom "nl"), (ErlangAtom "nl")])
erlps__nl__1 [arg_4] = (EXC.function_clause unit)
erlps__nl__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__columns__0 :: ErlangFun
erlps__columns__0 [] =
  let arg_0 = (erlps__default_output__0 [])
  in (erlps__columns__1 [arg_0])
erlps__columns__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__columns__1 :: ErlangFun
erlps__columns__1 [io_0] =
  let    arg_3 = (ErlangTuple [(ErlangAtom "get_geometry"), (ErlangAtom "columns")])
  in let case_1 = (erlps__request__2 [io_0, arg_3])
  in
    case case_1 of
      n_6 | (ErlangAtom "true") <-
              ((falsifyErrors
                  (\ _ ->
                     let lop_7 = (BIF.erlang__is_integer__1 [n_6])
                     in
                       case lop_7 of
                         (ErlangAtom "false") -> (ErlangAtom "false")
                         (ErlangAtom "true") ->
                           (BIF.erlang__op_greater
                              [n_6, (ErlangInt (DBI.fromInt 0))])
                         _ -> (EXC.badarg1 lop_7)))) ->
        (ErlangTuple [(ErlangAtom "ok"), n_6])
      _ -> (ErlangTuple [(ErlangAtom "error"), (ErlangAtom "enotsup")])
erlps__columns__1 [arg_15] = (EXC.function_clause unit)
erlps__columns__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__rows__0 :: ErlangFun
erlps__rows__0 [] =
  let arg_0 = (erlps__default_output__0 [])
  in (erlps__rows__1 [arg_0])
erlps__rows__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__rows__1 :: ErlangFun
erlps__rows__1 [io_0] =
  let    arg_3 = (ErlangTuple [(ErlangAtom "get_geometry"), (ErlangAtom "rows")])
  in let case_1 = (erlps__request__2 [io_0, arg_3])
  in
    case case_1 of
      n_6 | (ErlangAtom "true") <-
              ((falsifyErrors
                  (\ _ ->
                     let lop_7 = (BIF.erlang__is_integer__1 [n_6])
                     in
                       case lop_7 of
                         (ErlangAtom "false") -> (ErlangAtom "false")
                         (ErlangAtom "true") ->
                           (BIF.erlang__op_greater
                              [n_6, (ErlangInt (DBI.fromInt 0))])
                         _ -> (EXC.badarg1 lop_7)))) ->
        (ErlangTuple [(ErlangAtom "ok"), n_6])
      _ -> (ErlangTuple [(ErlangAtom "error"), (ErlangAtom "enotsup")])
erlps__rows__1 [arg_15] = (EXC.function_clause unit)
erlps__rows__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__get_chars__2 :: ErlangFun
erlps__get_chars__2 [prompt_0, n_1] =
  let arg_2 = (erlps__default_input__0 [])
  in (erlps__get_chars__3 [arg_2, prompt_0, n_1])
erlps__get_chars__2 [arg_5, arg_6] = (EXC.function_clause unit)
erlps__get_chars__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__get_chars__3 :: ErlangFun
erlps__get_chars__3 [io_0, prompt_1, n_2]
  | (ErlangAtom "true") <-
      ((falsifyErrors
          (\ _ ->
             let lop_9 = (BIF.erlang__is_integer__1 [n_2])
             in
               case lop_9 of
                 (ErlangAtom "false") -> (ErlangAtom "false")
                 (ErlangAtom "true") ->
                   (BIF.erlang__op_greaterEq [n_2, (ErlangInt (DBI.fromInt 0))])
                 _ -> (EXC.badarg1 lop_9)))) =
  let
    arg_4 =
      (ErlangTuple [(ErlangAtom "get_chars"), (ErlangAtom "unicode"), prompt_1, n_2])
  in (erlps__request__2 [io_0, arg_4])
erlps__get_chars__3 [arg_13, arg_14, arg_15] =
  (EXC.function_clause unit)
erlps__get_chars__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__get_line__1 :: ErlangFun
erlps__get_line__1 [prompt_0] =
  let arg_1 = (erlps__default_input__0 [])
  in (erlps__get_line__2 [arg_1, prompt_0])
erlps__get_line__1 [arg_3] = (EXC.function_clause unit)
erlps__get_line__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__get_line__2 :: ErlangFun
erlps__get_line__2 [io_0, prompt_1] =
  let
    arg_3 = (ErlangTuple [(ErlangAtom "get_line"), (ErlangAtom "unicode"), prompt_1])
  in (erlps__request__2 [io_0, arg_3])
erlps__get_line__2 [arg_7, arg_8] = (EXC.function_clause unit)
erlps__get_line__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__get_password__0 :: ErlangFun
erlps__get_password__0 [] =
  let arg_0 = (erlps__default_input__0 [])
  in (erlps__get_password__1 [arg_0])
erlps__get_password__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__get_password__1 :: ErlangFun
erlps__get_password__1 [io_0] =
  let arg_2 = (ErlangTuple [(ErlangAtom "get_password"), (ErlangAtom "unicode")])
  in (erlps__request__2 [io_0, arg_2])
erlps__get_password__1 [arg_5] = (EXC.function_clause unit)
erlps__get_password__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__getopts__0 :: ErlangFun
erlps__getopts__0 [] =
  let arg_0 = (erlps__default_input__0 [])
  in (erlps__getopts__1 [arg_0])
erlps__getopts__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__getopts__1 :: ErlangFun
erlps__getopts__1 [io_0] =
  (erlps__request__2 [io_0, (ErlangAtom "getopts")])
erlps__getopts__1 [arg_3] = (EXC.function_clause unit)
erlps__getopts__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__setopts__1 :: ErlangFun
erlps__setopts__1 [opts_0] =
  let arg_1 = (erlps__default_input__0 [])
  in (erlps__setopts__2 [arg_1, opts_0])
erlps__setopts__1 [arg_3] = (EXC.function_clause unit)
erlps__setopts__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__setopts__2 :: ErlangFun
erlps__setopts__2 [io_0, opts_1] =
  let arg_3 = (ErlangTuple [(ErlangAtom "setopts"), opts_1])
  in (erlps__request__2 [io_0, arg_3])
erlps__setopts__2 [arg_6, arg_7] = (EXC.function_clause unit)
erlps__setopts__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__write__1 :: ErlangFun
erlps__write__1 [term_0] =
  let arg_1 = (erlps__default_output__0 [])
  in (erlps__write__2 [arg_1, term_0])
erlps__write__1 [arg_3] = (EXC.function_clause unit)
erlps__write__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__write__2 :: ErlangFun
erlps__write__2 [io_0, term_1] =
  let arg_3 = (ErlangTuple [(ErlangAtom "write"), term_1])
  in (erlps__o_request__3 [io_0, arg_3, (ErlangAtom "write")])
erlps__write__2 [arg_7, arg_8] = (EXC.function_clause unit)
erlps__write__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__read__1 :: ErlangFun
erlps__read__1 [prompt_0] =
  let arg_1 = (erlps__default_input__0 [])
  in (erlps__read__2 [arg_1, prompt_0])
erlps__read__1 [arg_3] = (EXC.function_clause unit)
erlps__read__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__read__2 :: ErlangFun
erlps__read__2 [io_0, prompt_1] =
  let   
    arg_4 =
      (ErlangTuple
         [(ErlangAtom "get_until"), (ErlangAtom "unicode"), prompt_1, (ErlangAtom "erl_scan"),
          (ErlangAtom "tokens"),
          (ErlangCons (ErlangInt (DBI.fromInt 1)) ErlangEmptyList)])
  in let case_2 = (erlps__request__2 [io_0, arg_4])
  in
    case case_2 of
      (ErlangTuple [(ErlangAtom "ok"), toks_13, _endline_14]) ->
        case (ErlangInt (DBI.fromInt 2)) of
          (ErlangInt num_17) | ((ErlangInt num_17) ==
                                  (ErlangInt (DBI.fromInt 1))) ->
            (ErlangInt (DBI.fromInt 2))
          _ -> (EXC.badmatch (ErlangInt (DBI.fromInt 2)))
      (ErlangTuple [(ErlangAtom "error"), e_19, _endline_20]) ->
        (ErlangTuple [(ErlangAtom "error"), e_19])
      (ErlangTuple [(ErlangAtom "eof"), _endline_23]) -> (ErlangAtom "eof")
      other_24 -> other_24
erlps__read__2 [arg_25, arg_26] = (EXC.function_clause unit)
erlps__read__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__read__3 :: ErlangFun
erlps__read__3 [io_0, prompt_1, pos0_2] =
  (erlps__read__4 [io_0, prompt_1, pos0_2, ErlangEmptyList])
erlps__read__3 [arg_7, arg_8, arg_9] = (EXC.function_clause unit)
erlps__read__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__read__4 :: ErlangFun
erlps__read__4 [io_0, prompt_1, pos0_2, options_3] =
  let   
    arg_11 =
      (ErlangTuple
         [(ErlangAtom "get_until"), (ErlangAtom "unicode"), prompt_1, (ErlangAtom "erl_scan"),
          (ErlangAtom "tokens"),
          (ErlangCons pos0_2 (ErlangCons options_3 ErlangEmptyList))])
  in let case_9 = (erlps__request__2 [io_0, arg_11])
  in
    case case_9 of
      (ErlangTuple [(ErlangAtom "ok"), toks_18, endlocation_19]) ->
        case (ErlangInt (DBI.fromInt 2)) of
          (ErlangInt num_22) | ((ErlangInt num_22) ==
                                  (ErlangInt (DBI.fromInt 1))) ->
            (ErlangInt (DBI.fromInt 2))
          _ -> (EXC.badmatch (ErlangInt (DBI.fromInt 2)))
      error_26@(ErlangTuple [(ErlangAtom "error"), _e_24,
                             _endlocation_25]) ->
        error_26
      eof_28@(ErlangTuple [(ErlangAtom "eof"), _endlocation_27]) -> eof_28
      other_29 -> other_29
erlps__read__4 [arg_30, arg_31, arg_32, arg_33] =
  (EXC.function_clause unit)
erlps__read__4 args =
  (EXC.badarity (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__conv_reason__2 :: ErlangFun
erlps__conv_reason__2 [_, (ErlangAtom "arguments")] = (ErlangAtom "badarg")
erlps__conv_reason__2 [_, (ErlangAtom "terminated")] = (ErlangAtom "terminated")
erlps__conv_reason__2 [_, (ErlangTuple [(ErlangAtom "no_translation"), _, _])] =
  (ErlangAtom "no_translation")
erlps__conv_reason__2 [_, _reason_0] = (ErlangAtom "badarg")
erlps__conv_reason__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__conv_reason__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fwrite__1 :: ErlangFun
erlps__fwrite__1 [format_0] = (erlps__format__1 [format_0])
erlps__fwrite__1 [arg_2] = (EXC.function_clause unit)
erlps__fwrite__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fwrite__2 :: ErlangFun
erlps__fwrite__2 [format_0, args_1] =
  (erlps__format__2 [format_0, args_1])
erlps__fwrite__2 [arg_4, arg_5] = (EXC.function_clause unit)
erlps__fwrite__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fwrite__3 :: ErlangFun
erlps__fwrite__3 [io_0, format_1, args_2] =
  (erlps__format__3 [io_0, format_1, args_2])
erlps__fwrite__3 [arg_6, arg_7, arg_8] =
  (EXC.function_clause unit)
erlps__fwrite__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fread__2 :: ErlangFun
erlps__fread__2 [prompt_0, format_1] =
  let arg_2 = (erlps__default_input__0 [])
  in (erlps__fread__3 [arg_2, prompt_0, format_1])
erlps__fread__2 [arg_5, arg_6] = (EXC.function_clause unit)
erlps__fread__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fread__3 :: ErlangFun
erlps__fread__3 [io_0, prompt_1, format_2] =
  let arg_4 = (ErlangTuple [(ErlangAtom "fread"), prompt_1, format_2])
  in (erlps__request__2 [io_0, arg_4])
erlps__fread__3 [arg_8, arg_9, arg_10] =
  (EXC.function_clause unit)
erlps__fread__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__format__1 :: ErlangFun
erlps__format__1 [format_0] =
  (erlps__format__2 [format_0, ErlangEmptyList])
erlps__format__1 [arg_3] = (EXC.function_clause unit)
erlps__format__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__format__2 :: ErlangFun
erlps__format__2 [format_0, args_1] =
  let arg_2 = (erlps__default_output__0 [])
  in (erlps__format__3 [arg_2, format_0, args_1])
erlps__format__2 [arg_5, arg_6] = (EXC.function_clause unit)
erlps__format__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__format__3 :: ErlangFun
erlps__format__3 [io_0, format_1, args_2] =
  let arg_4 = (ErlangTuple [(ErlangAtom "format"), format_1, args_2])
  in (erlps__o_request__3 [io_0, arg_4, (ErlangAtom "format")])
erlps__format__3 [arg_9, arg_10, arg_11] =
  (EXC.function_clause unit)
erlps__format__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__request__1 :: ErlangFun
erlps__request__1 [request_0] =
  let arg_1 = (erlps__default_output__0 [])
  in (erlps__request__2 [arg_1, request_0])
erlps__request__1 [arg_3] = (EXC.function_clause unit)
erlps__request__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__request__2 :: ErlangFun
erlps__request__2 [(ErlangAtom "standard_io"), request_0] =
  let arg_1 = (BIF.erlang__group_leader__0 [])
  in (erlps__request__2 [arg_1, request_0])
erlps__request__2 [pid_0, request_1]
  | (ErlangAtom "true") <-
      ((falsifyErrors (\ _ -> (BIF.erlang__is_pid__1 [pid_0])))) =
  let arg_3 = (erlps__io_request__2 [pid_0, request_1])
  in (erlps__execute_request__2 [pid_0, arg_3])
erlps__request__2 [name_0, request_1] | (isEAtom name_0) =
  let case_2 = (BIF.erlang__whereis__1 [name_0])
  in
    case case_2 of
      (ErlangAtom "undefined") -> (ErlangTuple [(ErlangAtom "error"), (ErlangAtom "arguments")])
      pid_6 -> (erlps__request__2 [pid_6, request_1])
erlps__request__2 [arg_9, arg_10] = (EXC.function_clause unit)
erlps__request__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__execute_request__2 :: ErlangFun
erlps__execute_request__2 [pid_0,
                           (ErlangTuple [convert_1, converted_2])]
  =
  let    mref_3 = (BIF.erlang__make_ref__0 [])
  in let tup_el_8 = (BIF.erlang__self__0 [])
  in let
    arg_6 =
      (ErlangTuple [(ErlangAtom "io_request"), tup_el_8, mref_3, converted_2])
  in let case_4 = (BIF.do_remote_fun_call "Erlang.Ioserver" "erlps__request__2" [pid_0, arg_6])
  in
    case case_4 of
      (ErlangTuple [(ErlangAtom "io_reply"), mref_11, reply_12]) | (mref_11 ==
                                                             mref_3) ->
        case (ErlangAtom "true") of
          _ | (ErlangAtom "true") <- ((falsifyErrors (\ _ -> convert_1))) ->
            (erlps__convert_binaries__1 [reply_12])
          _ -> reply_12
      something_else -> (EXC.case_clause something_else)
erlps__execute_request__2 [arg_14, arg_15] =
  (EXC.function_clause unit)
erlps__execute_request__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__requests__1 :: ErlangFun
erlps__requests__1 [requests_0] =
  let arg_1 = (erlps__default_output__0 [])
  in (erlps__requests__2 [arg_1, requests_0])
erlps__requests__1 [arg_3] = (EXC.function_clause unit)
erlps__requests__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__requests__2 :: ErlangFun
erlps__requests__2 [(ErlangAtom "standard_io"), requests_0] =
  let arg_1 = (BIF.erlang__group_leader__0 [])
  in (erlps__requests__2 [arg_1, requests_0])
erlps__requests__2 [pid_0, requests_1]
  | (ErlangAtom "true") <-
      ((falsifyErrors (\ _ -> (BIF.erlang__is_pid__1 [pid_0])))) =
  let match_expr_6 = (erlps__io_requests__2 [pid_0, requests_1])
  in
    case match_expr_6 of
      (ErlangTuple [convert_4, converted_5]) ->
        let    tup_el_10 = (ErlangTuple [(ErlangAtom "requests"), converted_5])
        in let arg_8 = (ErlangTuple [convert_4, tup_el_10])
        in (erlps__execute_request__2 [pid_0, arg_8])
      _ -> (EXC.badmatch match_expr_6)
erlps__requests__2 [name_0, requests_1] | (isEAtom name_0) =
  let case_2 = (BIF.erlang__whereis__1 [name_0])
  in
    case case_2 of
      (ErlangAtom "undefined") -> (ErlangTuple [(ErlangAtom "error"), (ErlangAtom "arguments")])
      pid_6 -> (erlps__requests__2 [pid_6, requests_1])
erlps__requests__2 [arg_9, arg_10] = (EXC.function_clause unit)
erlps__requests__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__default_input__0 :: ErlangFun
erlps__default_input__0 [] = (BIF.erlang__group_leader__0 [])
erlps__default_input__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__default_output__0 :: ErlangFun
erlps__default_output__0 [] = (BIF.erlang__group_leader__0 [])
erlps__default_output__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__io_requests__2 :: ErlangFun
erlps__io_requests__2 [pid_0, rs_1] =
  (erlps__io_requests__4
     [pid_0, rs_1, ErlangEmptyList, ErlangEmptyList])
erlps__io_requests__2 [arg_6, arg_7] = (EXC.function_clause unit)
erlps__io_requests__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__io_requests__4 :: ErlangFun
erlps__io_requests__4 [pid_0,
                       (ErlangCons (ErlangTuple [(ErlangAtom "requests"), rs1_1]) rs_2),
                       cont_3, tail_4]
  =
  (erlps__io_requests__4
     [pid_0, rs1_1, (ErlangCons rs_2 cont_3), tail_4])
erlps__io_requests__4 [pid_0, (ErlangCons r_1 (ErlangEmptyList)),
                       (ErlangEmptyList), _tail_2]
  =
  let match_expr_7 = (erlps__io_request__2 [pid_0, r_1])
  in
    case match_expr_7 of
      (ErlangTuple [conv_5, request_6]) ->
        (ErlangTuple [conv_5, (ErlangCons request_6 ErlangEmptyList)])
      _ -> (EXC.badmatch match_expr_7)
erlps__io_requests__4 [pid_0, (ErlangCons r_1 rs_2), cont_3,
                       tail_4]
  =
  let match_expr_8 = (erlps__io_request__2 [pid_0, r_1])
  in
    case match_expr_8 of
      (ErlangTuple [_, request_7]) ->
        let
          match_expr_15 =
            (erlps__io_requests__4 [pid_0, rs_2, cont_3, tail_4])
        in
          case match_expr_15 of
            (ErlangTuple [conv_13, requests_14]) ->
              (ErlangTuple [conv_13, (ErlangCons request_7 requests_14)])
            _ -> (EXC.badmatch match_expr_15)
      _ -> (EXC.badmatch match_expr_8)
erlps__io_requests__4 [pid_0, (ErlangEmptyList),
                       (ErlangCons rs_1 cont_2), tail_3]
  =
  (erlps__io_requests__4 [pid_0, rs_1, cont_2, tail_3])
erlps__io_requests__4 [_pid_0, (ErlangEmptyList),
                       (ErlangEmptyList), _tail_1]
  =
  (ErlangTuple [(ErlangAtom "false"), ErlangEmptyList])
erlps__io_requests__4 [arg_4, arg_5, arg_6, arg_7] =
  (EXC.function_clause unit)
erlps__io_requests__4 args =
  (EXC.badarity (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__bc_req__3 :: ErlangFun
erlps__bc_req__3 [pid_0, req0_1, maybeconvert_2] =
  case (ErlangAtom "true") of
    (ErlangAtom "true") -> (ErlangTuple [(ErlangAtom "false"), req0_1])
    (ErlangAtom "false") ->
      let case_6 = (BIF.erlang__tuple_to_list__1 [req0_1])
      in
        case case_6 of
          (ErlangCons op_8 (ErlangCons _enc_9 (ErlangEmptyList))) ->
            (ErlangTuple [maybeconvert_2, op_8])
          (ErlangCons op_12 (ErlangCons _enc_13 t_14)) ->
            let
              req_18 = (BIF.erlang__list_to_tuple__1 [(ErlangCons op_12 t_14)])
            in (ErlangTuple [maybeconvert_2, req_18])
          something_else -> (EXC.case_clause something_else)
    something_else -> (EXC.case_clause something_else)
erlps__bc_req__3 [arg_21, arg_22, arg_23] =
  (EXC.function_clause unit)
erlps__bc_req__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__io_request__2 :: ErlangFun
erlps__io_request__2 [pid_0,
                      (ErlangTuple [(ErlangAtom "write"), term_1])]
  =
  let
    arg_3 =
      (ErlangTuple
         [(ErlangAtom "put_chars"), (ErlangAtom "unicode"), (ErlangAtom "io_lib"), (ErlangAtom "write"),
          (ErlangCons term_1 ErlangEmptyList)])
  in (erlps__bc_req__3 [pid_0, arg_3, (ErlangAtom "false")])
erlps__io_request__2 [pid_0,
                      (ErlangTuple [(ErlangAtom "format"), format_1, args_2])]
  =
  let
    arg_4 =
      (ErlangTuple
         [(ErlangAtom "put_chars"), (ErlangAtom "unicode"), (ErlangAtom "io_lib"), (ErlangAtom "format"),
          (ErlangCons format_1 (ErlangCons args_2 ErlangEmptyList))])
  in (erlps__bc_req__3 [pid_0, arg_4, (ErlangAtom "false")])
erlps__io_request__2 [pid_0,
                      (ErlangTuple [(ErlangAtom "fwrite"), format_1, args_2])]
  =
  let
    arg_4 =
      (ErlangTuple
         [(ErlangAtom "put_chars"), (ErlangAtom "unicode"), (ErlangAtom "io_lib"), (ErlangAtom "fwrite"),
          (ErlangCons format_1 (ErlangCons args_2 ErlangEmptyList))])
  in (erlps__bc_req__3 [pid_0, arg_4, (ErlangAtom "false")])
erlps__io_request__2 [pid_0, (ErlangAtom "nl")] =
  let    tup_el_5 = (toErl "\n")
  in let
    arg_2 = (ErlangTuple [(ErlangAtom "put_chars"), (ErlangAtom "unicode"), tup_el_5])
  in (erlps__bc_req__3 [pid_0, arg_2, (ErlangAtom "false")])
erlps__io_request__2 [_pid_0,
                      request0_3@(ErlangTuple [(ErlangAtom "put_chars"), enc_1, chars_2])]
  | (isEList chars_2) =
  let   
    case_4 =
      (EXC.tryCatch
         (\ _ -> (BIF.do_remote_fun_call "Erlang.Unicode" "erlps__characters_to_binary__2" [chars_2, enc_1]))
         (\ ex_8 ->
            case ex_8 of
              (ErlangTuple [(ErlangAtom "throw"), payload_9, _]) -> payload_9
              (ErlangTuple [(ErlangAtom "error"), payload_10, stack_11]) ->
                let tup_el_13 = (ErlangTuple [payload_10, stack_11])
                in (ErlangTuple [(ErlangAtom "EXIT"), tup_el_13])
              (ErlangTuple [(ErlangAtom "exit"), payload_16, _]) ->
                (ErlangTuple [(ErlangAtom "EXIT"), payload_16])
              ex -> (EXC.raise ex)))
  in let
    request_24 =
      case case_4 of
        binary_19 | (ErlangAtom "true") <-
                      ((falsifyErrors
                          (\ _ -> (BIF.erlang__is_binary__1 [binary_19])))) ->
          (ErlangTuple [(ErlangAtom "put_chars"), enc_1, binary_19])
        _ -> request0_3
  in (ErlangTuple [(ErlangAtom "false"), request_24])
erlps__io_request__2 [pid_0,
                      (ErlangTuple [(ErlangAtom "fread"), prompt_1, format_2])]
  =
  let
    arg_4 =
      (ErlangTuple
         [(ErlangAtom "get_until"), (ErlangAtom "unicode"), prompt_1, (ErlangAtom "io_lib"),
          (ErlangAtom "fread"), (ErlangCons format_2 ErlangEmptyList)])
  in (erlps__bc_req__3 [pid_0, arg_4, (ErlangAtom "true")])
erlps__io_request__2 [pid_0,
                      (ErlangTuple [(ErlangAtom "get_until"), enc_1, prompt_2, m_3, f_4,
                                    a_5])]
  =
  let
    arg_7 =
      (ErlangTuple [(ErlangAtom "get_until"), enc_1, prompt_2, m_3, f_4, a_5])
  in (erlps__bc_req__3 [pid_0, arg_7, (ErlangAtom "true")])
erlps__io_request__2 [pid_0,
                      (ErlangTuple [(ErlangAtom "get_chars"), enc_1, prompt_2, n_3])]
  =
  let arg_5 = (ErlangTuple [(ErlangAtom "get_chars"), enc_1, prompt_2, n_3])
  in (erlps__bc_req__3 [pid_0, arg_5, (ErlangAtom "true")])
erlps__io_request__2 [pid_0,
                      (ErlangTuple [(ErlangAtom "get_line"), enc_1, prompt_2])]
  =
  let arg_4 = (ErlangTuple [(ErlangAtom "get_line"), enc_1, prompt_2])
  in (erlps__bc_req__3 [pid_0, arg_4, (ErlangAtom "true")])
erlps__io_request__2 [pid_0,
                      (ErlangTuple [(ErlangAtom "get_password"), enc_1])]
  =
  let arg_3 = (ErlangTuple [(ErlangAtom "get_password"), enc_1])
  in (erlps__bc_req__3 [pid_0, arg_3, (ErlangAtom "true")])
erlps__io_request__2 [_pid_0, r_1] =
  (ErlangTuple [(ErlangAtom "false"), r_1])
erlps__io_request__2 [arg_4, arg_5] = (EXC.function_clause unit)
erlps__io_request__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__convert_binaries__1 :: ErlangFun
erlps__convert_binaries__1 [bin_0]
  | (ErlangAtom "true") <-
      ((falsifyErrors (\ _ -> (BIF.erlang__is_binary__1 [bin_0])))) =
  (BIF.do_remote_fun_call "Erlang.Unicode" "erlps__characters_to_binary__3"
     [bin_0, (ErlangAtom "latin1"), (ErlangAtom "unicode")])
erlps__convert_binaries__1 [else_0] = else_0
erlps__convert_binaries__1 [arg_1] = (EXC.function_clause unit)
erlps__convert_binaries__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)
