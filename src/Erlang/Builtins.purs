module Erlang.Builtins where

import Erlang.Type
import Erlang.Exception as EXC
import Erlang.Helpers as H
import Erlang.Binary as BIN
import Prelude
import Data.Tuple as DT
import Data.List as DL
import Data.Maybe as DM
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.List as DL
import Data.Int as DI
import Data.UInt as DU
import Data.Int.Bits as DIB
import Data.Map as Map
import Data.String as DSTR
import Data.BigInt as DBI
import Data.String.CodePoints as CodePoints
import Data.Char as Char
import Data.Number.Approximate as DNA
import Data.DateTime.Instant as TINS
import Data.Time.Duration as TDUR
import Effect.Now
import Math
import Control.Monad
import Effect.Exception (throw)
import Effect
import Effect.Unsafe
import Partial.Unsafe

unimplemented :: String -> ErlangTerm
unimplemented name = unsafePerformEffect (throw $ "unimplemented BIF: " <> name)

purs_tco_sucks :: ErlangFun
purs_tco_sucks _ = ErlangAtom "purs_tco_sucks"

--------------------------------------------------------------------------------
-- | Returns the inverse hyperbolic cosine of the argument.
foreign import acosh :: Number -> Number

-- | Returns the inverse hyperbolic sine of the argument.
foreign import asinh :: Number -> Number

-- | Returns the inverse hyperbolic tangent of the argument.
foreign import atanh :: Number -> Number

-- | Returns the cube root of the argument.
foreign import cbrt :: Number -> Number

-- | Returns the number of leading zeroes of a 32-bit integer.
foreign import clz32 :: Int -> Int

-- | Returns the hyperbolic cosine of the argument.
foreign import cosh :: Number -> Number

-- | Returns `exp x - 1` for the argument `x`.
foreign import expm1 :: Number -> Number

-- | Returns the square root of the sum of squares of the arguments.
foreign import hypot :: Number -> Number -> Number

-- | Returns the square root of the sum of squares of the arguments.
foreign import hypot3 :: Number -> Number -> Number -> Number

-- | Returns the natural logarithm of `1 + x` for a number `x`.
foreign import log1p :: Number -> Number

-- | Returns the base 10 logarithm of a number.
foreign import log10 :: Number -> Number

-- | Returns the base 2 logarithm of a number.
foreign import log2 :: Number -> Number

-- | Returns the sign of the argument.
foreign import sign :: Number -> Number

-- | Returns the hyperbolic sine of the argument.
foreign import sinh :: Number -> Number

-- | Returns the hyperbolic tangent of the argument.
foreign import tanh :: Number -> Number

--------------------------------------------------------------------------------
--- Code server OwO
foreign import do_ffi_remote_fun_call :: String -> String -> Array ErlangTerm -> (Unit -> ErlangTerm) -> ErlangTerm
do_remote_fun_call :: String -> String -> Array ErlangTerm -> ErlangTerm
do_remote_fun_call mName fName args =
    do_ffi_remote_fun_call mName fName args (\_ -> EXC.error $ ErlangAtom "undef")

foreign import do_ffi_ensure_loaded :: String -> ErlangTerm -> ErlangTerm -> ErlangTerm
do_ensure_loaded :: String -> ErlangTerm
do_ensure_loaded mName = do_ffi_ensure_loaded mName
                         (ErlangTuple [ErlangAtom "error", ErlangAtom "nofile"])
                         (ErlangTuple [ErlangAtom "module", ErlangAtom mName])

erlang__apply__2 :: ErlangFun
erlang__apply__2 [ft@(ErlangFun arity f), args0] | ErlangTuple args1 <- erlang__list_to_tuple__1 [args0] =
    case (DA.length args1) == arity of
        true ->
            f args1
        false ->
            EXC.error $ ErlangTuple [ErlangAtom "badarity", ft]
erlang__apply__2 [_,_] = EXC.badarg unit
erlang__apply__2 args = EXC.badarity (ErlangFun 2 erlang__apply__2) args

foreign import do_apply_4 :: String -> String -> Array ErlangTerm -> (Unit -> ErlangTerm) -> ErlangTerm
erlang__apply__3 :: ErlangFun
erlang__apply__3 [ErlangAtom m, ErlangAtom f, args0] | ErlangTuple args1 <- erlang__list_to_tuple__1 [args0] =
    do_apply_4 m f args1 (\_ -> EXC.error $ ErlangAtom "undef")
erlang__apply__3 [_,_,_] = EXC.badarg unit
erlang__apply__3 args = EXC.badarity (ErlangFun 3 erlang__apply__3) args

erlang__make_fun__3 :: ErlangFun
erlang__make_fun__3 [m@(ErlangAtom _), f@(ErlangAtom _), ErlangInt barity]
  | DM.Just arity <- H.bigIntToInt barity =
    ErlangFun arity
        (\ args -> erlang__apply__3 [m, f, arrayToErlangList args] )
erlang__make_fun__3 [_,_,_] = EXC.badarg unit
erlang__make_fun__3 args = EXC.badarity (ErlangFun 3 erlang__make_fun__3) args

foreign import do_function_exported_3 :: String -> String -> Int -> Boolean
erlang__function_exported__3 :: ErlangFun
erlang__function_exported__3 [ErlangAtom m, ErlangAtom f, ErlangInt ar] | DM.Just arity <- H.bigIntToInt ar =
    boolToTerm $ do_function_exported_3 m f arity
erlang__function_exported__3 [_,_,_] = EXC.badarg unit
erlang__function_exported__3 args = EXC.badarity (ErlangFun 3 erlang__function_exported__3) args

--------------------------------------------------------------------------------
--- FLOAT BIFS

math__pi__0 :: ErlangFun
math__pi__0 [] = ErlangFloat pi
math__pi__0 args = EXC.badarity (ErlangFun 0 math__pi__0) args

math_arr1 :: Array ErlangTerm -> (Number -> Number) -> ErlangTerm
math_arr1 [ErlangInt num] f = math_arr1 [ErlangFloat $ DBI.toNumber num] f
math_arr1 [ErlangFloat arg] f = ErlangFloat (f arg)
math_arr1 [_] _ = EXC.badarg unit
math_arr1 args f = EXC.badarity (ErlangFun 1 (\a -> math_arr1 a f)) args

math_arr2 :: Array ErlangTerm -> (Number -> Number -> Number) -> ErlangTerm
math_arr2 [ErlangInt num, arg2] f = math_arr2 [ErlangFloat $ DBI.toNumber num, arg2] f
math_arr2 [arg1, ErlangInt num] f = math_arr2 [arg1, ErlangFloat $ DBI.toNumber num] f
math_arr2 [ErlangFloat arg1, ErlangFloat arg2] f = ErlangFloat (f arg1 arg2)
math_arr2 [_] _ = EXC.badarg unit
math_arr2 args f = EXC.badarity (ErlangFun 2 (\a -> math_arr2 a f)) args

math__sin__1 args = math_arr1 args sin
math__cos__1 args = math_arr1 args cos
math__tan__1 args = math_arr1 args tan
math__asin__1 args = math_arr1 args asin
math__acos__1 args = math_arr1 args acos
math__atan__1 args = math_arr1 args atan
math__sinh__1 args = math_arr1 args sinh
math__cosh__1 args = math_arr1 args cosh
math__tanh__1 args = math_arr1 args tanh
math__asinh__1 args = math_arr1 args asinh
math__acosh__1 args = math_arr1 args acosh
math__atanh__1 args = math_arr1 args atanh
math__exp__1 args = math_arr1 args exp
math__log__1 args = math_arr1 args log
math__log2__1 args = math_arr1 args log2
math__log10__1 args = math_arr1 args log10
math__sqrt__1 args = math_arr1 args sqrt
-- FIXME: erf(X) = 2/sqrt(pi)*integral from 0 to X of exp(-t*t) dt.
-- TODO: display middle finger when someone calls this xD
math__erf__1 args = unimplemented "math__erf__1"
-- FIXME: erfc(X) = 1 - erf(X)
math__erfc__1 args = unimplemented "math__erfc__1"
math__ceil__1 args = math_arr1 args ceil
math__floor__1 args = math_arr1 args floor

math__atan2__2 args = math_arr2 args atan2
math__pow__2 args = math_arr2 args pow
math__fmod__2 args = math_arr2 args (%)

erlang__abs__1 :: ErlangFun
erlang__abs__1 [ErlangInt a] = ErlangInt (DBI.abs a)
erlang__abs__1 [ErlangFloat a] = ErlangFloat (abs a)
erlang__abs__1 [_] = EXC.badarg unit
erlang__abs__1 args = EXC.badarity (ErlangFun 1 erlang__abs__1) args

erlang__ceil__1 :: ErlangFun
erlang__ceil__1 args = unimplemented "erlang__ceil__1"
erlang__ceil__1 [_] = EXC.badarg unit
erlang__ceil__1 args = EXC.badarity (ErlangFun 1 erlang__ceil__1) args

erlang__floor__1 :: ErlangFun
erlang__floor__1 args = unimplemented "erlang__floor__1"
erlang__floor__1 [_] = EXC.badarg unit
erlang__floor__1 args = EXC.badarity (ErlangFun 1 erlang__floor__1) args

erlang__float__1 :: ErlangFun
erlang__float__1 [r@(ErlangFloat _)] = r
erlang__float__1 [ErlangInt n] = ErlangFloat $ DBI.toNumber n
erlang__float__1 [_] = EXC.badarg unit
erlang__float__1 args = EXC.badarity (ErlangFun 1 erlang__float__1) args

erlang__float__guard__1 :: ErlangFun
erlang__float__guard__1 [arg] = boolToTerm (H.isEFloat arg)
erlang__float__guard__1 [_] = EXC.badarg unit
erlang__float__guard__1 args = EXC.badarity (ErlangFun 1 erlang__float__1) args

--------------------------------------------------------------------------------
-- LISTS BIFS

lists__keysearch__3 :: ErlangFun
lists__keysearch__3 [key, idx@(ErlangInt bidxNum), l]
  | DM.Just idxNum <- H.bigIntToInt bidxNum, idxNum > 0 =
    let go ErlangEmptyList = ErlangAtom "false"
        go (ErlangCons el rest) = case el of
          ErlangTuple tup ->
            case DA.index tup (idxNum - 1) of
              DM.Just x ->
                case erlang__op_eq [x, key] of
                  ErlangAtom "true" -> (ErlangTuple [ErlangAtom "value", el])
                  _                 -> go rest
              _ -> go rest
          _ -> go rest
        go _ = EXC.badarg unit
    in go l
lists__keysearch__3 [_,_,_] = EXC.badarg unit
lists__keysearch__3 args = EXC.badarity (ErlangFun 3 lists__keysearch__3) args

lists__keymember__3 :: ErlangFun
lists__keymember__3 [key, idx@(ErlangInt bidxNum), l]
  | DM.Just idxNum <- H.bigIntToInt bidxNum, idxNum > 0  =
    let go ErlangEmptyList = ErlangAtom "false"
        go (ErlangCons el rest) = case el of
          ErlangTuple tup ->
            case DA.index tup (idxNum - 1) of
              DM.Just x ->
                case erlang__op_eq [x, key] of
                  ErlangAtom "true" -> ErlangAtom "true"
                  _                 -> go rest
              _ -> go rest
          _ -> go rest
        go _ = EXC.badarg unit
    in go l
lists__keymember__3 [_,_,_] = EXC.badarg unit
lists__keymember__3 args = EXC.badarity (ErlangFun 3 lists__keymember__3) args

lists__reverse__2 :: ErlangFun
lists__reverse__2 [l, acc] = go l acc where
  go ErlangEmptyList acc = acc
  go (ErlangCons h t) acc = go t (ErlangCons h acc)
  go _ _ = EXC.badarg unit
lists__reverse__2 args = EXC.badarity (ErlangFun 2 lists__reverse__2) args

lists__member__2 :: ErlangFun
lists__member__2 [x, l] = go x l where
  go _ ErlangEmptyList = ErlangAtom "false"
  go x (ErlangCons el rest) =
    case erlang__op_exactEq [x, el] of
      ErlangAtom "true" -> ErlangAtom "true"
      _                 -> lists__member__2 [x, rest]
  go _ _ = EXC.badarg unit
lists__member__2 args = EXC.badarity (ErlangFun 2 lists__member__2) args

lists__keyfind__3 :: ErlangFun
lists__keyfind__3 [key, idx@(ErlangInt bidxNum), l]
  | DM.Just idxNum <- H.bigIntToInt bidxNum, idxNum > 0 =
    let go ErlangEmptyList = ErlangAtom "false"
        go (ErlangCons el rest) = case el of
          ErlangTuple tup ->
            case DA.index tup (idxNum - 1) of
              DM.Just x ->
                case erlang__op_eq [x, key] of
                  ErlangAtom "true" -> el
                  _                 -> go rest
              _ -> go rest
          _ -> go rest
        go _ = EXC.badarg unit
    in go l
lists__keyfind__3 [_,_,_] = EXC.badarg unit
lists__keyfind__3 args = EXC.badarity (ErlangFun 3 lists__keyfind__3) args

erlang__hd__1 :: ErlangFun
erlang__hd__1 [ErlangCons h _] = h
erlang__hd__1 [_] = EXC.badarg unit
erlang__hd__1 args = EXC.badarity (ErlangFun 1 erlang__hd__1) args

erlang__tl__1 :: ErlangFun
erlang__tl__1 [ErlangCons _ t] = t
erlang__tl__1 [_] = EXC.badarg unit
erlang__tl__1 args = EXC.badarity (ErlangFun 1 erlang__tl__1) args

erlang__append__2 :: ErlangFun
erlang__append__2 args = erlang__op_append args

erlang__length__1 :: ErlangFun
erlang__length__1 [l] =
  let go ErlangEmptyList acc = ErlangInt (DBI.fromInt acc)
      go (ErlangCons _ t) acc = go t (acc + 1)
      go _ _ = EXC.badarg unit
  in go l 0
erlang__length__1 args = EXC.badarity (ErlangFun 1 erlang__length__1) args

erlang__subtract__2 :: ErlangFun
erlang__subtract__2 = erlang__op_unAppend

--------------------------------------------------------------------------------
--- MAP BIFS

maps__get__2 :: ErlangFun
maps__get__2 [k, ErlangMap m] =
    case Map.lookup k m of
        DM.Just v -> v
        DM.Nothing -> EXC.badkey k
maps__get__2 [_,m] = EXC.badmap m
maps__get__2 args = EXC.badarity (ErlangFun 2 maps__get__2) args

maps__find__2 :: ErlangFun
maps__find__2 [k, ErlangMap m] =
    case Map.lookup k m of
        DM.Just v -> ErlangTuple [ErlangAtom "ok", v]
        DM.Nothing -> ErlangAtom "error"
maps__find__2 [_,m] = EXC.badmap m
maps__find__2 args = EXC.badarity (ErlangFun 2 maps__find__2) args

maps__from_list__1 :: ErlangFun
maps__from_list__1 [t] | DM.Just l <- erlangListToList t =
    ErlangMap (Map.fromFoldable (map (\x ->
        case x of
            ErlangTuple [k,v] -> DT.Tuple k v
            _ -> EXC.badarg unit
        ) l))
maps__from_list__1 [_] = EXC.badarg unit
maps__from_list__1 args = EXC.badarity (ErlangFun 1 maps__from_list__1) args

maps__is_key__2 :: ErlangFun
maps__is_key__2 [k, ErlangMap m] = boolToTerm $ Map.member k m
maps__is_key__2 [_,m] = EXC.badmap m
maps__is_key__2 args = EXC.badarity (ErlangFun 2 maps__is_key__2) args

maps__keys__1 :: ErlangFun
maps__keys__1 [ErlangMap m] = arrayToErlangList $ DA.fromFoldable $ Map.keys m
maps__keys__1 [m] = EXC.badmap m
maps__keys__1 args = EXC.badarity (ErlangFun 1 maps__keys__1) args

maps__merge__2 :: ErlangFun
maps__merge__2 [ErlangMap m1, ErlangMap m2] = ErlangMap $ Map.union m2 m1
maps__merge__2 [ErlangMap _, m] = EXC.badmap m
maps__merge__2 [m, _] = EXC.badmap m
maps__merge__2 args = EXC.badarity (ErlangFun 2 maps__merge__2) args

maps__put__3 :: ErlangFun
maps__put__3 [k, v, ErlangMap m] = ErlangMap $ Map.insert k v m
maps__put__3 [_,_,m] = EXC.badmap m
maps__put__3 args = EXC.badarity (ErlangFun 3 maps__put__3) args

maps__remove__2 :: ErlangFun
maps__remove__2 [k, ErlangMap m] = ErlangMap $ Map.delete k m
maps__remove__2 [_,m] = EXC.badmap m
maps__remove__2 args = EXC.badarity (ErlangFun 2 maps__remove__2) args

maps__take__2 :: ErlangFun
maps__take__2 [k, ErlangMap m1] =
    case Map.pop k m1 of
        DM.Just (DT.Tuple v m2) -> ErlangTuple [v, ErlangMap m2]
        DM.Nothing -> ErlangAtom "error"
maps__take__2 [_,m] = EXC.badmap m
maps__take__2 args = EXC.badarity (ErlangFun 2 maps__take__2) args

maps__to_list__1 :: ErlangFun
maps__to_list__1 [ErlangMap m] = arrayToErlangList $ map (\(DT.Tuple k v) -> ErlangTuple [k, v]) $ Map.toUnfoldable m
maps__to_list__1 [m] = EXC.badmap m
maps__to_list__1 args = EXC.badarity (ErlangFun 1 maps__to_list__1) args

maps__update__3 :: ErlangFun
maps__update__3 [k, v, ErlangMap m] =
    case Map.lookup k m of
        DM.Just v -> ErlangMap $ Map.insert k v m
        DM.Nothing -> EXC.badkey k
maps__update__3 [_,_,m] = EXC.badmap m
maps__update__3 args = EXC.badarity (ErlangFun 3 maps__update__3) args

maps__values__1 :: ErlangFun
maps__values__1 [ErlangMap m] = arrayToErlangList $ DA.fromFoldable $ Map.values m
maps__values__1 [m] = EXC.badmap m
maps__values__1 args = EXC.badarity (ErlangFun 1 maps__values__1) args

erts_internal__map_next__3 :: ErlangFun
erts_internal__map_next__3 [ErlangInt bi, ErlangMap m, ErlangAtom "iterator"]
  | DM.Just 0 <- H.bigIntToInt bi =
    case Map.findMin m of
        DM.Nothing -> ErlangAtom "none"
        DM.Just ({key: k, value: v}) -> ErlangTuple [k, v, ErlangCons (ErlangInt (DBI.fromInt 0)) (ErlangMap $ Map.delete k m)]
erts_internal__map_next__3 [_, m, _] = EXC.badmap m
erts_internal__map_next__3 args = EXC.badarity (ErlangFun 3 erts_internal__map_next__3) args

erlang__map_size__1 :: ErlangFun
erlang__map_size__1 [ErlangMap m] = ErlangInt $ DBI.fromInt $ Map.size m
erlang__map_size__1 [m] = EXC.badmap m
erlang__map_size__1 args = EXC.badarity (ErlangFun 1 erlang__map_size__1) args

erlang__is_map_key__2 :: ErlangFun
erlang__is_map_key__2 args = maps__is_key__2 args

erlang__map_get__2 :: ErlangFun
erlang__map_get__2 args = maps__get__2 args

--------------------------------------------------------------------------------
--- BINARY AND UNARY OPERATIONS

-- =/=
erlang__op_exactNeq :: ErlangFun
erlang__op_exactNeq [a, b] = boolToTerm (not $ eqErlangTerm strongNumEq a b)
erlang__op_exactNeq [_, _] = EXC.badarg unit
erlang__op_exactNeq args = EXC.badarity (ErlangFun 2 erlang__op_exactNeq) args

-- =:=
erlang__op_exactEq :: ErlangFun
erlang__op_exactEq [a, b] = boolToTerm (eqErlangTerm strongNumEq a b)
erlang__op_exactEq [_, _] = EXC.badarg unit
erlang__op_exactEq args = EXC.badarity (ErlangFun 2 erlang__op_exactEq) args

-- /=
erlang__op_neq :: ErlangFun
erlang__op_neq [a, b] = boolToTerm (not $ eqErlangTerm weakNumEq a b)
erlang__op_neq [_, _] = EXC.badarg unit
erlang__op_neq args = EXC.badarity (ErlangFun 2 erlang__op_neq) args

-- ==
erlang__op_eq :: ErlangFun
erlang__op_eq [a, b] = boolToTerm (eqErlangTerm weakNumEq a b)
erlang__op_eq [_, _] = EXC.badarg unit
erlang__op_eq args = EXC.badarity (ErlangFun 2 erlang__op_eq) args

-- and
erlang__op_and :: ErlangFun
erlang__op_and [ErlangAtom "true",  ErlangAtom "true"]  = boolToTerm true
erlang__op_and [ErlangAtom "false", ErlangAtom "true"]  = boolToTerm false
erlang__op_and [ErlangAtom "true",  ErlangAtom "false"] = boolToTerm false
erlang__op_and [ErlangAtom "false", ErlangAtom "false"] = boolToTerm false
erlang__op_and [_, _] = EXC.badarg unit
erlang__op_and args = EXC.badarity (ErlangFun 2 erlang__op_and) args

-- or
erlang__op_or :: ErlangFun
erlang__op_or [ErlangAtom "true",  ErlangAtom "true"]  = boolToTerm true
erlang__op_or [ErlangAtom "false", ErlangAtom "true"]  = boolToTerm true
erlang__op_or [ErlangAtom "true",  ErlangAtom "false"] = boolToTerm true
erlang__op_or [ErlangAtom "false", ErlangAtom "false"] = boolToTerm false
erlang__op_or [_, _] = EXC.badarg unit
erlang__op_or args = EXC.badarity (ErlangFun 2 erlang__op_or) args

-- /
erlang__op_div :: ErlangFun
erlang__op_div [ErlangInt a, ErlangInt b] = ErlangInt (a / b)
erlang__op_div [ErlangInt a, ErlangFloat b] = ErlangFloat ((DBI.toNumber a) / b)
erlang__op_div [ErlangFloat a, ErlangInt b] = ErlangFloat (a / (DBI.toNumber b))
erlang__op_div [ErlangFloat a, ErlangFloat b] = ErlangFloat (a / b)
erlang__op_div [_, _] = EXC.badarg unit
erlang__op_div args = EXC.badarity (ErlangFun 2 erlang__op_div) args

-- 'div'
erlang__op_div_strict :: ErlangFun
erlang__op_div_strict [ErlangInt a, ErlangInt b] = ErlangInt (a / b)
erlang__op_div_strict [_, _] = EXC.badarg unit
erlang__op_div_strict args = EXC.badarity (ErlangFun 2 erlang__op_div_strict) args

-- 'rem'
erlang__op_rem_strict :: ErlangFun
erlang__op_rem_strict [ErlangInt left, ErlangInt right] = ErlangInt (mod left right)
erlang__op_rem_strict [_,_] = EXC.badarg unit
erlang__op_rem_strict args = EXC.badarity (ErlangFun 2 erlang__op_rem_strict) args

-- *
erlang__op_mult :: ErlangFun
erlang__op_mult [ErlangInt a, ErlangInt b] = ErlangInt (a * b)
erlang__op_mult [ErlangInt a, ErlangFloat b] = ErlangFloat ((DBI.toNumber a) * b)
erlang__op_mult [ErlangFloat a, ErlangInt b] = ErlangFloat (a * (DBI.toNumber b))
erlang__op_mult [ErlangFloat a, ErlangFloat b] = ErlangFloat (a * b)
erlang__op_mult [_, _] = EXC.badarg unit
erlang__op_mult args = EXC.badarity (ErlangFun 2 erlang__op_mult) args

-- -
erlang__op_minus :: ErlangFun
erlang__op_minus [ErlangInt a, ErlangInt b] = ErlangInt (a - b)
erlang__op_minus [ErlangInt a, ErlangFloat b] = ErlangFloat ((DBI.toNumber a) - b)
erlang__op_minus [ErlangFloat a, ErlangInt b] = ErlangFloat (a - (DBI.toNumber b))
erlang__op_minus [ErlangFloat a, ErlangFloat b] = ErlangFloat (a - b)
erlang__op_minus [_, _] = EXC.badarg unit
erlang__op_minus args = EXC.badarity (ErlangFun 2 erlang__op_minus) args

-- +
erlang__op_plus :: ErlangFun
erlang__op_plus [ErlangInt a, ErlangInt b] = ErlangInt (a + b)
erlang__op_plus [ErlangInt a, ErlangFloat b] = ErlangFloat ((DBI.toNumber a) + b)
erlang__op_plus [ErlangFloat a, ErlangInt b] = ErlangFloat (a + (DBI.toNumber b))
erlang__op_plus [ErlangFloat a, ErlangFloat b] = ErlangFloat (a + b)
erlang__op_plus [_, _] = EXC.badarg unit
erlang__op_plus args = EXC.badarity (ErlangFun 2 erlang__op_plus) args

-- >=
erlang__op_greaterEq :: ErlangFun
erlang__op_greaterEq [a, b] = case compareErlangTerm weakNumCmp a b of
  LT -> ErlangAtom "false"
  _  -> ErlangAtom "true"
erlang__op_greaterEq [_, _] = EXC.badarg unit
erlang__op_greaterEq args = EXC.badarity (ErlangFun 2 erlang__op_greaterEq) args

-- >
erlang__op_greater :: ErlangFun
erlang__op_greater [a, b] = case compareErlangTerm weakNumCmp a b of
  GT -> ErlangAtom "true"
  _  -> ErlangAtom "false"
erlang__op_greater [_, _] = EXC.badarg unit
erlang__op_greater args = EXC.badarity (ErlangFun 2 erlang__op_greater) args

-- =<
erlang__op_lesserEq :: ErlangFun
erlang__op_lesserEq [a, b] = case compareErlangTerm weakNumCmp a b of
  GT -> ErlangAtom "false"
  _  -> ErlangAtom "true"
erlang__op_lesserEq [_, _] = EXC.badarg unit
erlang__op_lesserEq args = EXC.badarity (ErlangFun 2 erlang__op_lesserEq) args

-- <
erlang__op_lesser :: ErlangFun
erlang__op_lesser [a, b] = case compareErlangTerm weakNumCmp a b of
  LT -> ErlangAtom "true"
  _  -> ErlangAtom "false"
erlang__op_lesser [_, _] = EXC.badarg unit
erlang__op_lesser args = EXC.badarity (ErlangFun 2 erlang__op_lesser) args

-- --
erlang__op_unAppend :: ErlangFun
erlang__op_unAppend [l, ErlangEmptyList] = -- This needs to throw an exception for inproper lists .-.
    let
        a = lists__reverse__2 [l, ErlangEmptyList]
    in
        l
erlang__op_unAppend [l, r] = do_unappend l r ErlangEmptyList
erlang__op_unAppend [_, _] = EXC.badarg unit
erlang__op_unAppend args = EXC.badarity (ErlangFun 2 erlang__op_unAppend) args

do_unappend ErlangEmptyList ErlangEmptyList ErlangEmptyList = ErlangEmptyList
do_unappend r@(ErlangCons _ _) ErlangEmptyList ErlangEmptyList = r
do_unappend ErlangEmptyList ErlangEmptyList acc = lists__reverse__2 [acc, ErlangEmptyList]
do_unappend (ErlangCons h t) ErlangEmptyList acc = do_unappend t ErlangEmptyList (ErlangCons h acc)
do_unappend ErlangEmptyList (ErlangCons _ term) acc = do_unappend (lists__reverse__2 [acc, ErlangEmptyList]) term ErlangEmptyList
do_unappend (ErlangCons hl tl) r@(ErlangCons hr tr) acc =
      case erlang__op_exactEq [hl, hr] of
        ErlangAtom "true"  -> do_unappend (lists__reverse__2 [acc, tl]) tr ErlangEmptyList
        _                  -> do_unappend tl r (ErlangCons hl acc)
do_unappend _ _ _ = EXC.badarg unit

-- ++
erlang__op_append :: ErlangFun
erlang__op_append [l, r] = lists__reverse__2 [lists__reverse__2 [l, ErlangEmptyList], r] -- TODO: Optimize in FFI
erlang__op_append [_, _] = EXC.badarg unit
erlang__op_append args = EXC.badarity (ErlangFun 2 erlang__op_append) args

isCons (ErlangCons _ _) = true
isCons _ = false
isEmpty ErlangEmptyList = true
isEmpty _ = false

-- -
erlang__op_neg :: ErlangFun
erlang__op_neg [ErlangInt n] = ErlangInt (-n)
erlang__op_neg [ErlangFloat n] = ErlangFloat (-n)
erlang__op_neg [_] = EXC.badarg unit
erlang__op_neg args = EXC.badarity (ErlangFun 1 erlang__op_neg) args

-- +
erlang__op_unary_plus :: ErlangFun
erlang__op_unary_plus [r@(ErlangInt _)] = r
erlang__op_unary_plus [r@(ErlangFloat _)] = r
erlang__op_unary_plus args = EXC.badarity (ErlangFun 1 erlang__op_unary_plus) args

-- not
erlang__op_not :: ErlangFun
erlang__op_not [ErlangAtom "false"] = ErlangAtom "true"
erlang__op_not [ErlangAtom "true"] = ErlangAtom "false"
erlang__op_not [_] = EXC.badarg unit
erlang__op_not args = EXC.badarity (ErlangFun 1 erlang__op_not) args

erlang__not__1 :: ErlangFun
erlang__not__1 args = erlang__op_not args

erlang__bor__2 :: ErlangFun
erlang__bor__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DBI.or a b
erlang__bor__2 [_,_] = EXC.badarg unit
erlang__bor__2 args = EXC.badarity (ErlangFun 2 erlang__bor__2) args

erlang__bnot__1 :: ErlangFun
erlang__bnot__1 [ErlangInt a] = ErlangInt $ DBI.not a
erlang__bnot__1 [_] = EXC.badarg unit
erlang__bnot__1 args = EXC.badarity (ErlangFun 1 erlang__bnot__1) args

erlang__bxor__2 :: ErlangFun
erlang__bxor__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DBI.xor a b
erlang__bxor__2 [_,_] = EXC.badarg unit
erlang__bxor__2 args = EXC.badarity (ErlangFun 2 erlang__bxor__2) args

erlang__or__2 :: ErlangFun
erlang__or__2 args = unimplemented "erlang__or__2"
erlang__or__2 [_,_] = EXC.badarg unit
erlang__or__2 args = EXC.badarity (ErlangFun 2 erlang__or__2) args

erlang__bsr__2 :: ErlangFun
erlang__bsr__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DBI.shr a (DBI.toNumber b)
erlang__bsr__2 [_,_] = EXC.badarg unit
erlang__bsr__2 args = EXC.badarity (ErlangFun 2 erlang__bsr__2) args

erlang__xor__2 :: ErlangFun
erlang__xor__2 args = unimplemented "erlang__xor__2"
erlang__xor__2 [_,_] = EXC.badarg unit
erlang__xor__2 args = EXC.badarity (ErlangFun 2 erlang__xor__2) args

erlang__bsl__2 :: ErlangFun
erlang__bsl__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DBI.shl a (DBI.toNumber b)
erlang__bsl__2 [_,_] = EXC.badarg unit
erlang__bsl__2 args = EXC.badarity (ErlangFun 2 erlang__bsl__2) args

erlang__band__2 :: ErlangFun
erlang__band__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DBI.and a b
erlang__band__2 [_,_] = EXC.badarg unit
erlang__band__2 args = EXC.badarity (ErlangFun 2 erlang__band__2) args

erlang__min__2 :: ErlangFun
erlang__min__2 [t1, t2] | t1 <= t2  = t1
                        | otherwise = t2
erlang__min__2 [_,_] = EXC.badarg unit
erlang__min__2 args = EXC.badarity (ErlangFun 2 erlang__min__2) args


erlang__max__2 :: ErlangFun
erlang__max__2 [t1, t2] | t1 >= t2  = t1
                        | otherwise = t2
erlang__max__2 [_,_] = EXC.badarg unit
erlang__max__2 args = EXC.badarity (ErlangFun 2 erlang__max__2) args

--------------------------------------------------------------------------------
--- Type tests

erlang__is_integer__1 :: ErlangFun
erlang__is_integer__1 [ErlangInt _] = ErlangAtom "true"
erlang__is_integer__1 [_] = ErlangAtom "false"
erlang__is_integer__1 args = EXC.badarity (ErlangFun 1 erlang__is_integer__1) args

erlang__is_float__1 :: ErlangFun
erlang__is_float__1 [ErlangFloat _] = ErlangAtom "true"
erlang__is_float__1 [_] = ErlangAtom "false"
erlang__is_float__1 args = EXC.badarity (ErlangFun 1 erlang__is_float__1) args

erlang__is_binary__1 :: ErlangFun
erlang__is_binary__1 [ErlangBinary _] = ErlangAtom "true" --FIXME: should return false for bitstrings
erlang__is_binary__1 [_] = ErlangAtom "false"
erlang__is_binary__1 args = EXC.badarity (ErlangFun 1 erlang__is_binary__1) args

erlang__is_bitstring__1 :: ErlangFun
erlang__is_bitstring__1 [ErlangBinary _] = ErlangAtom "true"
erlang__is_bitstring__1 [_] = ErlangAtom "false"
erlang__is_bitstring__1 args = EXC.badarity (ErlangFun 1 erlang__is_bitstring__1) args

erlang__is_port__1 :: ErlangFun
erlang__is_port__1 args = unimplemented "erlang__is_port__1"
erlang__is_port__1 [_] = EXC.badarg unit
erlang__is_port__1 args = EXC.badarity (ErlangFun 1 erlang__is_port__1) args

erlang__is_boolean__1 :: ErlangFun
erlang__is_boolean__1 [ErlangAtom "true"] = ErlangAtom "true"
erlang__is_boolean__1 [ErlangAtom "false"] = ErlangAtom "true"
erlang__is_boolean__1 [_] = boolToTerm false
erlang__is_boolean__1 [_] = EXC.badarg unit
erlang__is_boolean__1 args = EXC.badarity (ErlangFun 1 erlang__is_boolean__1) args

erlang__is_record__2 :: ErlangFun
erlang__is_record__2 [term, ErlangAtom tag] = case term of
  ErlangTuple arr | DM.Just (ErlangAtom termTag) <- DA.head arr -> boolToTerm $ tag == termTag
  _ -> ErlangAtom "false"
erlang__is_record__2 [_,_] = EXC.badarg unit
erlang__is_record__2 args = EXC.badarity (ErlangFun 2 erlang__is_record__2) args

erlang__is_tuple__1 :: ErlangFun
erlang__is_tuple__1 [ErlangTuple _] = ErlangAtom "true"
erlang__is_tuple__1 [_] = ErlangAtom "false"
erlang__is_tuple__1 args = EXC.badarity (ErlangFun 1 erlang__is_tuple__1) args

erlang__is_atom__1 :: ErlangFun
erlang__is_atom__1 [ErlangAtom _] = ErlangAtom "true"
erlang__is_atom__1 [_] = ErlangAtom "false"
erlang__is_atom__1 args = EXC.badarity (ErlangFun 1 erlang__is_atom__1) args

erlang__is_number__1 :: ErlangFun
erlang__is_number__1 [ErlangInt _] = ErlangAtom "true"
erlang__is_number__1 [ErlangFloat _] = ErlangAtom "true"
erlang__is_number__1 [_] = ErlangAtom "false"
erlang__is_number__1 args = EXC.badarity (ErlangFun 1 erlang__is_number__1) args

erlang__is_pid__1 :: ErlangFun
erlang__is_pid__1 [ErlangPID _] = boolToTerm true
erlang__is_pid__1 [_] = boolToTerm false
erlang__is_pid__1 args = EXC.badarity (ErlangFun 1 erlang__is_pid__1) args

erlang__is_function__1 :: ErlangFun
erlang__is_function__1 [ErlangFun _ _] = ErlangAtom "true"
erlang__is_function__1 [_] = ErlangAtom "false"
erlang__is_function__1 args = EXC.badarity (ErlangFun 1 erlang__is_function__1) args

erlang__is_reference__1 :: ErlangFun
erlang__is_reference__1 [ErlangReference _] = ErlangAtom "true"
erlang__is_reference__1 [_] = ErlangAtom "false"
erlang__is_reference__1 args = EXC.badarity (ErlangFun 1 erlang__is_reference__1) args

erlang__is_list__1 :: ErlangFun
erlang__is_list__1 [ErlangEmptyList] = boolToTerm true
erlang__is_list__1 [ErlangCons _ _] = boolToTerm true
erlang__is_list__1 [_] = boolToTerm false
erlang__is_list__1 args = EXC.badarity (ErlangFun 1 erlang__is_list__1) args

erlang__is_map__1 :: ErlangFun
erlang__is_map__1 [ErlangMap _] = boolToTerm true
erlang__is_map__1 [_] = boolToTerm false
erlang__is_map__1 args = EXC.badarity (ErlangFun 1 erlang__is_map__1) args

erlang__is_function__2 :: ErlangFun
erlang__is_function__2 [ErlangFun a _, ErlangInt b] | DBI.fromInt a == b = ErlangAtom "true"
erlang__is_function__2 [_, _] = ErlangAtom "false"
erlang__is_function__2 args = EXC.badarity (ErlangFun 2 erlang__is_function__2) args

erlang__is_record__3 :: ErlangFun
erlang__is_record__3 [term, ErlangAtom tag, ErlangInt bsize]
  | DM.Just size <- H.bigIntToInt bsize = case term of
    ErlangTuple arr | DM.Just (ErlangAtom termTag) <- DA.head arr, size == DA.length arr -> boolToTerm $ tag == termTag
    _ -> ErlangAtom "false"
erlang__is_record__3 [_,_,_] = EXC.badarg unit
erlang__is_record__3 args = EXC.badarity (ErlangFun 3 erlang__is_record__3) args

--------------------------------------------------------------------------------
--- Type Casts

erlang__integer_to_binary__2 :: ErlangFun
erlang__integer_to_binary__2 args = unimplemented "erlang__integer_to_binary__2"
erlang__integer_to_binary__2 [_,_] = EXC.badarg unit
erlang__integer_to_binary__2 args = EXC.badarity (ErlangFun 2 erlang__integer_to_binary__2) args

erlang__integer_to_list__2 :: ErlangFun
erlang__integer_to_list__2 [ErlangInt num, ErlangInt bbase]
    | DM.Just base <- H.bigIntToInt bbase
    = H.make_string $ DBI.toBase base num
erlang__integer_to_list__2 [_,_] = EXC.badarg unit
erlang__integer_to_list__2 args = EXC.badarity (ErlangFun 2 erlang__integer_to_list__2) args

erlang__list_to_float__1 :: ErlangFun
erlang__list_to_float__1 args = unimplemented "erlang__list_to_float__1"
erlang__list_to_float__1 [_] = EXC.badarg unit
erlang__list_to_float__1 args = EXC.badarity (ErlangFun 1 erlang__list_to_float__1) args

erlang__list_to_integer__2 :: ErlangFun
erlang__list_to_integer__2 [t, ErlangInt bbase]
  | DM.Just base <- H.bigIntToInt bbase
  , DM.Just i <- H.erlangListToString t >>= DBI.fromBase base = ErlangInt i
erlang__list_to_integer__2 [_,_] = EXC.badarg unit
erlang__list_to_integer__2 args = EXC.badarity (ErlangFun 2 erlang__list_to_integer__2) args

erlang__binary_to_atom__2 :: ErlangFun
erlang__binary_to_atom__2 args = unimplemented "erlang__binary_to_atom__2"
erlang__binary_to_atom__2 [_,_] = EXC.badarg unit
erlang__binary_to_atom__2 args = EXC.badarity (ErlangFun 2 erlang__binary_to_atom__2) args

erlang__binary_to_term__2 :: ErlangFun
erlang__binary_to_term__2 args = unimplemented "erlang__binary_to_term__2"
erlang__binary_to_term__2 [_,_] = EXC.badarg unit
erlang__binary_to_term__2 args = EXC.badarity (ErlangFun 2 erlang__binary_to_term__2) args

erlang__integer_to_binary__1 :: ErlangFun
erlang__integer_to_binary__1 args = unimplemented "erlang__integer_to_binary__1"
erlang__integer_to_binary__1 [_] = EXC.badarg unit
erlang__integer_to_binary__1 args = EXC.badarity (ErlangFun 1 erlang__integer_to_binary__1) args

erlang__iolist_to_iovec__1 :: ErlangFun
erlang__iolist_to_iovec__1 args = ErlangCons (erlang__iolist_to_binary__1 args) ErlangEmptyList
erlang__iolist_to_iovec__1 [_] = EXC.badarg unit
erlang__iolist_to_iovec__1 args = EXC.badarity (ErlangFun 1 erlang__iolist_to_iovec__1) args

erlang__iolist_to_binary__1 :: ErlangFun
erlang__iolist_to_binary__1 [b@(ErlangBinary _)] = b
erlang__iolist_to_binary__1 [el]
  | DM.Just l <- H.erlangListToFlatList el = ErlangBinary
    $ BIN.fromFoldable
    $ map (\x -> case x of ErlangInt bi | DM.Just i <- H.bigIntToInt bi -> i
                           _ -> EXC.badarg unit
          )
    $ l
erlang__iolist_to_binary__1 [_] = EXC.badarg unit
erlang__iolist_to_binary__1 args = EXC.badarity (ErlangFun 1 erlang__iolist_to_binary__1) args

-- TODO: Optimize me :P
erlang__iolist_size__1 :: ErlangFun
erlang__iolist_size__1 args = erlang__byte_size__1 [erlang__iolist_to_binary__1 args]

erlang__list_to_binary__1 :: ErlangFun
erlang__list_to_binary__1 [ErlangBinary _] = EXC.badarg unit
erlang__list_to_binary__1 [el]
  | DM.Just l <- H.erlangListToFlatList el = ErlangBinary
    $ BIN.fromFoldable
    $ map (\x -> case x of ErlangInt bi | DM.Just i <- H.bigIntToInt bi -> i
                           _ -> EXC.badarg unit
          )
    $ l
erlang__list_to_binary__1 [_] = EXC.badarg unit
erlang__list_to_binary__1 args = EXC.badarity (ErlangFun 1 erlang__list_to_binary__1) args

erlang__list_to_bitstring__1 :: ErlangFun
erlang__list_to_bitstring__1 args = unimplemented "erlang__list_to_bitstring__1"
erlang__list_to_bitstring__1 [_] = EXC.badarg unit
erlang__list_to_bitstring__1 args = EXC.badarity (ErlangFun 1 erlang__list_to_bitstring__1) args

erlang__pid_to_list__1 :: ErlangFun
erlang__pid_to_list__1 args = unimplemented "erlang__pid_to_list__1"
erlang__pid_to_list__1 [_] = EXC.badarg unit
erlang__pid_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__pid_to_list__1) args

erlang__binary_to_integer__2 :: ErlangFun
erlang__binary_to_integer__2 args = unimplemented "erlang__binary_to_integer__2"
erlang__binary_to_integer__2 [_,_] = EXC.badarg unit
erlang__binary_to_integer__2 args = EXC.badarity (ErlangFun 2 erlang__binary_to_integer__2) args

erlang__binary_to_existing_atom__2 :: ErlangFun
erlang__binary_to_existing_atom__2 args = unimplemented "erlang__binary_to_existing_atom__2"
erlang__binary_to_existing_atom__2 [_,_] = EXC.badarg unit
erlang__binary_to_existing_atom__2 args = EXC.badarity (ErlangFun 2 erlang__binary_to_existing_atom__2) args

erlang__port_to_list__1 :: ErlangFun
erlang__port_to_list__1 args = unimplemented "erlang__port_to_list__1"
erlang__port_to_list__1 [_] = EXC.badarg unit
erlang__port_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__port_to_list__1) args

erlang__float_to_list__1 :: ErlangFun
erlang__float_to_list__1 args = unimplemented "erlang__float_to_list__1"
erlang__float_to_list__1 [_] = EXC.badarg unit
erlang__float_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__float_to_list__1) args

erlang__float_to_list__2 :: ErlangFun
erlang__float_to_list__2 args = unimplemented "erlang__float_to_list__2"
erlang__float_to_list__2 [_,_] = EXC.badarg unit
erlang__float_to_list__2 args = EXC.badarity (ErlangFun 2 erlang__float_to_list__2) args

erlang__binary_to_list__3 :: ErlangFun
erlang__binary_to_list__3 [ErlangBinary bin, ErlangInt dl, ErlangInt dr]
  | DM.Just l <- H.bigIntToInt dl, DM.Just r <- H.bigIntToInt dr
  = BIN.to_erlang_list_from_to bin l r
erlang__binary_to_list__3 [_,_,_] = EXC.badarg unit
erlang__binary_to_list__3 args = EXC.badarity (ErlangFun 3 erlang__binary_to_list__3) args

erlang__list_to_atom__1 :: ErlangFun
erlang__list_to_atom__1 [el] =
  case erlangListToList el of
    DM.Nothing -> EXC.badarg unit
    DM.Just l -> ErlangAtom
      $ CodePoints.fromCodePointArray
      $ DA.fromFoldable
      $ map (\term -> case term of
                ErlangInt bi
                  | DM.Just i <- H.bigIntToInt bi
                  , DM.Just c <- Char.fromCharCode i ->
                  CodePoints.codePointFromChar c
                _ -> EXC.badarg unit
            )
      $ l
erlang__list_to_atom__1 args = EXC.badarity (ErlangFun 1 erlang__list_to_atom__1) args

erlang__list_to_existing_atom__1 :: ErlangFun
erlang__list_to_existing_atom__1 args = erlang__list_to_atom__1 args

erlang__ref_to_list__1 :: ErlangFun
erlang__ref_to_list__1 args = unimplemented "erlang__ref_to_list__1"
erlang__ref_to_list__1 [_] = EXC.badarg unit
erlang__ref_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__ref_to_list__1) args

erlang__binary_to_list__1 :: ErlangFun
erlang__binary_to_list__1 [ErlangBinary bin] = BIN.to_erlang_list bin
erlang__binary_to_list__1 [_] = EXC.badarg unit
erlang__binary_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__binary_to_list__1) args

erlang__term_to_binary__1 :: ErlangFun
erlang__term_to_binary__1 args = unimplemented "erlang__term_to_binary__1"
erlang__term_to_binary__1 [_] = EXC.badarg unit
erlang__term_to_binary__1 args = EXC.badarity (ErlangFun 1 erlang__term_to_binary__1) args

erlang__term_to_binary__2 :: ErlangFun
erlang__term_to_binary__2 args = unimplemented "erlang__term_to_binary__2"
erlang__term_to_binary__2 [_,_] = EXC.badarg unit
erlang__term_to_binary__2 args = EXC.badarity (ErlangFun 2 erlang__term_to_binary__2) args

erlang__tuple_to_list__1 :: ErlangFun
erlang__tuple_to_list__1 [ErlangTuple t] = arrayToErlangList t
erlang__tuple_to_list__1 [_] = EXC.badarg unit
erlang__tuple_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__tuple_to_list__1) args

erlang__binary_to_term__1 :: ErlangFun
erlang__binary_to_term__1 args = unimplemented "erlang__binary_to_term__1"
erlang__binary_to_term__1 [_] = EXC.badarg unit
erlang__binary_to_term__1 args = EXC.badarity (ErlangFun 1 erlang__binary_to_term__1) args

erlang__list_to_integer__1 :: ErlangFun
erlang__list_to_integer__1 [t]
  | DM.Just i <- H.erlangListToString t >>= DBI.fromString = ErlangInt i
erlang__list_to_integer__1 [_] = EXC.badarg unit
erlang__list_to_integer__1 args = EXC.badarity (ErlangFun 1 erlang__list_to_integer__1) args

erlang__list_to_port__1 :: ErlangFun
erlang__list_to_port__1 args = unimplemented "erlang__list_to_port__1"
erlang__list_to_port__1 [_] = EXC.badarg unit
erlang__list_to_port__1 args = EXC.badarity (ErlangFun 1 erlang__list_to_port__1) args

erlang__binary_to_float__1 :: ErlangFun
erlang__binary_to_float__1 args = unimplemented "erlang__binary_to_float__1"
erlang__binary_to_float__1 [_] = EXC.badarg unit
erlang__binary_to_float__1 args = EXC.badarity (ErlangFun 1 erlang__binary_to_float__1) args

erlang__list_to_ref__1 :: ErlangFun
erlang__list_to_ref__1 args = unimplemented "erlang__list_to_ref__1"
erlang__list_to_ref__1 [_] = EXC.badarg unit
erlang__list_to_ref__1 args = EXC.badarity (ErlangFun 1 erlang__list_to_ref__1) args

erlang__float_to_binary__1 :: ErlangFun
erlang__float_to_binary__1 args = unimplemented "erlang__float_to_binary__1"
erlang__float_to_binary__1 [_] = EXC.badarg unit
erlang__float_to_binary__1 args = EXC.badarity (ErlangFun 1 erlang__float_to_binary__1) args

erlang__atom_to_binary__2 :: ErlangFun
erlang__atom_to_binary__2 args = unimplemented "erlang__atom_to_binary__2"
erlang__atom_to_binary__2 [_,_] = EXC.badarg unit
erlang__atom_to_binary__2 args = EXC.badarity (ErlangFun 2 erlang__atom_to_binary__2) args

erlang__binary_to_integer__1 :: ErlangFun
erlang__binary_to_integer__1 args = unimplemented "erlang__binary_to_integer__1"
erlang__binary_to_integer__1 [_] = EXC.badarg unit
erlang__binary_to_integer__1 args = EXC.badarity (ErlangFun 1 erlang__binary_to_integer__1) args

erlang__atom_to_list__1 :: ErlangFun
erlang__atom_to_list__1 [ErlangAtom atom] = H.make_string atom
erlang__atom_to_list__1 [_] = EXC.badarg unit
erlang__atom_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__atom_to_list__1) args

erlang__integer_to_list__1 :: ErlangFun
erlang__integer_to_list__1 [ErlangInt num] = H.make_string $ DBI.toString num
erlang__integer_to_list__1 [_] = EXC.badarg unit
erlang__integer_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__integer_to_list__1) args

erlang__fun_to_list__1 :: ErlangFun
erlang__fun_to_list__1 args = unimplemented "erlang__fun_to_list__1"
erlang__fun_to_list__1 [_] = EXC.badarg unit
erlang__fun_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__fun_to_list__1) args

erlang__list_to_pid__1 :: ErlangFun
erlang__list_to_pid__1 args = unimplemented "erlang__list_to_pid__1"
erlang__list_to_pid__1 [_] = EXC.badarg unit
erlang__list_to_pid__1 args = EXC.badarity (ErlangFun 1 erlang__list_to_pid__1) args

erlang__float_to_binary__2 :: ErlangFun
erlang__float_to_binary__2 args = unimplemented "erlang__float_to_binary__2"
erlang__float_to_binary__2 [_,_] = EXC.badarg unit
erlang__float_to_binary__2 args = EXC.badarity (ErlangFun 2 erlang__float_to_binary__2) args

erlang__list_to_tuple__1 :: ErlangFun
erlang__list_to_tuple__1 [list] | DM.Just r <- erlangListToList list =
    ErlangTuple (DA.fromFoldable r)
erlang__list_to_tuple__1 [_] = EXC.badarg unit
erlang__list_to_tuple__1 args = EXC.badarity (ErlangFun 1 ErlangTuple) args

erlang__bitstring_to_list__1 :: ErlangFun
erlang__bitstring_to_list__1 [ErlangBinary bin] = BIN.to_erlang_list bin
erlang__bitstring_to_list__1 [_] = EXC.badarg unit
erlang__bitstring_to_list__1 args = EXC.badarity (ErlangFun 1 erlang__bitstring_to_list__1) args

--------------------------------------------------------------------------------
--- Process dictionary

foreign import do_put_2 :: (ErlangTerm -> ErlangTerm -> Boolean) -> ErlangTerm -> ErlangTerm -> ErlangTerm -> ErlangTerm
erlang__put__2 :: ErlangFun
erlang__put__2 [key, value] = do_put_2 (==) key value (ErlangAtom "undefined")
erlang__put__2 [_,_] = EXC.badarg unit
erlang__put__2 args = EXC.badarity (ErlangFun 2 erlang__put__2) args

foreign import do_get_1 :: (ErlangTerm -> ErlangTerm -> Boolean) -> ErlangTerm -> ErlangTerm -> ErlangTerm
erlang__get__1 :: ErlangFun
erlang__get__1 [key] = do_get_1 (==) key (ErlangAtom "undefined")
erlang__get__1 [_] = EXC.badarg unit
erlang__get__1 args = EXC.badarity (ErlangFun 1 erlang__get__1) args

foreign import do_get_0 :: (ErlangTerm -> ErlangTerm -> ErlangTerm) -> Array ErlangTerm
erlang__get__0 :: ErlangFun
erlang__get__0 [] = arrayToErlangList $ do_get_0 (\x y -> ErlangTuple [x,y])
erlang__get__0 args = EXC.badarity (ErlangFun 0 erlang__get__0) args

foreign import do_get_keys_0 :: Unit -> Array ErlangTerm
erlang__get_keys__0 :: ErlangFun
erlang__get_keys__0 [] = arrayToErlangList $ do_get_keys_0 unit
erlang__get_keys__0 args = EXC.badarity (ErlangFun 0 erlang__get_keys__0) args

foreign import do_get_keys_1 :: (ErlangTerm -> ErlangTerm -> Boolean) -> ErlangTerm -> Array ErlangTerm
erlang__get_keys__1 :: ErlangFun
erlang__get_keys__1 [val] = arrayToErlangList $ do_get_keys_1 (==) val
erlang__get_keys__1 [_] = EXC.badarg unit
erlang__get_keys__1 args = EXC.badarity (ErlangFun 1 erlang__get_keys__1) args

foreign import do_erase_0 :: (ErlangTerm -> ErlangTerm -> ErlangTerm) -> Array ErlangTerm
erlang__erase__0 :: ErlangFun
erlang__erase__0 [] = arrayToErlangList $ do_erase_0 (\x y -> ErlangTuple [x,y])
erlang__erase__0 args = unimplemented "erlang__erase__0"

foreign import do_erase_1 :: (ErlangTerm -> ErlangTerm -> Boolean) -> ErlangTerm -> ErlangTerm -> ErlangTerm
erlang__erase__1 :: ErlangFun
erlang__erase__1 [key] = do_erase_1 (==) key (ErlangAtom "undefined")
erlang__erase__1 [_] = EXC.badarg unit
erlang__erase__1 args = EXC.badarity (ErlangFun 1 erlang__erase__1) args

--------------------------------------------------------------------------------

erlang__process_display__2 :: ErlangFun
erlang__process_display__2 args = unimplemented "erlang__process_display__2"
erlang__process_display__2 [_,_] = EXC.badarg unit
erlang__process_display__2 args = EXC.badarity (ErlangFun 2 erlang__process_display__2) args

erlang__fun_info_mfa__1 :: ErlangFun
erlang__fun_info_mfa__1 args = unimplemented "erlang__fun_info_mfa__1"
erlang__fun_info_mfa__1 [_] = EXC.badarg unit
erlang__fun_info_mfa__1 args = EXC.badarity (ErlangFun 1 erlang__fun_info_mfa__1) args

erlang__nif_error__2 :: ErlangFun
erlang__nif_error__2 args = erlang__error__2 args
erlang__nif_error__2 [_,_] = EXC.badarg unit
erlang__nif_error__2 args = EXC.badarity (ErlangFun 2 erlang__nif_error__2) args

erlang__get_stacktrace__0 :: ErlangFun
erlang__get_stacktrace__0 args = unimplemented "erlang__get_stacktrace__0"

erlang__registered__0 :: ErlangFun
erlang__registered__0 args = unimplemented "erlang__registered__0"

erlang__get_module_info__1 :: ErlangFun
erlang__get_module_info__1 args = unimplemented "erlang__get_module_info__1"
erlang__get_module_info__1 [_] = EXC.badarg unit
erlang__get_module_info__1 args = EXC.badarity (ErlangFun 1 erlang__get_module_info__1) args

erlang__module_info__1 :: ErlangFun
erlang__module_info__1 args = unimplemented "erlang__module_info__1"
erlang__module_info__1 [_] = EXC.badarg unit
erlang__module_info__1 args = EXC.badarity (ErlangFun 1 erlang__module_info__1) args

erlang__cancel_timer__1 :: ErlangFun
erlang__cancel_timer__1 args = unimplemented "erlang__cancel_timer__1"
erlang__cancel_timer__1 [_] = EXC.badarg unit
erlang__cancel_timer__1 args = EXC.badarity (ErlangFun 1 erlang__cancel_timer__1) args

erlang__dist_ctrl_get_data_notification__1 :: ErlangFun
erlang__dist_ctrl_get_data_notification__1 args = unimplemented "erlang__dist_ctrl_get_data_notification__1"
erlang__dist_ctrl_get_data_notification__1 [_] = EXC.badarg unit
erlang__dist_ctrl_get_data_notification__1 args = EXC.badarity (ErlangFun 1 erlang__dist_ctrl_get_data_notification__1) args

erlang__is_builtin__3 :: ErlangFun
erlang__is_builtin__3 args = unimplemented "erlang__is_builtin__3"
erlang__is_builtin__3 [_,_,_] = EXC.badarg unit
erlang__is_builtin__3 args = EXC.badarity (ErlangFun 3 erlang__is_builtin__3) args

erlang__suspend_process__1 :: ErlangFun
erlang__suspend_process__1 args = unimplemented "erlang__suspend_process__1"
erlang__suspend_process__1 [_] = EXC.badarg unit
erlang__suspend_process__1 args = EXC.badarity (ErlangFun 1 erlang__suspend_process__1) args

erlang__spawn_link__2 :: ErlangFun
erlang__spawn_link__2 args = unimplemented "erlang__spawn_link__2"
erlang__spawn_link__2 [_,_] = EXC.badarg unit
erlang__spawn_link__2 args = EXC.badarity (ErlangFun 2 erlang__spawn_link__2) args

erlang__dist_ctrl_get_data__1 :: ErlangFun
erlang__dist_ctrl_get_data__1 args = unimplemented "erlang__dist_ctrl_get_data__1"
erlang__dist_ctrl_get_data__1 [_] = EXC.badarg unit
erlang__dist_ctrl_get_data__1 args = EXC.badarity (ErlangFun 1 erlang__dist_ctrl_get_data__1) args

erlang__setnode__3 :: ErlangFun
erlang__setnode__3 args = unimplemented "erlang__setnode__3"
erlang__setnode__3 [_,_,_] = EXC.badarg unit
erlang__setnode__3 args = EXC.badarity (ErlangFun 3 erlang__setnode__3) args

erlang__now__0 :: ErlangFun
erlang__now__0 args = unimplemented "erlang__now__0"

erlang__ports__0 :: ErlangFun
erlang__ports__0 args = unimplemented "erlang__ports__0"

erlang__dt_spread_tag__1 :: ErlangFun
erlang__dt_spread_tag__1 args = unimplemented "erlang__dt_spread_tag__1"
erlang__dt_spread_tag__1 [_] = EXC.badarg unit
erlang__dt_spread_tag__1 args = EXC.badarity (ErlangFun 1 erlang__dt_spread_tag__1) args

erlang__convert_time_unit__3 :: ErlangFun
erlang__convert_time_unit__3 args = unimplemented "erlang__convert_time_unit__3"
erlang__convert_time_unit__3 [_,_,_] = EXC.badarg unit
erlang__convert_time_unit__3 args = EXC.badarity (ErlangFun 3 erlang__convert_time_unit__3) args

erlang__decode_packet__3 :: ErlangFun
erlang__decode_packet__3 args = unimplemented "erlang__decode_packet__3"
erlang__decode_packet__3 [_,_,_] = EXC.badarg unit
erlang__decode_packet__3 args = EXC.badarity (ErlangFun 3 erlang__decode_packet__3) args

erlang__get_cookie__0 :: ErlangFun
erlang__get_cookie__0 args = unimplemented "erlang__get_cookie__0"

erlang__unique_integer__1 :: ErlangFun
erlang__unique_integer__1 args = unimplemented "erlang__unique_integer__1"
erlang__unique_integer__1 [_] = EXC.badarg unit
erlang__unique_integer__1 args = EXC.badarity (ErlangFun 1 erlang__unique_integer__1) args

erlang__exit__2 :: ErlangFun
erlang__exit__2 args = unimplemented "erlang__exit__2"
erlang__exit__2 [_,_] = EXC.badarg unit
erlang__exit__2 args = EXC.badarity (ErlangFun 2 erlang__exit__2) args

erlang__purge_module__1 :: ErlangFun
erlang__purge_module__1 args = unimplemented "erlang__purge_module__1"
erlang__purge_module__1 [_] = EXC.badarg unit
erlang__purge_module__1 args = EXC.badarity (ErlangFun 1 erlang__purge_module__1) args

erlang__dt_prepend_vm_tag_data__1 :: ErlangFun
erlang__dt_prepend_vm_tag_data__1 args = unimplemented "erlang__dt_prepend_vm_tag_data__1"
erlang__dt_prepend_vm_tag_data__1 [_] = EXC.badarg unit
erlang__dt_prepend_vm_tag_data__1 args = EXC.badarity (ErlangFun 1 erlang__dt_prepend_vm_tag_data__1) args

erlang__has_prepared_code_on_load__1 :: ErlangFun
erlang__has_prepared_code_on_load__1 args = unimplemented "erlang__has_prepared_code_on_load__1"
erlang__has_prepared_code_on_load__1 [_] = EXC.badarg unit
erlang__has_prepared_code_on_load__1 args = EXC.badarity (ErlangFun 1 erlang__has_prepared_code_on_load__1) args

erlang__external_size__2 :: ErlangFun
erlang__external_size__2 args = unimplemented "erlang__external_size__2"
erlang__external_size__2 [_,_] = EXC.badarg unit
erlang__external_size__2 args = EXC.badarity (ErlangFun 2 erlang__external_size__2) args

erlang__is_alive__0 :: ErlangFun
erlang__is_alive__0 args = unimplemented "erlang__is_alive__0"

erlang__make_tuple__2 :: ErlangFun
erlang__make_tuple__2 [ErlangInt barity, what]
  | DM.Just arity <- H.bigIntToInt barity
  = ErlangTuple $ DA.replicate arity what
erlang__make_tuple__2 [_,_] = EXC.badarg unit
erlang__make_tuple__2 args = EXC.badarity (ErlangFun 2 erlang__make_tuple__2) args

erlang__and__2 :: ErlangFun
erlang__and__2 args = unimplemented "erlang__and__2"
erlang__and__2 [_,_] = EXC.badarg unit
erlang__and__2 args = EXC.badarity (ErlangFun 2 erlang__and__2) args

foreign import do_is_process_alive_1 :: Int -> Boolean
erlang__is_process_alive__1 :: ErlangFun
erlang__is_process_alive__1 [ErlangPID id] =
    boolToTerm $ do_is_process_alive_1 id
erlang__is_process_alive__1 [_] = EXC.badarg unit
erlang__is_process_alive__1 args = EXC.badarity (ErlangFun 1 erlang__is_process_alive__1) args

erlang__dist_get_stat__1 :: ErlangFun
erlang__dist_get_stat__1 args = unimplemented "erlang__dist_get_stat__1"
erlang__dist_get_stat__1 [_] = EXC.badarg unit
erlang__dist_get_stat__1 args = EXC.badarity (ErlangFun 1 erlang__dist_get_stat__1) args

erlang__alloc_sizes__1 :: ErlangFun
erlang__alloc_sizes__1 args = unimplemented "erlang__alloc_sizes__1"
erlang__alloc_sizes__1 [_] = EXC.badarg unit
erlang__alloc_sizes__1 args = EXC.badarity (ErlangFun 1 erlang__alloc_sizes__1) args

erlang__spawn_opt__2 :: ErlangFun
erlang__spawn_opt__2 args = unimplemented "erlang__spawn_opt__2"
erlang__spawn_opt__2 [_,_] = EXC.badarg unit
erlang__spawn_opt__2 args = EXC.badarity (ErlangFun 2 erlang__spawn_opt__2) args

erlang__element__2 :: ErlangFun
erlang__element__2 [ErlangInt bpos, ErlangTuple array]
  | DM.Just pos <- H.bigIntToInt bpos
  , DM.Just res <- DA.index array (pos-1) = res
erlang__element__2 [_,_] = EXC.badarg unit
erlang__element__2 args = EXC.badarity (ErlangFun 2 erlang__element__2) args

erlang__port_get_data__1 :: ErlangFun
erlang__port_get_data__1 args = unimplemented "erlang__port_get_data__1"
erlang__port_get_data__1 [_] = EXC.badarg unit
erlang__port_get_data__1 args = EXC.badarity (ErlangFun 1 erlang__port_get_data__1) args

erlang__group_leader__2 :: ErlangFun
erlang__group_leader__2 args = unimplemented "erlang__group_leader__2"
erlang__group_leader__2 [_,_] = EXC.badarg unit
erlang__group_leader__2 args = EXC.badarity (ErlangFun 2 erlang__group_leader__2) args

erlang__split_binary__2 :: ErlangFun
erlang__split_binary__2 args = unimplemented "erlang__split_binary__2"
erlang__split_binary__2 [_,_] = EXC.badarg unit
erlang__split_binary__2 args = EXC.badarity (ErlangFun 2 erlang__split_binary__2) args

-- https://github.com/erlang/otp/blob/ec6e0a3d1ca4e38deee26aee15bb762ee1f86a0a/erts/emulator/beam/bif.c#L5061
erlang__phash__2 :: ErlangFun
erlang__phash__2 [term, ErlangInt range] =
    ErlangInt $ finalize_hash range $ unsignedToBigInt $ (make_hash term (DU.fromInt 0))
erlang__phash__2 [_,_] = EXC.badarg unit
erlang__phash__2 args = EXC.badarity (ErlangFun 2 erlang__phash__2) args

finalize_hash range res = (DBI.fromInt 1) + (res `mod` range)

unsignedToBigInt :: DU.UInt -> DBI.BigInt
unsignedToBigInt x =
    case DU.toInt x of
        a | a >= 0 -> DBI.fromInt a
        a -> (DBI.shl (DBI.fromInt 1) 32.0) + (DBI.fromInt a)

c_FUNNY_NUMBER1  = DU.fromInt 268440163
c_FUNNY_NUMBER2  = DU.fromInt 268439161
c_FUNNY_NUMBER3  = DU.fromInt 268435459
c_FUNNY_NUMBER4  = DU.fromInt 268436141
c_FUNNY_NUMBER5  = DU.fromInt 268438633
c_FUNNY_NUMBER6  = DU.fromInt 268437017
c_FUNNY_NUMBER7  = DU.fromInt 268438039
c_FUNNY_NUMBER8  = DU.fromInt 268437511
c_FUNNY_NUMBER9  = DU.fromInt 268439627
c_FUNNY_NUMBER10 = DU.fromInt 268440479
c_FUNNY_NUMBER11 = DU.fromInt 268440577
c_FUNNY_NUMBER12 = DU.fromInt 268440581
c_FUNNY_NUMBER13 = DU.fromInt 268440593
c_FUNNY_NUMBER14 = DU.fromInt 268440611

-- https://github.com/erlang/otp/blob/ec6e0a3d1ca4e38deee26aee15bb762ee1f86a0a/erts/emulator/beam/utils.c#L815
make_hash :: ErlangTerm -> DU.UInt -> DU.UInt
make_hash (ErlangInt n) hash =
    let
        { isNegative: isNeg, value: bytes } = DBI.digitsInBase 256 n
        h1 = DA.foldl (\acc el -> acc*c_FUNNY_NUMBER2 + (DU.fromInt el)) hash (DAN.reverse $ pad_arr bytes)
    in
        case isNeg of
            true -> h1 * c_FUNNY_NUMBER4
            false -> h1 * c_FUNNY_NUMBER3
make_hash (ErlangPID n) hash =
    let
        { isNegative: _, value: bytes } = DBI.digitsInBase 256 (DBI.fromInt n)
        h1 = DA.foldl (\acc el -> acc*c_FUNNY_NUMBER5 + (DU.fromInt el)) hash (DAN.reverse $ pad_arr bytes)
    in
        h1 * c_FUNNY_NUMBER6
make_hash (ErlangReference n) hash =
    let
        { isNegative: _, value: bytes } = DBI.digitsInBase 256 (DBI.fromInt n)
        h1 = DA.foldl (\acc el -> acc*c_FUNNY_NUMBER9 + (DU.fromInt el)) hash (DAN.reverse $ pad_arr bytes)
    in
        h1 * c_FUNNY_NUMBER10

make_hash (ErlangAtom a) hash =
    hash * c_FUNNY_NUMBER1 + (hash_atom a)

--make_hash (ErlangFloat a) hash = ...
--make_hash (ErlangBinary a) hash = ...

make_hash (ErlangTuple t) hash =
    (DA.foldl (\acc el -> make_hash el acc) hash t) * c_FUNNY_NUMBER9 + (DU.fromInt (DA.length t))

make_hash ErlangEmptyList hash = hash * c_FUNNY_NUMBER3 + (DU.fromInt 1)

make_hash (ErlangCons (ErlangInt n) t@(ErlangCons _ _)) hash | n < DBI.fromInt 256, DM.Just i <- H.bigIntToInt n =
    make_hash t (hash*c_FUNNY_NUMBER2 + (DU.fromInt i))
make_hash (ErlangCons (ErlangInt n) t) hash | n < DBI.fromInt 256, DM.Just i <- H.bigIntToInt n =
    (make_hash t (hash*c_FUNNY_NUMBER2 + (DU.fromInt i))) * c_FUNNY_NUMBER8
make_hash (ErlangCons h t@(ErlangCons _ _)) hash =
    make_hash t (make_hash h hash)
make_hash (ErlangCons h t) hash =
    (make_hash t (make_hash h hash)) * c_FUNNY_NUMBER8

make_hash _ _ =
    let
        a = EXC.badarg unit
    in
        (DU.fromInt 1)

pad_arr :: DAN.NonEmptyArray Int -> DAN.NonEmptyArray Int
pad_arr a | DAN.length a < 4 = pad_arr (DAN.cons 0 a)
          | otherwise = a

-- https://github.com/erlang/otp/blob/00a80c4d81c62c8820172b35c695f98489f4b118/erts/emulator/beam/atom.c#L129
hash_atom s = hash_atom_int s (DU.fromInt 0)
hash_atom_int :: String -> DU.UInt -> DU.UInt
hash_atom_int s hash =
    case DSTR.uncons s of
        DM.Nothing -> hash
        DM.Just {head: h, tail: t} ->
            let
                v = (DU.fromInt (H.codePointToInt h))
                hash1 = ((DU.shl hash (DU.fromInt 4)) + v)
                g = DU.shr hash1 (DU.fromInt 28)
                b = g > (DU.fromInt 0)
            in
                case b of
                    false ->
                        hash_atom_int t hash1
                    true ->
                        hash_atom_int t (DU.xor (DU.xor hash1 (DU.shl g (DU.fromInt 4))) (DU.shl g (DU.fromInt 28)))

erlang__dist_ctrl_put_data__2 :: ErlangFun
erlang__dist_ctrl_put_data__2 args = unimplemented "erlang__dist_ctrl_put_data__2"
erlang__dist_ctrl_put_data__2 [_,_] = EXC.badarg unit
erlang__dist_ctrl_put_data__2 args = EXC.badarity (ErlangFun 2 erlang__dist_ctrl_put_data__2) args

erlang__garbage_collect_message_area__0 :: ErlangFun
erlang__garbage_collect_message_area__0 args = unimplemented "erlang__garbage_collect_message_area__0"

erlang__spawn_link__1 :: ErlangFun
erlang__spawn_link__1 args = unimplemented "erlang__spawn_link__1"
erlang__spawn_link__1 [_] = EXC.badarg unit
erlang__spawn_link__1 args = EXC.badarity (ErlangFun 1 erlang__spawn_link__1) args

erlang__garbage_collect__0 :: ErlangFun
erlang__garbage_collect__0 args = unimplemented "erlang__garbage_collect__0"

erlang__trace_pattern__3 :: ErlangFun
erlang__trace_pattern__3 args = unimplemented "erlang__trace_pattern__3"
erlang__trace_pattern__3 [_,_,_] = EXC.badarg unit
erlang__trace_pattern__3 args = EXC.badarity (ErlangFun 3 erlang__trace_pattern__3) args

erlang__dt_restore_tag__1 :: ErlangFun
erlang__dt_restore_tag__1 args = unimplemented "erlang__dt_restore_tag__1"
erlang__dt_restore_tag__1 [_] = EXC.badarg unit
erlang__dt_restore_tag__1 args = EXC.badarity (ErlangFun 1 erlang__dt_restore_tag__1) args

erlang__system_profile__0 :: ErlangFun
erlang__system_profile__0 args = unimplemented "erlang__system_profile__0"

erlang__match_spec_test__3 :: ErlangFun
erlang__match_spec_test__3 args = unimplemented "erlang__match_spec_test__3"
erlang__match_spec_test__3 [_,_,_] = EXC.badarg unit
erlang__match_spec_test__3 args = EXC.badarity (ErlangFun 3 erlang__match_spec_test__3) args

erlang__pre_loaded__0 :: ErlangFun
erlang__pre_loaded__0 args = unimplemented "erlang__pre_loaded__0"

erlang__display_string__1 :: ErlangFun
erlang__display_string__1 args = unimplemented "erlang__display_string__1"
erlang__display_string__1 [_] = EXC.badarg unit
erlang__display_string__1 args = EXC.badarity (ErlangFun 1 erlang__display_string__1) args

erlang__finish_loading__1 :: ErlangFun
erlang__finish_loading__1 args = unimplemented "erlang__finish_loading__1"
erlang__finish_loading__1 [_] = EXC.badarg unit
erlang__finish_loading__1 args = EXC.badarity (ErlangFun 1 erlang__finish_loading__1) args

erlang__spawn_link__3 :: ErlangFun
erlang__spawn_link__3 args = unimplemented "erlang__spawn_link__3"
erlang__spawn_link__3 [_,_,_] = EXC.badarg unit
erlang__spawn_link__3 args = EXC.badarity (ErlangFun 3 erlang__spawn_link__3) args

erlang__garbage_collect__2 :: ErlangFun
erlang__garbage_collect__2 args = unimplemented "erlang__garbage_collect__2"
erlang__garbage_collect__2 [_,_] = EXC.badarg unit
erlang__garbage_collect__2 args = EXC.badarity (ErlangFun 2 erlang__garbage_collect__2) args

erlang__system_flag__2 :: ErlangFun
erlang__system_flag__2 args = unimplemented "erlang__system_flag__2"
erlang__system_flag__2 [_,_] = EXC.badarg unit
erlang__system_flag__2 args = EXC.badarity (ErlangFun 2 erlang__system_flag__2) args

erlang__universaltime_to_localtime__1 :: ErlangFun
erlang__universaltime_to_localtime__1 args = unimplemented "erlang__universaltime_to_localtime__1"
erlang__universaltime_to_localtime__1 [_] = EXC.badarg unit
erlang__universaltime_to_localtime__1 args = EXC.badarity (ErlangFun 1 erlang__universaltime_to_localtime__1) args

erlang__whereis__1 :: ErlangFun
erlang__whereis__1 [ErlangAtom "user"] = erlang__group_leader__0 []
erlang__whereis__1 args = unimplemented "erlang__whereis__1"
erlang__whereis__1 [_] = EXC.badarg unit
erlang__whereis__1 args = EXC.badarity (ErlangFun 1 erlang__whereis__1) args

erlang__port_call__3 :: ErlangFun
erlang__port_call__3 args = unimplemented "erlang__port_call__3"
erlang__port_call__3 [_,_,_] = EXC.badarg unit
erlang__port_call__3 args = EXC.badarity (ErlangFun 3 erlang__port_call__3) args

erlang__date__0 :: ErlangFun
erlang__date__0 args = unimplemented "erlang__date__0"

foreign import do_make_ref_0 :: (Int -> ErlangTerm) -> ErlangTerm
erlang__make_ref__0 :: ErlangFun
erlang__make_ref__0 _ = do_make_ref_0 ErlangReference

erlang__port_control__3 :: ErlangFun
erlang__port_control__3 args = unimplemented "erlang__port_control__3"
erlang__port_control__3 [_,_,_] = EXC.badarg unit
erlang__port_control__3 args = EXC.badarity (ErlangFun 3 erlang__port_control__3) args

erlang__byte_size__1 :: ErlangFun
erlang__byte_size__1 [ErlangBinary buf] = BIN.size buf
erlang__byte_size__1 [_] = EXC.badarg unit
erlang__byte_size__1 args = EXC.badarity (ErlangFun 1 erlang__byte_size__1) args

erlang__check_process_code__2 :: ErlangFun
erlang__check_process_code__2 args = unimplemented "erlang__check_process_code__2"
erlang__check_process_code__2 [_,_] = EXC.badarg unit
erlang__check_process_code__2 args = EXC.badarity (ErlangFun 2 erlang__check_process_code__2) args

erlang__system_monitor__0 :: ErlangFun
erlang__system_monitor__0 args = unimplemented "erlang__system_monitor__0"

erlang__bitsize__1 :: ErlangFun
erlang__bitsize__1 args = unimplemented "erlang__bitsize__1"
erlang__bitsize__1 [_] = EXC.badarg unit
erlang__bitsize__1 args = EXC.badarity (ErlangFun 1 erlang__bitsize__1) args

erlang__nodes__1 :: ErlangFun
erlang__nodes__1 args = unimplemented "erlang__nodes__1"
erlang__nodes__1 [_] = EXC.badarg unit
erlang__nodes__1 args = EXC.badarity (ErlangFun 1 erlang__nodes__1) args

erlang__time__0 :: ErlangFun
erlang__time__0 args = unimplemented "erlang__time__0"

erlang__time_offset__1 :: ErlangFun
erlang__time_offset__1 args = unimplemented "erlang__time_offset__1"
erlang__time_offset__1 [_] = EXC.badarg unit
erlang__time_offset__1 args = EXC.badarity (ErlangFun 1 erlang__time_offset__1) args

erlang__seq_trace_print__1 :: ErlangFun
erlang__seq_trace_print__1 args = unimplemented "erlang__seq_trace_print__1"
erlang__seq_trace_print__1 [_] = EXC.badarg unit
erlang__seq_trace_print__1 args = EXC.badarity (ErlangFun 1 erlang__seq_trace_print__1) args

erlang__send_nosuspend__3 :: ErlangFun
erlang__send_nosuspend__3 [arg1, arg2, _] = erlang__send_nosuspend__2 [arg1, arg2]
erlang__send_nosuspend__3 _ = EXC.badarg unit

erlang__send_nosuspend__2 :: ErlangFun
erlang__send_nosuspend__2 args =
  let a = erlang__send__2 args
  in boolToTerm true

erlang__send__3 :: ErlangFun
erlang__send__3 [arg1, arg2, _] = erlang__send__2 [arg1, arg2]
erlang__send__3 _ = EXC.badarg unit

foreign import do_send_2 :: Int -> ErlangTerm -> ErlangTerm
erlang__send__2 :: ErlangFun
erlang__send__2 [ErlangPID pid, term] = do_send_2 pid term
erlang__send__2 _ = EXC.badarg unit

foreign import do_receive_2 :: ErlangFun -> DBI.BigInt -> (String -> ErlangTerm) -> ErlangTerm
prim_eval__receive__2 :: ErlangFun
prim_eval__receive__2 [ErlangFun 1 fun, ErlangAtom "infinity"] =
  do_receive_2 fun (DBI.fromInt (-1)) ErlangAtom
prim_eval__receive__2 [ErlangFun 1 fun, ErlangInt timeout] | timeout >= DBI.fromInt(0) =
  do_receive_2 fun timeout ErlangAtom
prim_eval__receive__2 _ = EXC.badarg unit

erlang__halt__1 :: ErlangFun
erlang__halt__1 args = unimplemented "erlang__halt__1"
erlang__halt__1 [_] = EXC.badarg unit
erlang__halt__1 args = EXC.badarity (ErlangFun 1 erlang__halt__1) args

erlang__spawn_opt__5 :: ErlangFun
erlang__spawn_opt__5 args = unimplemented "erlang__spawn_opt__5"
erlang__spawn_opt__5 [_,_,_,_,_] = EXC.badarg unit
erlang__spawn_opt__5 args = EXC.badarity (ErlangFun 5 erlang__spawn_opt__5) args

erlang__size__1 :: ErlangFun
erlang__size__1 [ErlangTuple t] = ErlangInt (DBI.fromInt (DA.length t))
erlang__size__1 [ErlangBinary b] = BIN.size b
erlang__size__1 [_] = EXC.badarg unit
erlang__size__1 args = EXC.badarity (ErlangFun 1 erlang__size__1) args

erlang__process_info__1 :: ErlangFun
erlang__process_info__1 args = unimplemented "erlang__process_info__1"
erlang__process_info__1 [_] = EXC.badarg unit
erlang__process_info__1 args = EXC.badarity (ErlangFun 1 erlang__process_info__1) args

erlang__md5__1 :: ErlangFun
erlang__md5__1 args = unimplemented "erlang__md5__1"
erlang__md5__1 [_] = EXC.badarg unit
erlang__md5__1 args = EXC.badarity (ErlangFun 1 erlang__md5__1) args

erlang__binary_part__2 :: ErlangFun
erlang__binary_part__2 args = unimplemented "erlang__binary_part__2"
erlang__binary_part__2 [_,_] = EXC.badarg unit
erlang__binary_part__2 args = EXC.badarity (ErlangFun 2 erlang__binary_part__2) args

erlang__format_cpu_topology__1 :: ErlangFun
erlang__format_cpu_topology__1 args = unimplemented "erlang__format_cpu_topology__1"
erlang__format_cpu_topology__1 [_] = EXC.badarg unit
erlang__format_cpu_topology__1 args = EXC.badarity (ErlangFun 1 erlang__format_cpu_topology__1) args

foreign import do_spawn_1 :: (Unit -> ErlangTerm) -> (Int -> ErlangTerm) -> ErlangTerm
erlang__spawn__1 :: ErlangFun
erlang__spawn__1 [f] =
    do_spawn_1 (\x -> erlang__apply__2 [f, ErlangEmptyList]) ErlangPID
erlang__spawn__1 [_] = EXC.badarg unit
erlang__spawn__1 args = EXC.badarity (ErlangFun 1 erlang__spawn__1) args

erlang__throw__1 :: ErlangFun
erlang__throw__1 [arg] = EXC.throw arg
erlang__throw__1 args = EXC.badarity (ErlangFun 1 erlang__throw__1) args

erlang__load_nif__2 :: ErlangFun
erlang__load_nif__2 args = unimplemented "erlang__load_nif__2"
erlang__load_nif__2 [_,_] = EXC.badarg unit
erlang__load_nif__2 args = EXC.badarity (ErlangFun 2 erlang__load_nif__2) args

erlang__prepare_loading__2 :: ErlangFun
erlang__prepare_loading__2 args = unimplemented "erlang__prepare_loading__2"
erlang__prepare_loading__2 [_,_] = EXC.badarg unit
erlang__prepare_loading__2 args = EXC.badarity (ErlangFun 2 erlang__prepare_loading__2) args

erlang__open_port__2 :: ErlangFun
erlang__open_port__2 args = unimplemented "erlang__open_port__2"
erlang__open_port__2 [_,_] = EXC.badarg unit
erlang__open_port__2 args = EXC.badarity (ErlangFun 2 erlang__open_port__2) args

erlang__port_set_data__2 :: ErlangFun
erlang__port_set_data__2 args = unimplemented "erlang__port_set_data__2"
erlang__port_set_data__2 [_,_] = EXC.badarg unit
erlang__port_set_data__2 args = EXC.badarity (ErlangFun 2 erlang__port_set_data__2) args

foreign import do_self_0 :: (Int -> ErlangTerm) -> ErlangTerm
erlang__self__0 :: ErlangFun
erlang__self__0 _ = do_self_0 ErlangPID

erlang__read_timer__2 :: ErlangFun
erlang__read_timer__2 args = unimplemented "erlang__read_timer__2"
erlang__read_timer__2 [_,_] = EXC.badarg unit
erlang__read_timer__2 args = EXC.badarity (ErlangFun 2 erlang__read_timer__2) args

erlang__statistics__1 :: ErlangFun
erlang__statistics__1 args = unimplemented "erlang__statistics__1"
erlang__statistics__1 [_] = EXC.badarg unit
erlang__statistics__1 args = EXC.badarity (ErlangFun 1 erlang__statistics__1) args

erlang__nodes__0 :: ErlangFun
erlang__nodes__0 args = unimplemented "erlang__nodes__0"

erlang__insert_element__3 :: ErlangFun
erlang__insert_element__3 args = unimplemented "erlang__insert_element__3"
erlang__insert_element__3 [_,_,_] = EXC.badarg unit
erlang__insert_element__3 args = EXC.badarity (ErlangFun 3 erlang__insert_element__3) args

erlang__spawn__3 :: ErlangFun
erlang__spawn__3 args = unimplemented "erlang__spawn__3"
erlang__spawn__3 [_,_,_] = EXC.badarg unit
erlang__spawn__3 args = EXC.badarity (ErlangFun 3 erlang__spawn__3) args

erlang__send_after__4 :: ErlangFun
erlang__send_after__4 args = unimplemented "erlang__send_after__4"
erlang__send_after__4 [_,_,_,_] = EXC.badarg unit
erlang__send_after__4 args = EXC.badarity (ErlangFun 4 erlang__send_after__4) args

erlang__trace__3 :: ErlangFun
erlang__trace__3 args = unimplemented "erlang__trace__3"
erlang__trace__3 [_,_,_] = EXC.badarg unit
erlang__trace__3 args = EXC.badarity (ErlangFun 3 erlang__trace__3) args

erlang__adler32__1 :: ErlangFun
erlang__adler32__1 args = unimplemented "erlang__adler32__1"
erlang__adler32__1 [_] = EXC.badarg unit
erlang__adler32__1 args = EXC.badarity (ErlangFun 1 erlang__adler32__1) args

erlang__dt_get_tag_data__0 :: ErlangFun
erlang__dt_get_tag_data__0 args = unimplemented "erlang__dt_get_tag_data__0"

erlang__resume_process__1 :: ErlangFun
erlang__resume_process__1 args = unimplemented "erlang__resume_process__1"
erlang__resume_process__1 [_] = EXC.badarg unit
erlang__resume_process__1 args = EXC.badarity (ErlangFun 1 erlang__resume_process__1) args

erlang__port_close__1 :: ErlangFun
erlang__port_close__1 args = unimplemented "erlang__port_close__1"
erlang__port_close__1 [_] = EXC.badarg unit
erlang__port_close__1 args = EXC.badarity (ErlangFun 1 erlang__port_close__1) args

erlang__port_call__2 :: ErlangFun
erlang__port_call__2 args = unimplemented "erlang__port_call__2"
erlang__port_call__2 [_,_] = EXC.badarg unit
erlang__port_call__2 args = EXC.badarity (ErlangFun 2 erlang__port_call__2) args

erlang__dt_get_tag__0 :: ErlangFun
erlang__dt_get_tag__0 args = unimplemented "erlang__dt_get_tag__0"

erlang__universaltime_to_posixtime__1 :: ErlangFun
erlang__universaltime_to_posixtime__1 args = unimplemented "erlang__universaltime_to_posixtime__1"
erlang__universaltime_to_posixtime__1 [_] = EXC.badarg unit
erlang__universaltime_to_posixtime__1 args = EXC.badarity (ErlangFun 1 erlang__universaltime_to_posixtime__1) args

erlang__md5_init__0 :: ErlangFun
erlang__md5_init__0 args = unimplemented "erlang__md5_init__0"

erlang__link__1 :: ErlangFun
erlang__link__1 args = unimplemented "erlang__link__1"
erlang__link__1 [_] = EXC.badarg unit
erlang__link__1 args = EXC.badarity (ErlangFun 1 erlang__link__1) args

erlang__spawn_opt__1 :: ErlangFun
erlang__spawn_opt__1 args = unimplemented "erlang__spawn_opt__1"
erlang__spawn_opt__1 [_] = EXC.badarg unit
erlang__spawn_opt__1 args = EXC.badarity (ErlangFun 1 erlang__spawn_opt__1) args

erlang__monitor_node__2 :: ErlangFun
erlang__monitor_node__2 args = unimplemented "erlang__monitor_node__2"
erlang__monitor_node__2 [_,_] = EXC.badarg unit
erlang__monitor_node__2 args = EXC.badarity (ErlangFun 2 erlang__monitor_node__2) args

erlang__time_offset__0 :: ErlangFun
erlang__time_offset__0 args = unimplemented "erlang__time_offset__0"

erlang__port_connect__2 :: ErlangFun
erlang__port_connect__2 args = unimplemented "erlang__port_connect__2"
erlang__port_connect__2 [_,_] = EXC.badarg unit
erlang__port_connect__2 args = EXC.badarity (ErlangFun 2 erlang__port_connect__2) args

erlang__setelement__3 :: ErlangFun
erlang__setelement__3 [ErlangInt bpos, ErlangTuple tuple, new_el]
    | DM.Just pos <- H.bigIntToInt bpos
    , DM.Just new_tuple <- DA.updateAt (pos - 1) new_el tuple =
    ErlangTuple new_tuple
erlang__setelement__3 [_,_,_] = EXC.badarg unit
erlang__setelement__3 args = EXC.badarity (ErlangFun 3 erlang__setelement__3) args

erlang__gather_gc_info_result__1 :: ErlangFun
erlang__gather_gc_info_result__1 args = unimplemented "erlang__gather_gc_info_result__1"
erlang__gather_gc_info_result__1 [_] = EXC.badarg unit
erlang__gather_gc_info_result__1 args = EXC.badarity (ErlangFun 1 erlang__gather_gc_info_result__1) args

erlang__tuple_size__1 :: ErlangFun
erlang__tuple_size__1 [ErlangTuple a] = ErlangInt $ DBI.fromInt $ DA.length a
erlang__tuple_size__1 [_] = EXC.badarg unit
erlang__tuple_size__1 args = EXC.badarity (ErlangFun 1 erlang__tuple_size__1) args

erlang__system_monitor__1 :: ErlangFun
erlang__system_monitor__1 args = unimplemented "erlang__system_monitor__1"
erlang__system_monitor__1 [_] = EXC.badarg unit
erlang__system_monitor__1 args = EXC.badarity (ErlangFun 1 erlang__system_monitor__1) args

erlang__timestamp__0 :: ErlangFun
erlang__timestamp__0 args = unimplemented "erlang__timestamp__0"

erlang__system_time__1 :: ErlangFun
erlang__system_time__1 args = unimplemented "erlang__system_time__1"
erlang__system_time__1 [_] = EXC.badarg unit
erlang__system_time__1 args = EXC.badarity (ErlangFun 1 erlang__system_time__1) args

erlang__register__2 :: ErlangFun
erlang__register__2 args = unimplemented "erlang__register__2"
erlang__register__2 [_,_] = EXC.badarg unit
erlang__register__2 args = EXC.badarity (ErlangFun 2 erlang__register__2) args

erlang__dmonitor_node__3 :: ErlangFun
erlang__dmonitor_node__3 args = unimplemented "erlang__dmonitor_node__3"
erlang__dmonitor_node__3 [_,_,_] = EXC.badarg unit
erlang__dmonitor_node__3 args = EXC.badarity (ErlangFun 3 erlang__dmonitor_node__3) args

erlang__fun_info__1 :: ErlangFun
erlang__fun_info__1 args = unimplemented "erlang__fun_info__1"
erlang__fun_info__1 [_] = EXC.badarg unit
erlang__fun_info__1 args = EXC.badarity (ErlangFun 1 erlang__fun_info__1) args

erlang__fun_info__2 :: ErlangFun
erlang__fun_info__2 [ErlangFun a _, k@(ErlangAtom "type")] = ErlangTuple [k, ErlangAtom "external"]
erlang__fun_info__2 [ErlangFun a _, k@(ErlangAtom "arity")] = ErlangTuple [k, ErlangInt $ DBI.fromInt a]
erlang__fun_info__2 [ErlangFun a _, k@(ErlangAtom "env")] = ErlangTuple [k, ErlangEmptyList]
erlang__fun_info__2 [ErlangFun a _, k@(ErlangAtom "module")] = ErlangTuple [k, ErlangAtom "fixme"]
erlang__fun_info__2 [ErlangFun a _, k@(ErlangAtom "name")] = ErlangTuple [k, ErlangAtom "fixme"]
erlang__fun_info__2 [_,_] = EXC.badarg unit
erlang__fun_info__2 args = EXC.badarity (ErlangFun 2 erlang__fun_info__2) args

erlang__finish_after_on_load__2 :: ErlangFun
erlang__finish_after_on_load__2 args = unimplemented "erlang__finish_after_on_load__2"
erlang__finish_after_on_load__2 [_,_] = EXC.badarg unit
erlang__finish_after_on_load__2 args = EXC.badarity (ErlangFun 2 erlang__finish_after_on_load__2) args

erlang__send_after__3 :: ErlangFun
erlang__send_after__3 args = unimplemented "erlang__send_after__3"
erlang__send_after__3 [_,_,_] = EXC.badarg unit
erlang__send_after__3 args = EXC.badarity (ErlangFun 3 erlang__send_after__3) args

erlang__trace_pattern__2 :: ErlangFun
erlang__trace_pattern__2 args = unimplemented "erlang__trace_pattern__2"
erlang__trace_pattern__2 [_,_] = EXC.badarg unit
erlang__trace_pattern__2 args = EXC.badarity (ErlangFun 2 erlang__trace_pattern__2) args

erlang__port_command__3 :: ErlangFun
erlang__port_command__3 args = unimplemented "erlang__port_command__3"
erlang__port_command__3 [_,_,_] = EXC.badarg unit
erlang__port_command__3 args = EXC.badarity (ErlangFun 3 erlang__port_command__3) args

erlang__alloc_info__1 :: ErlangFun
erlang__alloc_info__1 args = unimplemented "erlang__alloc_info__1"
erlang__alloc_info__1 [_] = EXC.badarg unit
erlang__alloc_info__1 args = EXC.badarity (ErlangFun 1 erlang__alloc_info__1) args

erlang__port_command__2 :: ErlangFun
erlang__port_command__2 args = unimplemented "erlang__port_command__2"
erlang__port_command__2 [_,_] = EXC.badarg unit
erlang__port_command__2 args = EXC.badarity (ErlangFun 2 erlang__port_command__2) args

erlang__external_size__1 :: ErlangFun
erlang__external_size__1 args = unimplemented "erlang__external_size__1"
erlang__external_size__1 [_] = EXC.badarg unit
erlang__external_size__1 args = EXC.badarity (ErlangFun 1 erlang__external_size__1) args

erlang__spawn_opt__3 :: ErlangFun
erlang__spawn_opt__3 args = unimplemented "erlang__spawn_opt__3"
erlang__spawn_opt__3 [_,_,_] = EXC.badarg unit
erlang__spawn_opt__3 args = EXC.badarity (ErlangFun 3 erlang__spawn_opt__3) args

erlang__exit_signal__2 :: ErlangFun
erlang__exit_signal__2 args = unimplemented "erlang__exit_signal__2"
erlang__exit_signal__2 [_,_] = EXC.badarg unit
erlang__exit_signal__2 args = EXC.badarity (ErlangFun 2 erlang__exit_signal__2) args

erlang__display_nl__0 :: ErlangFun
erlang__display_nl__0 args = unimplemented "erlang__display_nl__0"

erlang__append_element__2 :: ErlangFun
erlang__append_element__2 args = unimplemented "erlang__append_element__2"
erlang__append_element__2 [_,_] = EXC.badarg unit
erlang__append_element__2 args = EXC.badarity (ErlangFun 2 erlang__append_element__2) args

erlang__delete_element__2 :: ErlangFun
erlang__delete_element__2 args = unimplemented "erlang__delete_element__2"
erlang__delete_element__2 [_,_] = EXC.badarg unit
erlang__delete_element__2 args = EXC.badarity (ErlangFun 2 erlang__delete_element__2) args

erlang__round__1 :: ErlangFun
erlang__round__1 [i@(ErlangInt _)] = i
erlang__round__1 [ErlangFloat f]
  | DM.Just shot1 <- DBI.fromNumber f
  , DM.Just shot2 <- DBI.fromNumber (f + 1.0)
  , DM.Just shot3 <- DBI.fromNumber (f - 1.0) =
    let back1 = DBI.toNumber shot1
        back2 = DBI.toNumber shot2
        back3 = DBI.toNumber shot3
    in if f > 0.0
       then if f - back1 < back2 - f
            then ErlangInt shot1
            else ErlangInt shot2
       else if f < 0.0
       then if back1 - f < f - back3
            then ErlangInt shot1
            else ErlangInt shot3
       else ErlangInt (DBI.fromInt 0)
erlang__round__1 [_] = EXC.badarg unit
erlang__round__1 args = EXC.badarity (ErlangFun 1 erlang__round__1) args

erlang__crc32__2 :: ErlangFun
erlang__crc32__2 args = unimplemented "erlang__crc32__2"
erlang__crc32__2 [_,_] = EXC.badarg unit
erlang__crc32__2 args = EXC.badarity (ErlangFun 2 erlang__crc32__2) args

erlang__adler32__2 :: ErlangFun
erlang__adler32__2 args = unimplemented "erlang__adler32__2"
erlang__adler32__2 [_,_] = EXC.badarg unit
erlang__adler32__2 args = EXC.badarity (ErlangFun 2 erlang__adler32__2) args

erlang__md5_final__1 :: ErlangFun
erlang__md5_final__1 args = unimplemented "erlang__md5_final__1"
erlang__md5_final__1 [_] = EXC.badarg unit
erlang__md5_final__1 args = EXC.badarity (ErlangFun 1 erlang__md5_final__1) args

erlang__monitor_node__3 :: ErlangFun
erlang__monitor_node__3 args = unimplemented "erlang__monitor_node__3"
erlang__monitor_node__3 [_,_,_] = EXC.badarg unit
erlang__monitor_node__3 args = EXC.badarity (ErlangFun 3 erlang__monitor_node__3) args

erlang__monotonic_time__0 :: ErlangFun
erlang__monotonic_time__0 args = unimplemented "erlang__monotonic_time__0"

erlang__nif_error__1 :: ErlangFun
erlang__nif_error__1 args = erlang__error__1 args
erlang__nif_error__1 [_] = EXC.badarg unit
erlang__nif_error__1 args = EXC.badarity (ErlangFun 1 erlang__nif_error__1) args

erlang__check_process_code__3 :: ErlangFun
erlang__check_process_code__3 args = unimplemented "erlang__check_process_code__3"
erlang__check_process_code__3 [_,_,_] = EXC.badarg unit
erlang__check_process_code__3 args = EXC.badarity (ErlangFun 3 erlang__check_process_code__3) args

erlang__localtime__0 :: ErlangFun
erlang__localtime__0 args = unimplemented "erlang__localtime__0"

erlang__trace_delivered__1 :: ErlangFun
erlang__trace_delivered__1 args = unimplemented "erlang__trace_delivered__1"
erlang__trace_delivered__1 [_] = EXC.badarg unit
erlang__trace_delivered__1 args = EXC.badarity (ErlangFun 1 erlang__trace_delivered__1) args

erlang__module_info__0 :: ErlangFun
erlang__module_info__0 args = unimplemented "erlang__module_info__0"

erlang__spawn__2 :: ErlangFun
erlang__spawn__2 args = unimplemented "erlang__spawn__2"
erlang__spawn__2 [_,_] = EXC.badarg unit
erlang__spawn__2 args = EXC.badarity (ErlangFun 2 erlang__spawn__2) args

erlang__set_cookie__2 :: ErlangFun
erlang__set_cookie__2 args = unimplemented "erlang__set_cookie__2"
erlang__set_cookie__2 [_,_] = EXC.badarg unit
erlang__set_cookie__2 args = EXC.badarity (ErlangFun 2 erlang__set_cookie__2) args

erlang__seq_trace_print__2 :: ErlangFun
erlang__seq_trace_print__2 args = unimplemented "erlang__seq_trace_print__2"
erlang__seq_trace_print__2 [_,_] = EXC.badarg unit
erlang__seq_trace_print__2 args = EXC.badarity (ErlangFun 2 erlang__seq_trace_print__2) args

erlang__suspend_process__2 :: ErlangFun
erlang__suspend_process__2 args = unimplemented "erlang__suspend_process__2"
erlang__suspend_process__2 [_,_] = EXC.badarg unit
erlang__suspend_process__2 args = EXC.badarity (ErlangFun 2 erlang__suspend_process__2) args

erlang__crc32_combine__3 :: ErlangFun
erlang__crc32_combine__3 args = unimplemented "erlang__crc32_combine__3"
erlang__crc32_combine__3 [_,_,_] = EXC.badarg unit
erlang__crc32_combine__3 args = EXC.badarity (ErlangFun 3 erlang__crc32_combine__3) args

erlang__process_info__2 :: ErlangFun
erlang__process_info__2 args = unimplemented "erlang__process_info__2"
erlang__process_info__2 [_,_] = EXC.badarg unit
erlang__process_info__2 args = EXC.badarity (ErlangFun 2 erlang__process_info__2) args

erlang__unique_integer__0 :: ErlangFun
erlang__unique_integer__0 args = ErlangInt $ DBI.fromInt 42 -- FIXME

erlang__system_time__0 :: ErlangFun
erlang__system_time__0 _
    | TDUR.Milliseconds n <- TINS.unInstant $ unsafePerformEffect now, DM.Just m <- DBI.fromNumber n =
    ErlangInt m
erlang__system_time__0 _ = EXC.badarg unit

erlang__yield__0 :: ErlangFun
erlang__yield__0 args = unimplemented "erlang__yield__0"

erlang__posixtime_to_universaltime__1 :: ErlangFun
erlang__posixtime_to_universaltime__1 args = unimplemented "erlang__posixtime_to_universaltime__1"
erlang__posixtime_to_universaltime__1 [_] = EXC.badarg unit
erlang__posixtime_to_universaltime__1 args = EXC.badarity (ErlangFun 1 erlang__posixtime_to_universaltime__1) args

erlang__start_timer__3 :: ErlangFun
erlang__start_timer__3 args = unimplemented "erlang__start_timer__3"
erlang__start_timer__3 [_,_,_] = EXC.badarg unit
erlang__start_timer__3 args = EXC.badarity (ErlangFun 3 erlang__start_timer__3) args

erlang__port_info__1 :: ErlangFun
erlang__port_info__1 args = unimplemented "erlang__port_info__1"
erlang__port_info__1 [_] = EXC.badarg unit
erlang__port_info__1 args = EXC.badarity (ErlangFun 1 erlang__port_info__1) args

erlang__process_flag__2 :: ErlangFun
erlang__process_flag__2 args = unimplemented "erlang__process_flag__2"
erlang__process_flag__2 [_,_] = EXC.badarg unit
erlang__process_flag__2 args = EXC.badarity (ErlangFun 2 erlang__process_flag__2) args

erlang__monotonic_time__1 :: ErlangFun
erlang__monotonic_time__1 args = unimplemented "erlang__monotonic_time__1"
erlang__monotonic_time__1 [_] = EXC.badarg unit
erlang__monotonic_time__1 args = EXC.badarity (ErlangFun 1 erlang__monotonic_time__1) args

erlang__seq_trace_info__1 :: ErlangFun
erlang__seq_trace_info__1 args = unimplemented "erlang__seq_trace_info__1"
erlang__seq_trace_info__1 [_] = EXC.badarg unit
erlang__seq_trace_info__1 args = EXC.badarity (ErlangFun 1 erlang__seq_trace_info__1) args

erlang__memory__1 :: ErlangFun
erlang__memory__1 args = unimplemented "erlang__memory__1"
erlang__memory__1 [_] = EXC.badarg unit
erlang__memory__1 args = EXC.badarity (ErlangFun 1 erlang__memory__1) args

erlang__get_module_info__2 :: ErlangFun
erlang__get_module_info__2 args = unimplemented "erlang__get_module_info__2"
erlang__get_module_info__2 [_,_] = EXC.badarg unit
erlang__get_module_info__2 args = EXC.badarity (ErlangFun 2 erlang__get_module_info__2) args

erlang__seq_trace__2 :: ErlangFun
erlang__seq_trace__2 args = unimplemented "erlang__seq_trace__2"
erlang__seq_trace__2 [_,_] = EXC.badarg unit
erlang__seq_trace__2 args = EXC.badarity (ErlangFun 2 erlang__seq_trace__2) args

erlang__disconnect_node__1 :: ErlangFun
erlang__disconnect_node__1 args = unimplemented "erlang__disconnect_node__1"
erlang__disconnect_node__1 [_] = EXC.badarg unit
erlang__disconnect_node__1 args = EXC.badarity (ErlangFun 1 erlang__disconnect_node__1) args

erlang__setnode__2 :: ErlangFun
erlang__setnode__2 args = unimplemented "erlang__setnode__2"
erlang__setnode__2 [_,_] = EXC.badarg unit
erlang__setnode__2 args = EXC.badarity (ErlangFun 2 erlang__setnode__2) args

erlang__node__0 :: ErlangFun
erlang__node__0 [] = ErlangAtom "nonode@nohost"
erlang__node__0 args = EXC.badarity (ErlangFun 0 erlang__node__0) args

erlang__exit__1 :: ErlangFun
erlang__exit__1 [arg] = EXC.exit arg
erlang__exit__1 [_] = EXC.badarg unit
erlang__exit__1 args = EXC.badarity (ErlangFun 1 erlang__exit__1) args

erlang__system_info__1 :: ErlangFun
erlang__system_info__1 args = unimplemented "erlang__system_info__1"
erlang__system_info__1 [_] = EXC.badarg unit
erlang__system_info__1 args = EXC.badarity (ErlangFun 1 erlang__system_info__1) args

erlang__binary_part__3 :: ErlangFun
erlang__binary_part__3 args = unimplemented "erlang__binary_part__3"
erlang__binary_part__3 [_,_,_] = EXC.badarg unit
erlang__binary_part__3 args = EXC.badarity (ErlangFun 3 erlang__binary_part__3) args

erlang__halt__2 :: ErlangFun
erlang__halt__2 args = unimplemented "erlang__halt__2"
erlang__halt__2 [_,_] = EXC.badarg unit
erlang__halt__2 args = EXC.badarity (ErlangFun 2 erlang__halt__2) args

erlang__localtime_to_universaltime__2 :: ErlangFun
erlang__localtime_to_universaltime__2 args = unimplemented "erlang__localtime_to_universaltime__2"
erlang__localtime_to_universaltime__2 [_,_] = EXC.badarg unit
erlang__localtime_to_universaltime__2 args = EXC.badarity (ErlangFun 2 erlang__localtime_to_universaltime__2) args

erlang__cancel_timer__2 :: ErlangFun
erlang__cancel_timer__2 args = unimplemented "erlang__cancel_timer__2"
erlang__cancel_timer__2 [_,_] = EXC.badarg unit
erlang__cancel_timer__2 args = EXC.badarity (ErlangFun 2 erlang__cancel_timer__2) args

erlang__display__1 :: ErlangFun
erlang__display__1 args = unimplemented "erlang__display__1"
erlang__display__1 [_] = EXC.badarg unit
erlang__display__1 args = EXC.badarity (ErlangFun 1 erlang__display__1) args

erlang__load_module__2 :: ErlangFun
erlang__load_module__2 args = unimplemented "erlang__load_module__2"
erlang__load_module__2 [_,_] = EXC.badarg unit
erlang__load_module__2 args = EXC.badarity (ErlangFun 2 erlang__load_module__2) args

erlang__dt_append_vm_tag_data__1 :: ErlangFun
erlang__dt_append_vm_tag_data__1 args = unimplemented "erlang__dt_append_vm_tag_data__1"
erlang__dt_append_vm_tag_data__1 [_] = EXC.badarg unit
erlang__dt_append_vm_tag_data__1 args = EXC.badarity (ErlangFun 1 erlang__dt_append_vm_tag_data__1) args

erlang__port_info__2 :: ErlangFun
erlang__port_info__2 args = unimplemented "erlang__port_info__2"
erlang__port_info__2 [_,_] = EXC.badarg unit
erlang__port_info__2 args = EXC.badarity (ErlangFun 2 erlang__port_info__2) args

erlang__unregister__1 :: ErlangFun
erlang__unregister__1 args = unimplemented "erlang__unregister__1"
erlang__unregister__1 [_] = EXC.badarg unit
erlang__unregister__1 args = EXC.badarity (ErlangFun 1 erlang__unregister__1) args

erlang__dist_ctrl_input_handler__2 :: ErlangFun
erlang__dist_ctrl_input_handler__2 args = unimplemented "erlang__dist_ctrl_input_handler__2"
erlang__dist_ctrl_input_handler__2 [_,_] = EXC.badarg unit
erlang__dist_ctrl_input_handler__2 args = EXC.badarity (ErlangFun 2 erlang__dist_ctrl_input_handler__2) args

erlang__trunc__1 :: ErlangFun
erlang__trunc__1 [r@(ErlangInt _)] = r
erlang__trunc__1 [ErlangFloat n] | n >= 0.0, DM.Just nn <- DBI.fromNumber (floor n) = ErlangInt nn
erlang__trunc__1 [ErlangFloat n] | DM.Just nn <- DBI.fromNumber (floor (-n)) = ErlangInt (-nn)
erlang__trunc__1 [_] = EXC.badarg unit
erlang__trunc__1 args = EXC.badarity (ErlangFun 1 erlang__trunc__1) args

erlang__crasher__6 :: ErlangFun
erlang__crasher__6 args = unimplemented "erlang__crasher__6"
erlang__crasher__6 [_,_,_,_,_,_] = EXC.badarg unit
erlang__crasher__6 args = EXC.badarity (ErlangFun 6 erlang__crasher__6) args

erlang__read_timer__1 :: ErlangFun
erlang__read_timer__1 args = unimplemented "erlang__read_timer__1"
erlang__read_timer__1 [_] = EXC.badarg unit
erlang__read_timer__1 args = EXC.badarity (ErlangFun 1 erlang__read_timer__1) args

erlang__module_loaded__1 :: ErlangFun
erlang__module_loaded__1 args = unimplemented "erlang__module_loaded__1"
erlang__module_loaded__1 [_] = EXC.badarg unit
erlang__module_loaded__1 args = EXC.badarity (ErlangFun 1 erlang__module_loaded__1) args

erlang__md5_update__2 :: ErlangFun
erlang__md5_update__2 args = unimplemented "erlang__md5_update__2"
erlang__md5_update__2 [_,_] = EXC.badarg unit
erlang__md5_update__2 args = EXC.badarity (ErlangFun 2 erlang__md5_update__2) args

erlang__localtime_to_universaltime__1 :: ErlangFun
erlang__localtime_to_universaltime__1 args = unimplemented "erlang__localtime_to_universaltime__1"
erlang__localtime_to_universaltime__1 [_] = EXC.badarg unit
erlang__localtime_to_universaltime__1 args = EXC.badarity (ErlangFun 1 erlang__localtime_to_universaltime__1) args

erlang__spawn_opt__4 :: ErlangFun
erlang__spawn_opt__4 args = unimplemented "erlang__spawn_opt__4"
erlang__spawn_opt__4 [_,_,_,_] = EXC.badarg unit
erlang__spawn_opt__4 args = EXC.badarity (ErlangFun 4 erlang__spawn_opt__4) args

erlang__group_leader__0 :: ErlangFun
erlang__group_leader__0 args = erlang__self__0 [] -- FIXME

erlang__dt_put_tag__1 :: ErlangFun
erlang__dt_put_tag__1 args = unimplemented "erlang__dt_put_tag__1"
erlang__dt_put_tag__1 [_] = EXC.badarg unit
erlang__dt_put_tag__1 args = EXC.badarity (ErlangFun 1 erlang__dt_put_tag__1) args

erlang__phash2__1 :: ErlangFun
erlang__phash2__1 [a] = erlang__phash2__2 [a, ErlangInt $ DBI.fromInt 268435456]  -- FIXME
erlang__phash2__1 [_] = EXC.badarg unit
erlang__phash2__1 args = EXC.badarity (ErlangFun 1 erlang__phash2__1) args

erlang__phash2__2 :: ErlangFun
erlang__phash2__2 args = erlang__op_minus [erlang__phash__2 args, ErlangInt $ DBI.fromInt 1] -- FIXME
erlang__phash2__2 [_,_] = EXC.badarg unit
erlang__phash2__2 args = EXC.badarity (ErlangFun 2 erlang__phash2__2) args

erlang__spawn__4 :: ErlangFun
erlang__spawn__4 args = unimplemented "erlang__spawn__4"
erlang__spawn__4 [_,_,_,_] = EXC.badarg unit
erlang__spawn__4 args = EXC.badarity (ErlangFun 4 erlang__spawn__4) args

erlang__crc32__1 :: ErlangFun
erlang__crc32__1 args = unimplemented "erlang__crc32__1"
erlang__crc32__1 [_] = EXC.badarg unit
erlang__crc32__1 args = EXC.badarity (ErlangFun 1 erlang__crc32__1) args

erlang__system_monitor__2 :: ErlangFun
erlang__system_monitor__2 args = unimplemented "erlang__system_monitor__2"
erlang__system_monitor__2 [_,_] = EXC.badarg unit
erlang__system_monitor__2 args = EXC.badarity (ErlangFun 2 erlang__system_monitor__2) args

erlang__spawn_monitor__3 :: ErlangFun
erlang__spawn_monitor__3 args = unimplemented "erlang__spawn_monitor__3"
erlang__spawn_monitor__3 [_,_,_] = EXC.badarg unit
erlang__spawn_monitor__3 args = EXC.badarity (ErlangFun 3 erlang__spawn_monitor__3) args

erlang__demonitor__2 :: ErlangFun
erlang__demonitor__2 args = unimplemented "erlang__demonitor__2"
erlang__demonitor__2 [_,_] = EXC.badarg unit
erlang__demonitor__2 args = EXC.badarity (ErlangFun 2 erlang__demonitor__2) args

erlang__raise__3 :: ErlangFun
erlang__raise__3 args = unimplemented "erlang__raise__3"
erlang__raise__3 [_,_,_] = EXC.badarg unit
erlang__raise__3 args = EXC.badarity (ErlangFun 3 erlang__raise__3) args

erlang__hibernate__3 :: ErlangFun
erlang__hibernate__3 args = unimplemented "erlang__hibernate__3"
erlang__hibernate__3 [_,_,_] = EXC.badarg unit
erlang__hibernate__3 args = EXC.badarity (ErlangFun 3 erlang__hibernate__3) args

erlang__adler32_combine__3 :: ErlangFun
erlang__adler32_combine__3 args = unimplemented "erlang__adler32_combine__3"
erlang__adler32_combine__3 [_,_,_] = EXC.badarg unit
erlang__adler32_combine__3 args = EXC.badarity (ErlangFun 3 erlang__adler32_combine__3) args

erlang__node__1 :: ErlangFun
erlang__node__1 args = unimplemented "erlang__node__1"
erlang__node__1 [_] = EXC.badarg unit
erlang__node__1 args = EXC.badarity (ErlangFun 1 erlang__node__1) args

erlang__monitor__2 :: ErlangFun
erlang__monitor__2 args = unimplemented "erlang__monitor__2"
erlang__monitor__2 [_,_] = EXC.badarg unit
erlang__monitor__2 args = EXC.badarity (ErlangFun 2 erlang__monitor__2) args

erlang__start_timer__4 :: ErlangFun
erlang__start_timer__4 args = unimplemented "erlang__start_timer__4"
erlang__start_timer__4 [_,_,_,_] = EXC.badarg unit
erlang__start_timer__4 args = EXC.badarity (ErlangFun 4 erlang__start_timer__4) args

erlang__bit_size__1 :: ErlangFun
erlang__bit_size__1 args = unimplemented "erlang__bit_size__1"
erlang__bit_size__1 [_] = EXC.badarg unit
erlang__bit_size__1 args = EXC.badarity (ErlangFun 1 erlang__bit_size__1) args

erlang__call_on_load_function__1 :: ErlangFun
erlang__call_on_load_function__1 args = unimplemented "erlang__call_on_load_function__1"
erlang__call_on_load_function__1 [_] = EXC.badarg unit
erlang__call_on_load_function__1 args = EXC.badarity (ErlangFun 1 erlang__call_on_load_function__1) args

erlang__processes__0 :: ErlangFun
erlang__processes__0 args = unimplemented "erlang__processes__0"

erlang__error__2 :: ErlangFun
erlang__error__2 [err, _] = erlang__error__1 [err]
erlang__error__2 [_,_] = EXC.badarg unit
erlang__error__2 args = EXC.badarity (ErlangFun 2 erlang__error__2) args

erlang__loaded__0 :: ErlangFun
erlang__loaded__0 args = unimplemented "erlang__loaded__0"

erlang__bump_reductions__1 :: ErlangFun
erlang__bump_reductions__1 args = unimplemented "erlang__bump_reductions__1"
erlang__bump_reductions__1 [_] = EXC.badarg unit
erlang__bump_reductions__1 args = EXC.badarity (ErlangFun 1 erlang__bump_reductions__1) args

erlang__make_tuple__3 :: ErlangFun
erlang__make_tuple__3 [ErlangInt barity, def, list]
  | DM.Just arity <- H.bigIntToInt barity, arity >= 0 =
  let processList :: ErlangTerm -> Map.Map Int ErlangTerm -> Map.Map Int ErlangTerm
      processList ErlangEmptyList acc = acc
      processList (ErlangCons (ErlangTuple [(ErlangInt bidx), val]) rest) acc
        | DM.Just idx <- H.bigIntToInt bidx, idx > 0, idx <= arity =
          processList rest (Map.insert idx val acc)
      processList _ _ = EXC.badarg unit

      idxMap = processList list Map.empty

      buildTuple 0 acc =
        ErlangTuple $ DA.fromFoldable acc
      buildTuple n acc =
        buildTuple (n - 1) (DL.Cons (DM.fromMaybe def (Map.lookup n idxMap)) acc)
  in buildTuple arity DL.Nil
erlang__make_tuple__3 [_,_,_] = EXC.badarg unit
erlang__make_tuple__3 args = EXC.badarity (ErlangFun 3 erlang__make_tuple__3) args

erlang__unlink__1 :: ErlangFun
erlang__unlink__1 args = unimplemented "erlang__unlink__1"
erlang__unlink__1 [_] = EXC.badarg unit
erlang__unlink__1 args = EXC.badarity (ErlangFun 1 erlang__unlink__1) args

erlang__demonitor__1 :: ErlangFun
erlang__demonitor__1 args = unimplemented "erlang__demonitor__1"
erlang__demonitor__1 [_] = EXC.badarg unit
erlang__demonitor__1 args = EXC.badarity (ErlangFun 1 erlang__demonitor__1) args

erlang__trace_info__2 :: ErlangFun
erlang__trace_info__2 args = unimplemented "erlang__trace_info__2"
erlang__trace_info__2 [_,_] = EXC.badarg unit
erlang__trace_info__2 args = EXC.badarity (ErlangFun 2 erlang__trace_info__2) args

erlang__delete_module__1 :: ErlangFun
erlang__delete_module__1 args = unimplemented "erlang__delete_module__1"
erlang__delete_module__1 [_] = EXC.badarg unit
erlang__delete_module__1 args = EXC.badarity (ErlangFun 1 erlang__delete_module__1) args

erlang__garbage_collect__1 :: ErlangFun
erlang__garbage_collect__1 args = unimplemented "erlang__garbage_collect__1"
erlang__garbage_collect__1 [_] = EXC.badarg unit
erlang__garbage_collect__1 args = EXC.badarity (ErlangFun 1 erlang__garbage_collect__1) args

erlang__check_old_code__1 :: ErlangFun
erlang__check_old_code__1 args = unimplemented "erlang__check_old_code__1"
erlang__check_old_code__1 [_] = EXC.badarg unit
erlang__check_old_code__1 args = EXC.badarity (ErlangFun 1 erlang__check_old_code__1) args

erlang__spawn_monitor__1 :: ErlangFun
erlang__spawn_monitor__1 args = unimplemented "erlang__spawn_monitor__1"
erlang__spawn_monitor__1 [_] = EXC.badarg unit
erlang__spawn_monitor__1 args = EXC.badarity (ErlangFun 1 erlang__spawn_monitor__1) args

erlang__system_profile__2 :: ErlangFun
erlang__system_profile__2 args = unimplemented "erlang__system_profile__2"
erlang__system_profile__2 [_,_] = EXC.badarg unit
erlang__system_profile__2 args = EXC.badarity (ErlangFun 2 erlang__system_profile__2) args

erlang__error__1 :: ErlangFun
erlang__error__1 [arg] = EXC.error arg
erlang__error__1 [_] = EXC.badarg unit
erlang__error__1 args = EXC.badarity (ErlangFun 1 erlang__error__1) args

erlang__delay_trap__2 :: ErlangFun
erlang__delay_trap__2 args = unimplemented "erlang__delay_trap__2"
erlang__delay_trap__2 [_,_] = EXC.badarg unit
erlang__delay_trap__2 args = EXC.badarity (ErlangFun 2 erlang__delay_trap__2) args

erlang__spawn_link__4 :: ErlangFun
erlang__spawn_link__4 args = unimplemented "erlang__spawn_link__4"
erlang__spawn_link__4 [_,_,_,_] = EXC.badarg unit
erlang__spawn_link__4 args = EXC.badarity (ErlangFun 4 erlang__spawn_link__4) args

erlang__memory__0 :: ErlangFun
erlang__memory__0 args = unimplemented "erlang__memory__0"

erlang__halt__0 :: ErlangFun
erlang__halt__0 args = unimplemented "erlang__halt__0"

erlang__process_flag__3 :: ErlangFun
erlang__process_flag__3 args = unimplemented "erlang__process_flag__3"
erlang__process_flag__3 [_,_,_] = EXC.badarg unit
erlang__process_flag__3 args = EXC.badarity (ErlangFun 3 erlang__process_flag__3) args

erlang__set_cpu_topology__1 :: ErlangFun
erlang__set_cpu_topology__1 args = unimplemented "erlang__set_cpu_topology__1"
erlang__set_cpu_topology__1 [_] = EXC.badarg unit
erlang__set_cpu_topology__1 args = EXC.badarity (ErlangFun 1 erlang__set_cpu_topology__1) args

erlang__universaltime__0 :: ErlangFun
erlang__universaltime__0 args = unimplemented "erlang__universaltime__0"


--------------------------------------------------------------------------------

code__ensure_loaded__1 :: ErlangFun
code__ensure_loaded__1 [ErlangAtom mName] = do_ensure_loaded mName
code__ensure_loaded__1 [_] = EXC.function_clause unit
code__ensure_loaded__1 args = EXC.badarity (ErlangFun 1 code__ensure_loaded__1) args

--------------------------------------------------------------------------------

binary__decode_unsigned__1 :: ErlangFun
binary__decode_unsigned__1 [ErlangBinary buf] =
  ErlangInt $ BIN.decode_unsigned_big buf
binary__decode_unsigned__1 [_, _] = EXC.badarg unit
binary__decode_unsigned__1 args = EXC.badarity (ErlangFun 1 binary__decode_unsigned__1) args

binary__decode_unsigned__2 :: ErlangFun
binary__decode_unsigned__2 [ErlangBinary buf, ErlangAtom "little"] =
  ErlangInt $ BIN.decode_unsigned_little buf
binary__decode_unsigned__2 [ErlangBinary buf, ErlangAtom "big"] =
  ErlangInt $ BIN.decode_unsigned_big buf
binary__decode_unsigned__2 [_, _] = EXC.badarg unit
binary__decode_unsigned__2 args = EXC.badarity (ErlangFun 2 binary__decode_unsigned__2) args

binary__encode_unsigned__1 :: ErlangFun
binary__encode_unsigned__1 [i@(ErlangInt _)] =
  binary__encode_unsigned__2 [i, ErlangAtom "big"]
binary__encode_unsigned__1 [_] = EXC.badarg unit
binary__encode_unsigned__1 args = EXC.badarity (ErlangFun 1 binary__encode_unsigned__1) args

binary__encode_unsigned__2 :: ErlangFun
binary__encode_unsigned__2 [ErlangInt i, ErlangAtom "little"] =
  ErlangBinary $ BIN.from_int_bound i DM.Nothing 8 BIN.Little
binary__encode_unsigned__2 [ErlangInt i, ErlangAtom "big"] =
  ErlangBinary $ BIN.from_int_bound i DM.Nothing 8 BIN.Big
binary__encode_unsigned__2 [_, _] = EXC.badarg unit
binary__encode_unsigned__2 args = EXC.badarity (ErlangFun 2 binary__encode_unsigned__2) args

binary__split__2 :: ErlangFun
binary__split__2 [bin, pat] = binary__split__3 [bin, pat, ErlangEmptyList]
binary__split__2 args = EXC.badarity (ErlangFun 2 binary__split__2) args

binary__split__3 :: ErlangFun
binary__split__3 [bin, pat@(ErlangBinary _), opts] =
  binary__split__3 [bin, ErlangCons pat ErlangEmptyList, opts]
binary__split__3 [ErlangBinary buf, epat, eopts]
  | DM.Just pat <- erlangListToList epat
  , DM.Just opts <- erlangListToList eopts
  = BIN.split buf patterns options where
    patterns = map (\p -> case p of
                       ErlangBinary b | BIN.rawSize b /= 0 -> b
                       _ -> EXC.badarg unit
                       ) pat
    options = map (\o -> case o of
                      ErlangAtom "trim" -> BIN.Trim
                      ErlangAtom "trim_all" -> BIN.TrimAll
                      ErlangAtom "global" -> BIN.Global
                      ErlangTuple [ErlangAtom "scope", ErlangTuple [ErlangInt bstart, ErlangInt blen]]
                          | DM.Just start <- H.bigIntToInt bstart
                          , DM.Just len <- H.bigIntToInt blen
                          , start >= 0
                            -> BIN.Scope start len
                      _ -> EXC.badarg unit
                       ) opts
binary__split__3 [_, _, _] = EXC.badarg unit
binary__split__3 args = EXC.badarity (ErlangFun 3 binary__split__3) args
