module Erlang.Binary where

import Prelude
import Erlang.Type
import Erlang.Helpers as H
import Erlang.Exception as EXC
import Node.Buffer as Buffer
import Node.Buffer(Buffer)
import Node.Encoding
import Data.BigInt as BI
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw, throwException)
import Data.UInt (UInt, toInt, fromInt)
import Data.Array.NonEmpty as NonEmpty
import Partial.Unsafe (unsafePartial)
import Data.Array as DA
import Data.Maybe as DM
import Data.Tuple as DT
import Data.List as DL
import Data.Int as Int
import Data.BigInt as DBI
import Data.Maybe(Maybe, fromJust)
import Data.Foldable

data Endian = Big | Little
data Sign   = Signed | Unsigned
data BinResult = Nah | Ok ErlangTerm Buffer.Buffer

fromFoldable :: forall f. Foldable f => f Int -> Buffer
fromFoldable f = unsafePerformEffect (Buffer.fromArray (DA.fromFoldable f))

buffer :: ErlangTerm -> Buffer
buffer (ErlangBinary buf) = buf
buffer _ = EXC.badarg unit

concat :: Array Buffer -> Buffer
concat args = unsafePerformEffect $ Buffer.concat args

concat_erl :: ErlangTerm -> ErlangTerm
concat_erl listTerm =
  case erlangListToList listTerm of
    DM.Nothing -> EXC.bad_generator listTerm
    DM.Just l -> ErlangBinary $ concat (DA.fromFoldable $ map buffer l)

empty :: Buffer -> Boolean
empty buf = unsafePerformEffect $ map (_ == 0) (Buffer.size buf)

toArray :: Buffer -> Array Int
toArray = unsafePerformEffect <<< Buffer.toArray

rawSize :: Buffer -> Int
rawSize = unsafePerformEffect <<< Buffer.size

size :: Buffer -> ErlangTerm
size = ErlangInt <<< DBI.fromInt <<< unsafePerformEffect <<< Buffer.size

packed_size :: ErlangTerm -> ErlangTerm
packed_size (ErlangBinary b) = size b
packed_size _ = EXC.badarg unit

chop_int :: Buffer.Buffer -> DBI.BigInt -> Int -> Endian -> Sign -> BinResult
chop_int buf bsize unit endian sign
  | DM.Just size <- H.bigIntToInt bsize
    = unsafePerformEffect $ do
  let chopSize = (size * unit) / 8
  size <- Buffer.size buf
  if size < chopSize
    then pure Nah
    else do
    let chop = Buffer.slice 0 chopSize buf
        rest = Buffer.slice chopSize size buf
        nonSign = case endian of
          Big    ->  (decode_unsigned_big chop)
          Little ->  (decode_unsigned_little chop)
        regSign = case sign of
          Unsigned -> nonSign
          Signed ->
            let p = DBI.pow (DBI.fromInt 2) (DBI.fromInt $ chopSize * 8 - 1)
            in if nonSign < p then nonSign else nonSign - p * (DBI.fromInt 2)
    pure $ Ok (ErlangInt regSign) rest
chop_int _ _ _ _ _ = EXC.badarg unit

chop_bin :: Buffer.Buffer -> DBI.BigInt -> Int -> BinResult
chop_bin buf bsize unit
  | DM.Just size <- H.bigIntToInt bsize
    = unsafePerformEffect $ do
  let chopSize = (size * unit) / 8
  size <- Buffer.size buf
  if size < chopSize
    then pure Nah
    else do
    let chop = Buffer.slice 0 chopSize buf
        rest = Buffer.slice chopSize size buf
    pure $ Ok (ErlangBinary chop) rest
chop_bin _ _ _ = EXC.badarg unit

foreign import arrayToFloat32 :: Array Int -> Number
foreign import arrayToFloat64 :: Array Int -> Number
chop_float :: Buffer.Buffer -> DBI.BigInt -> Int -> Endian -> BinResult
chop_float buf bsize unit endian
  | DM.Just size <- H.bigIntToInt bsize
    = unsafePerformEffect $ do
  bufSize <- Buffer.size buf
  let chopSize = (size * unit) / 8
  if chopSize == 8 || chopSize == 4
    then do
      let chop = Buffer.slice 0 chopSize buf
          rest = Buffer.slice chopSize bufSize buf
      trueChop <- case endian of
        Big -> Buffer.toArray chop
        Little -> do
          asArr <- Buffer.toArray chop
          pure (DA.reverse asArr)
      pure $ Ok (if chopSize == 8
                 then ErlangFloat (arrayToFloat64 trueChop)
                 else ErlangFloat (arrayToFloat32 trueChop)
                ) rest
    else pure Nah
chop_float _ _ _ _ = EXC.badarg unit

unsafe_at :: Buffer -> Int -> Int
unsafe_at buf n = unsafePartial $ fromJust $ unsafePerformEffect $ (Buffer.getAtOffset n buf)

decode_unsigned_big :: Buffer -> DBI.BigInt
decode_unsigned_big buf = unsafePerformEffect (Buffer.size buf >>= go buf (DBI.fromInt 0)) where
  go :: Buffer -> DBI.BigInt -> Int -> Effect DBI.BigInt
  go buf acc size = do
    case size of
      0 -> pure acc
      _ -> go
           (Buffer.slice 1 size buf)
           ((DBI.fromInt 256) * acc + DBI.fromInt (unsafe_at buf 0))
           (size - 1)

decode_unsigned_little :: Buffer -> DBI.BigInt
decode_unsigned_little buf = unsafePerformEffect (Buffer.size buf >>= go buf (DBI.fromInt 0)) where
  go :: Buffer -> DBI.BigInt -> Int -> Effect DBI.BigInt
  go buf acc size = do
    case size of
      0 -> pure acc
      _ -> go
           (Buffer.slice 0 (size - 1) buf)
           ((DBI.fromInt 256) * acc + DBI.fromInt (unsafe_at buf (size - 1)))
           (size - 1)

from_int :: ErlangTerm -> ErlangTerm -> Int -> Endian -> Buffer
from_int (ErlangInt n) (ErlangInt size) unit endian =
  let bufSize = (size * DBI.fromInt unit) / DBI.fromInt 8
      build x num acc =
        if x == DBI.fromInt 0
        then acc
        else build (x - DBI.fromInt 1) (num / DBI.fromInt 256)
             (DL.Cons (unsafePartial $ DM.fromJust $ H.bigIntToInt $ num `mod` DBI.fromInt 256) acc)
      big = build bufSize n DL.Nil
  in fromFoldable $
    case endian of
      Big -> big
      Little -> DL.reverse big
from_int _ _ _ _ = EXC.badarg unit

foreign import float32ToArray :: Number -> Array Int
foreign import float64ToArray :: Number -> Array Int
from_float :: ErlangTerm -> ErlangTerm -> Int -> Endian -> Buffer
from_float _ (ErlangInt bsize) unit_ _
  | DM.Just size <- H.bigIntToInt bsize
  , size * unit_ /= 32 && size * unit_ /= 64 = EXC.badarg unit
from_float (ErlangInt i) s u e =
  from_float (ErlangFloat (DBI.toNumber i)) s u e
from_float (ErlangFloat f) (ErlangInt bsize) unit_ endian
  | DM.Just size <- H.bigIntToInt bsize =
  let big = case size * unit_ of
        32 -> float32ToArray f
        _  -> float64ToArray f
  in fromFoldable $ case endian of
    Little -> DA.reverse big
    Big -> big
from_float _ _ _ _ = EXC.badarg unit

format_bin :: ErlangTerm -> ErlangTerm -> Int -> Buffer
format_bin (ErlangBinary buf) (ErlangInt bsize) unit
  | DM.Just size <- H.bigIntToInt bsize =
  let bufSize = size * unit / 8
  in Buffer.slice 0 bufSize buf
format_bin _ _ _ = EXC.badarg unit

to_erlang_list_from_to :: Buffer -> Int -> Int -> ErlangTerm
to_erlang_list_from_to buf from to =
  if from < 1 || to > unsafePerformEffect (Buffer.size buf)
  then EXC.badarg unit
  else to_erlang_list (Buffer.slice (from - 1) (to - 1) buf)

to_erlang_list :: Buffer -> ErlangTerm
to_erlang_list =
  arrayToErlangList
  <<< map (ErlangInt <<< DBI.fromInt)
  <<< unsafePerformEffect
  <<< Buffer.toArray

data SplitOpt = Global | Trim | TrimAll | Scope Int Int
derive instance splitOptEq :: Eq SplitOpt

split :: forall f. Foldable f
      => Buffer -> f Buffer -> f SplitOpt -> ErlangTerm
split buf pats opts =
  let trim    = elem Trim    opts
      trimAll = elem TrimAll opts
      global  = elem Global  opts
      DT.Tuple start len =
        DM.fromMaybe (DT.Tuple 0 (rawSize buf))
        $ findMap (\o -> case o of
                      Scope s l -> DM.Just $ DT.Tuple s l
                      _ -> DM.Nothing) opts

      go :: DL.List Buffer -> Int -> Int -> DL.List Buffer
      go acc last n =
        if n >= start + len  -- If we reached the end
        then case acc of
             DL.Nil ->  -- If we didn't find anything then split at all
               DL.Cons buf DL.Nil
             DL.Cons lastCut rest ->  -- Add right side to last result
               DL.reverse $ DL.Cons
                              (Buffer.slice
                               (last - rawSize lastCut) -- Rollback to lastCut's beginning
                               (rawSize buf) -- Take everything
                               buf)
                              acc
        else  -- Check if some pattern starts here. TODO: KNP
          case find (\pat ->
                      rawSize pat <= (start + len) - n &&  -- It fits...
                      toArray (Buffer.slice n (rawSize pat) buf) == toArray pat  -- It matches
                    ) pats of
            -- If not then we try further
            DM.Nothing -> go acc last (n + 1)
            -- If some `pat` matches
            DM.Just pat ->
              let cut = if last == start  -- If we didn't find anything yet
                         then  -- Include left side
                           Buffer.slice 0 n buf
                         else  -- Cut from last to here ("here" exclusive)
                           Buffer.slice last (n - last) buf
              in if global
                 then  -- Proceed from behind the pat
                   go (DL.Cons cut acc) n (n + rawSize pat)
                 else  -- Force finalization
                   go (DL.Cons cut acc) n len

      splitted = go DL.Nil start start

      list =
        if trimAll
        then DL.filter (\b -> rawSize b > 0) splitted
        else if trim
        then DL.reverse $ DL.dropWhile (\b -> rawSize b == 0) $ DL.reverse $ splitted
        else splitted
  in DL.foldr (\e acc -> ErlangCons (ErlangBinary e) acc) ErlangEmptyList list
