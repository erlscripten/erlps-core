-- | Module for handling binaries (implemented as nodejs buffers).
-- | The stored values should be unsigned integers lesser than 256.
-- | Binary size is restricted to fit in PureScript's Int range
module Erlang.Binary where

import Prelude

import Data.Array as DA
import Data.BigInt as DBI
import Data.Foldable (class Foldable, elem, find, findMap)
import Data.List as DL
import Data.Maybe as DM
import Data.Tuple as DT
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erlang.Exception as EXC
import Erlang.Type (ErlangTerm(..), fromErl, toErl)
import Erlang.Utils as Util
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Prelude as Prelude

data Endian = Big | Little
data Sign   = Signed | Unsigned

-- | Stateful binary operation that may chop off values out of binary
data BinResult = Nah | Ok ErlangTerm Buffer.Buffer

fromFoldable :: forall f. Foldable f => f Int -> Buffer
fromFoldable f = unsafePerformEffect $
                 Buffer.fromArray $
                 map (_ `mod` 256) $
                 DA.fromFoldable f

-- | Forcefully get the buffer. Throws `badarg` on failure
buffer :: ErlangTerm -> Buffer
buffer (ErlangBinary buf) = buf
buffer _ = EXC.badarg unit

-- | Concatenates an array of buffers
concat :: Array Buffer -> Buffer
concat args = unsafePerformEffect $ Buffer.concat args

-- | Concatenates an Erlang list of buffers
concatErl :: ErlangTerm -> ErlangTerm
concatErl listTerm =
  case fromErl listTerm of
    DM.Nothing -> EXC.bad_generator listTerm
    DM.Just l -> ErlangBinary $ concat (map buffer l)

-- | Checks if buffer is empty
empty :: Buffer -> Boolean
empty buf = unsafePerformEffect $ map (_ == 0) (Buffer.size buf)

-- | Converts to array of integers
toArray :: Buffer -> Array Int
toArray = unsafePerformEffect <<< Buffer.toArray

-- | Returns size as raw integer
rawSize :: Buffer -> Int
rawSize = unsafePerformEffect <<< Buffer.size

-- | Returns size as an ErlangTerm
size :: Buffer -> ErlangTerm
size = ErlangInt <<< DBI.fromInt <<< unsafePerformEffect <<< Buffer.size

-- | Calculates size of an Erlang binary or throws `badarg` if
-- | something else was provided
packedSize :: ErlangTerm -> ErlangTerm
packedSize (ErlangBinary b) = size b
packedSize _ = EXC.badarg unit

-- | Chops off an integer of given size and unit from a buffer
chopInt :: Buffer.Buffer -> DBI.BigInt -> Int -> Endian -> Sign -> BinResult
chopInt buf bsize unit endian sign
  | DM.Just isize <- Util.bigIntToInt bsize
    = unsafePerformEffect $ do
  let chopSize = (isize * unit) / 8
  bufsize <- Buffer.size buf
  if bufsize < chopSize
    then pure Nah
    else do
    let chop = Buffer.slice 0 chopSize buf
        rest = Buffer.slice chopSize bufsize buf
        nonSign = case endian of
          Big    ->  (decodeUnsignedBig chop)
          Little ->  (decodeUnsignedLittle chop)
        regSign = case sign of
          Unsigned -> nonSign
          Signed ->
            let p = DBI.pow (DBI.fromInt 2) (DBI.fromInt $ chopSize * 8 - 1)
            in if nonSign < p then nonSign else nonSign - p * (DBI.fromInt 2)
    pure $ Ok (ErlangInt regSign) rest
chopInt _ _ _ _ _ = EXC.badarg unit

-- | Chops off a binary of a given size from a buffer
chopBin :: Buffer.Buffer -> DBI.BigInt -> Int -> BinResult
chopBin buf bsize unit
  | DM.Just isize <- Util.bigIntToInt bsize
    = unsafePerformEffect $ do
  let chopSize = (isize * unit) / 8
  bufsize <- Buffer.size buf
  if bufsize < chopSize
    then pure Nah
    else do
    let chop = Buffer.slice 0 chopSize buf
        rest = Buffer.slice chopSize bufsize buf
    pure $ Ok (ErlangBinary chop) rest
chopBin _ _ _ = EXC.badarg unit

foreign import arrayToFloat32 :: Array Int -> Number
foreign import arrayToFloat64 :: Array Int -> Number

-- | Chops off a float of a given size from a buffer
chopFloat :: Buffer.Buffer -> DBI.BigInt -> Int -> Endian -> BinResult
chopFloat buf bsize unit endian
  | DM.Just isize <- Util.bigIntToInt bsize
    = unsafePerformEffect $ do
  bufSize <- Buffer.size buf
  let chopSize = (isize * unit) / 8
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
chopFloat _ _ _ _ = EXC.badarg unit

-- | Forcefully checks a value at given index. Throws error on miss
unsafeAt :: Buffer -> Int -> Int
unsafeAt buf n = case unsafePerformEffect $ (Buffer.getAtOffset n buf) of
  DM.Just i -> i
  DM.Nothing -> Util.runtimeError $ "BIN.unsafeAt: bad idx " <> show n

-- | Decodes unsigned integer in big-endian
decodeUnsignedBig :: Buffer -> DBI.BigInt
decodeUnsignedBig buf = unsafePerformEffect (Buffer.size buf >>= go buf (DBI.fromInt 0)) where
  go :: Buffer -> DBI.BigInt -> Int -> Effect DBI.BigInt
  go b acc bufsize = do
    case bufsize of
      0 -> pure acc
      _ -> go
           (Buffer.slice 1 bufsize b)
           ((DBI.fromInt 256) * acc + DBI.fromInt (unsafeAt b 0))
           (bufsize - 1)

-- | Decodes unsigned integer in little-endian
decodeUnsignedLittle :: Buffer -> DBI.BigInt
decodeUnsignedLittle buf = unsafePerformEffect (Buffer.size buf >>= go buf (DBI.fromInt 0)) where
  go :: Buffer -> DBI.BigInt -> Int -> Effect DBI.BigInt
  go b acc bufsize = do
    case bufsize of
      0 -> pure acc
      _ -> go
           (Buffer.slice 0 (bufsize - 1) b)
           ((DBI.fromInt 256) * acc + DBI.fromInt (unsafeAt b (bufsize - 1)))
           (bufsize - 1)

-- | Encodes an integer into a buffer of a given size. Overflows cause
-- | "flip" of the value
fromInt :: ErlangTerm -> ErlangTerm -> Int -> Endian -> Buffer
fromInt (ErlangInt n) (ErlangInt bsize) unit endian =
  fromIntBound n (DM.Just bsize) unit endian
fromInt _ _ _ _ = EXC.badarg unit

-- | Encodes an integer into a buffer of an optionally given size. Overflows cause
-- | "flip" of the value. The size is unpacked.
fromIntBound :: DBI.BigInt -> DM.Maybe DBI.BigInt -> Int -> Endian -> Buffer
fromIntBound n msize unit endian =
  let bufSize = map ((_ * DBI.fromInt unit) >>> (_ / DBI.fromInt 8)) msize
      build x num acc =
        if DM.maybe (num == DBI.fromInt 0) (\isize -> isize == DBI.fromInt 0) x
        then acc
        else build (map (_ - DBI.fromInt 1) x) (num / DBI.fromInt 256)
             (DL.Cons (case Util.bigIntToInt $ num `mod` DBI.fromInt 256 of
                          DM.Just xn -> xn
                          DM.Nothing -> EXC.badarg Prelude.unit
                      ) acc)
      big = build bufSize n DL.Nil
  in fromFoldable $
    case endian of
      Big -> big
      Little -> DL.reverse big

fromInts :: ErlangTerm -> ErlangTerm -> Int -> Endian -> Buffer
fromInts elist bsize unit endian
  | DM.Just (l :: Array ErlangTerm) <- fromErl elist
  = concat
    $ map (\e -> fromInt e bsize unit endian)
    $ l
fromInts _ _ _ _ = EXC.badarg unit

foreign import float32ToArray :: Number -> Array Int
foreign import float64ToArray :: Number -> Array Int

-- | Encodes float value into a buffer
fromFloat :: ErlangTerm -> ErlangTerm -> Int -> Endian -> Buffer
fromFloat _ (ErlangInt bsize) unit_ _
  | DM.Just isize <- Util.bigIntToInt bsize
  , isize * unit_ /= 32 && isize * unit_ /= 64 = EXC.badarg unit
fromFloat (ErlangInt i) s u e =
  fromFloat (ErlangFloat (DBI.toNumber i)) s u e
fromFloat (ErlangFloat f) (ErlangInt bsize) unit_ endian
  | DM.Just isize <- Util.bigIntToInt bsize =
  let big = case isize * unit_ of
        32 -> float32ToArray f
        _  -> float64ToArray f
  in fromFoldable $ case endian of
    Little -> DA.reverse big
    Big -> big
fromFloat _ _ _ _ = EXC.badarg unit

-- | Slices binary from the beginning
binPrefix :: ErlangTerm -> ErlangTerm -> Int -> Buffer
binPrefix (ErlangBinary buf) (ErlangInt bsize) unit
  | DM.Just isize <- Util.bigIntToInt bsize =
  let bufSize = isize * unit / 8
  in Buffer.slice 0 bufSize buf
binPrefix _ _ _ = EXC.badarg unit

-- | Converts a buffer into erlang list of ints
toErlangList :: Buffer -> ErlangTerm
toErlangList =
  toErl
  <<< map (ErlangInt <<< DBI.fromInt)
  <<< unsafePerformEffect
  <<< Buffer.toArray

-- | Converts a given range of buffer into erlang list of ints
toErlangListFromTo :: Buffer -> Int -> Int -> ErlangTerm
toErlangListFromTo buf from to =
  if from < 1 || to > unsafePerformEffect (Buffer.size buf)
  then EXC.badarg unit
  else toErlangList (Buffer.slice (from - 1) (to - 1) buf)

data SplitOpt = Global | Trim | TrimAll | Scope Int Int
derive instance splitOptEq :: Eq SplitOpt

-- | Splits buffer using given set of patterns into a list of binaries.
-- | Implements binary:split from Erlang stdlib.
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
             DL.Cons _lastCut rest ->  -- Add right side to last result
               DL.reverse $ DL.Cons
                              (Buffer.slice last
                               (rawSize buf) -- Take everything
                               buf)
                              acc
        else  -- Check if some pattern starts here. TODO: KNP
          case find (\pat ->
                      rawSize pat <= (start + len) - n &&  -- It fits...
                      toArray (Buffer.slice n (n + rawSize pat) buf) == toArray pat  -- It matches...
                    ) pats of
            -- If not then we try further
            DM.Nothing -> go acc last (n + 1)
            -- If some `pat` matches
            DM.Just pat ->
              let cut = if last == start  -- If we didn't find anything yet
                         then  -- Include left side
                           Buffer.slice 0 n buf
                         else  -- Cut from last to here ("here" exclusive)
                           Buffer.slice last n buf
              in if global
                 then  -- Proceed from behind the pat
                   go (DL.Cons cut acc) (n + rawSize pat) (n + rawSize pat)
                 else  -- Force finalization
                   go (DL.Cons cut acc) (n + rawSize pat) (start + len)

      splitted = go DL.Nil start start

      list =
        if trimAll
        then DL.filter (\b -> rawSize b > 0) splitted
        else if trim
        then DL.reverse $ DL.dropWhile (\b -> rawSize b == 0) $ DL.reverse $ splitted
        else splitted
  in DL.foldr (\e acc -> ErlangCons (ErlangBinary e) acc) ErlangEmptyList list
