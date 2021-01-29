-- | Main Erlang term definitions and utilities
module Erlang.Type
       ( ErlangFun
       , ErlangTerm(..)
       , strongEq, strongNEq, strongCmp, strongGeq, strongGt, strongLeq, strongLt
       , weakEq, weakNEq, weakCmp, weakGeq, weakGt, weakLeq, weakLt
       , WeakErlangTerm
       , unpackWeak
       , class ToErlang, class FromErlang
       , toErl, fromErl
       , nil, cons, tup, bin
       , isEList, isEInt, isENum, isEFloat, isEAtom, isEPID
       , isEBinary, isETuple, isEFun, isEFunA, isEMap, isEReference
       ) where

import Prelude

import Data.Array as DA
import Data.BigInt as DBI
import Data.Char as DC
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.List as DL
import Data.Map as Map
import Data.Maybe as DM
import Data.String as DS
import Data.String.CodePoints as DSCP
import Data.Traversable (traverse)
import Data.Tuple as DT
import Data.Unfoldable (class Unfoldable)
import Effect.Unsafe (unsafePerformEffect)
import Erlang.Utils (bigIntToInt, codePointToInt)
import Node.Buffer (Buffer, toArray, fromArray)
import Unsafe.Coerce (unsafeCoerce)

-- | Type of an Erlang function. Needs to be uncurried in order
-- to support arbitrary arities and catch `badarity` errors properly
type ErlangFun = Array ErlangTerm -> ErlangTerm

-- | Data representing an Erlang Term
data ErlangTerm
    = ErlangInt       DBI.BigInt
    | ErlangFloat     Number
    | ErlangAtom      String
    | ErlangCons      ErlangTerm ErlangTerm
    | ErlangEmptyList
    | ErlangBinary    Buffer
    | ErlangTuple     (Array ErlangTerm)
    | ErlangFun       Int ErlangFun
    | ErlangMap       (Map.Map ErlangTerm ErlangTerm)
    | ErlangReference Int
    | ErlangPID       Int

---- INSTANCES ----

-- | Converts an array to string with custom opener, delimiter, closer
-- and element display
foreign import showArrayImplGeneral
  :: forall a. String -> String -> String -> (a -> String) -> Array a -> String

instance showErlangTerm :: Show ErlangTerm where
    show ErlangEmptyList =
        "[]"
    show term
      | DM.Just (l :: Array DSCP.CodePoint) <- fromErl term >>=
          traverse (\t -> case t of
                      ErlangInt bi -> Int.fromNumber (DBI.toNumber bi) >>=
                         \i -> if (i >= 32 && i <= 126) || (i >= 8 && i <= 12)
                               then map DSCP.codePointFromChar $ DC.fromCharCode i else DM.Nothing
                      _ -> DM.Nothing
                   )
            = show $ DS.fromCodePointArray $ DA.fromFoldable l
    show (ErlangInt a) =
        DBI.toString a
    show (ErlangFloat a) =
        show a
    show term  | DM.Just (l :: Array ErlangTerm) <- fromErl term =
        show l
    show (ErlangCons h t) =
        "[" <> show h <> "|" <> show t <> "]"
    show (ErlangBinary a)
      | DM.Just l <-
          traverse (\i -> if (i >= 32 && i <= 126) || (i >= 8 && i <= 12)
                          then map DSCP.codePointFromChar $ DC.fromCharCode i
                          else DM.Nothing
                   ) (unsafePerformEffect (toArray a))
            = "<<" <> show (DS.fromCodePointArray l) <> ">>"
    show (ErlangBinary a) =
        showArrayImplGeneral "<<" ">>" "," show (unsafePerformEffect $ toArray a)
    show (ErlangTuple a) =
        showArrayImplGeneral "{" "}" "," show a
    show (ErlangFun arity _) =
        "<some_function/" <> show arity <> ">"
    show (ErlangAtom atom) =
        if DA.any
             (\cp -> let i = unsafeCoerce cp in i < 48 || (i > 57 && i < 65) || (i > 90 && i < 95) || i == 96 || i > 122) (DS.toCodePointArray atom)
           || DM.maybe false ((\cp -> let i = unsafeCoerce cp.head in i < 97 || i > 122 )) (DSCP.uncons atom)
        then "'" <> atom <> "'" else atom
    show (ErlangMap m) =
        showArrayImplGeneral "#{" "}" "," (\(DT.Tuple k v) -> show k <> " => " <> show v) (Map.toUnfoldable m)
    show (ErlangReference a) =
        show a
    show (ErlangPID a) =
        show a

-- | Compares terms for equality unifying floats and ints of the same value
weakNumEq :: ErlangTerm -> ErlangTerm -> Boolean
weakNumEq (ErlangInt a) (ErlangFloat b) = (DBI.toNumber a) == b
weakNumEq (ErlangFloat a) (ErlangInt b) = a == (DBI.toNumber b)
weakNumEq x y = eqErlangTerm weakNumEq x y

-- | Compares terms for equality treating floats and ints of the same value as different
strongNumEq :: ErlangTerm -> ErlangTerm -> Boolean
strongNumEq (ErlangInt _) (ErlangFloat _) = false
strongNumEq (ErlangFloat _) (ErlangInt _) = false
strongNumEq x y = eqErlangTerm strongNumEq x y

-- | General array comparator
eqArraysWith
  :: (ErlangTerm -> ErlangTerm -> Boolean)
  -> Array ErlangTerm -> Array ErlangTerm -> Boolean
eqArraysWith e a1 a2 =
  let go i = case DT.Tuple (DA.index a1 i) (DA.index a2 i) of
        DT.Tuple (DM.Just x1) (DM.Just x2) ->
          if e x1 x2 then go (i + 1) else false
        _ -> true
  in if DA.length a1 == DA.length a2 then go 0 else false

-- | Generic term comparator parametrized with number comparator
eqErlangTerm :: (ErlangTerm -> ErlangTerm -> Boolean)
             ->  ErlangTerm -> ErlangTerm -> Boolean
eqErlangTerm _ (ErlangAtom a) (ErlangAtom b) = a == b
eqErlangTerm _ (ErlangInt a) (ErlangInt b) = a == b
eqErlangTerm _ (ErlangFloat a) (ErlangFloat b) = a == b
eqErlangTerm numEq a@(ErlangInt _) b@(ErlangFloat _) = numEq a b
eqErlangTerm numEq a@(ErlangFloat _) b@(ErlangInt _) = numEq a b
eqErlangTerm _ (ErlangReference a) (ErlangReference b) = a == b
eqErlangTerm _ (ErlangPID a) (ErlangPID b) = a == b
eqErlangTerm numEq (ErlangCons ha ta) (ErlangCons hb tb) =
  -- heads MUST NOT be compared by recursive call
  if eqErlangTermTCOBreak numEq ha hb then eqErlangTerm numEq ta tb else false
eqErlangTerm _  ErlangEmptyList ErlangEmptyList = true
eqErlangTerm _ (ErlangBinary a) (ErlangBinary b) =
  (unsafePerformEffect $ toArray a) == (unsafePerformEffect $ toArray b)
eqErlangTerm numEq (ErlangTuple a) (ErlangTuple b) =
  eqArraysWith (eqErlangTermTCOBreak numEq) a b
eqErlangTerm numEq (ErlangMap m1) (ErlangMap m2) =
  let DT.Tuple keys1 values1 = DA.unzip $ Map.toUnfoldable m1
      DT.Tuple keys2 values2 = DA.unzip $ Map.toUnfoldable m2
  in if eqArraysWith (eqErlangTermTCOBreak strongNumEq) keys1 keys2
    then eqArraysWith (eqErlangTermTCOBreak numEq) values1 values2
    else false
eqErlangTerm _ _ _ = false

eqErlangTermTCOBreak
  :: (ErlangTerm -> ErlangTerm -> Boolean)
  ->  ErlangTerm -> ErlangTerm -> Boolean
eqErlangTermTCOBreak numEq = eqErlangTerm numEq


-- | Compares terms unifying floats and ints of the same value
weakNumCmp :: ErlangTerm -> ErlangTerm -> Ordering
weakNumCmp (ErlangInt a) (ErlangFloat b) = compare (DBI.toNumber a) b
weakNumCmp (ErlangFloat a) (ErlangInt b) = compare a (DBI.toNumber b)
weakNumCmp x y = compareErlangTerm weakNumCmp x y

-- | Compares terms treating floats and ints of the same value as different
strongNumCmp :: ErlangTerm -> ErlangTerm -> Ordering
strongNumCmp (ErlangInt _) (ErlangFloat _) = LT
strongNumCmp (ErlangFloat _) (ErlangInt _) = GT
strongNumCmp x y = compareErlangTerm strongNumCmp x y

compareArraysWith
  :: (ErlangTerm -> ErlangTerm -> Ordering)
  -> Array ErlangTerm -> Array ErlangTerm -> Ordering
compareArraysWith cmp a1 a2 =
  let l1 = DA.length a1
      l2 = DA.length a2
      go i = case DT.Tuple (DA.index a1 i) (DA.index a2 i) of
        DT.Tuple (DM.Just x1) (DM.Just x2) ->
          case cmp x1 x2 of
            EQ -> go (i + 1)
            res -> res
        _ -> EQ
  in case compare l1 l2 of
    EQ -> go 0
    res -> res

compareErlangTerm
  :: (ErlangTerm -> ErlangTerm -> Ordering)
  ->  ErlangTerm -> ErlangTerm -> Ordering
compareErlangTerm _ (ErlangInt a) (ErlangInt b) = compare a b
compareErlangTerm _ (ErlangFloat a) (ErlangFloat b) = compare a b
compareErlangTerm numCmp a@(ErlangInt _) b@(ErlangFloat _) = numCmp a b
compareErlangTerm numCmp a@(ErlangFloat _) b@(ErlangInt _) = numCmp a b
compareErlangTerm _ (ErlangReference a) (ErlangReference b) = compare a b
compareErlangTerm _ (ErlangPID a) (ErlangPID b) = compare a b
compareErlangTerm _ (ErlangAtom a) (ErlangAtom b) = compare a b
compareErlangTerm numCmp (ErlangCons ha ta) (ErlangCons hb tb) =
  -- heads MUST NOT be compared by recursive call until PS fixes TCO
  case compareErlangTermTCOBreak numCmp ha hb of
    EQ -> compareErlangTerm numCmp ta tb
    res -> res
compareErlangTerm _ ErlangEmptyList ErlangEmptyList = EQ
compareErlangTerm _ (ErlangBinary a) (ErlangBinary b) =
  compare (unsafePerformEffect $ toArray a) (unsafePerformEffect $ toArray b)
compareErlangTerm numCmp (ErlangTuple a) (ErlangTuple b) =
  compareArraysWith (compareErlangTermTCOBreak numCmp) a b
compareErlangTerm numCmp (ErlangMap m1) (ErlangMap m2) =
  let DT.Tuple keys1 values1 = DA.unzip $ Map.toUnfoldable m1
      DT.Tuple keys2 values2 = DA.unzip $ Map.toUnfoldable m2
  in case compareArraysWith (compareErlangTermTCOBreak strongNumCmp) keys1 keys2 of
    EQ -> compareArraysWith (compareErlangTermTCOBreak numCmp) values1 values2
    res -> res

compareErlangTerm _   (ErlangBinary _)    _ = GT
compareErlangTerm _ _ (ErlangBinary _)      = LT
compareErlangTerm _   (ErlangCons _ _)    _ = GT
compareErlangTerm _ _ (ErlangCons _ _)      = LT
compareErlangTerm _   (ErlangEmptyList)   _ = GT
compareErlangTerm _ _ (ErlangEmptyList)     = LT
compareErlangTerm _   (ErlangMap _)       _ = GT
compareErlangTerm _ _ (ErlangMap _)         = LT
compareErlangTerm _   (ErlangTuple _)     _ = GT
compareErlangTerm _ _ (ErlangTuple _)       = LT
compareErlangTerm _   (ErlangFun _ _)     _ = GT
compareErlangTerm _ _ (ErlangFun _ _)       = LT
compareErlangTerm _   (ErlangAtom _)      _ = GT
compareErlangTerm _ _ (ErlangAtom _)        = LT
compareErlangTerm _   (ErlangPID _)       _ = GT
compareErlangTerm _ _ (ErlangPID _)         = LT
compareErlangTerm _   (ErlangReference _) _ = GT
compareErlangTerm _ _ (ErlangReference _)   = LT

compareErlangTermTCOBreak
  :: (ErlangTerm -> ErlangTerm -> Ordering)
  ->  ErlangTerm -> ErlangTerm -> Ordering
compareErlangTermTCOBreak numCmp = compareErlangTerm numCmp

-- | Weak equality comparator
weakEq :: ErlangTerm -> ErlangTerm -> Boolean
weakEq = eqErlangTerm weakNumEq

-- | Weak nonequality comparator
weakNEq :: ErlangTerm -> ErlangTerm -> Boolean
weakNEq = not (eqErlangTerm weakNumEq)

-- | Weak comparator
weakCmp :: ErlangTerm -> ErlangTerm -> Ordering
weakCmp = compareErlangTerm weakNumCmp

-- | Weak greater-than comparator
weakGt :: ErlangTerm -> ErlangTerm -> Boolean
weakGt a b = compareErlangTerm weakNumCmp a b == GT

-- | Weak lesser-than comparator
weakLt :: ErlangTerm -> ErlangTerm -> Boolean
weakLt a b = compareErlangTerm weakNumCmp a b == LT

-- | Weak greater-equal comparator
weakGeq :: ErlangTerm -> ErlangTerm -> Boolean
weakGeq a b = compareErlangTerm weakNumCmp a b /= LT

-- | Weak lesser-equal comparator
weakLeq :: ErlangTerm -> ErlangTerm -> Boolean
weakLeq a b = compareErlangTerm weakNumCmp a b /= GT


-- | Strong equality comparator
strongEq :: ErlangTerm -> ErlangTerm -> Boolean
strongEq = eqErlangTerm strongNumEq

-- | Strong nonequality comparator
strongNEq :: ErlangTerm -> ErlangTerm -> Boolean
strongNEq = not (eqErlangTerm strongNumEq)

-- | Strong comparator
strongCmp :: ErlangTerm -> ErlangTerm -> Ordering
strongCmp = compareErlangTerm strongNumCmp

-- | Strong greater-than comparator
strongGt :: ErlangTerm -> ErlangTerm -> Boolean
strongGt a b = compareErlangTerm strongNumCmp a b == GT

-- | Strong lesser-than comparator
strongLt :: ErlangTerm -> ErlangTerm -> Boolean
strongLt a b = compareErlangTerm strongNumCmp a b == LT

-- | Strong greater-equal comparator
strongGeq :: ErlangTerm -> ErlangTerm -> Boolean
strongGeq a b = compareErlangTerm strongNumCmp a b /= LT

-- | Strong lesser-equal comparator
strongLeq :: ErlangTerm -> ErlangTerm -> Boolean
strongLeq a b = compareErlangTerm strongNumCmp a b /= GT

instance eqErlangTermInst :: Eq ErlangTerm where
    eq = strongEq

instance ordErlangTermInst :: Ord ErlangTerm where
    compare = strongCmp


-- | Version of `ErlangTerm` that uses weak comparator as default
newtype WeakErlangTerm = WeakErlangTerm ErlangTerm
unpackWeak :: WeakErlangTerm -> ErlangTerm
unpackWeak (WeakErlangTerm t) = t

instance eqWeakErlangTerm :: Eq WeakErlangTerm where
  eq e1 e2 = weakEq (unpackWeak e1) (unpackWeak e2)
instance ordWeakErlangTerm :: Ord WeakErlangTerm where
  compare e1 e2 = weakCmp (unpackWeak e1) (unpackWeak e2)


---- ToErlang class ----

-- | Conversion to Erlang term
class ToErlang e where
  toErl :: e -> ErlangTerm

instance erlangTermToErlang :: ToErlang ErlangTerm where
  toErl x = x

instance weakErlangTermToErlang :: ToErlang WeakErlangTerm where
  toErl = unpackWeak

instance foldableToErlang :: (Foldable f, ToErlang e) => ToErlang (f e) where
  toErl f = go (DL.fromFoldable f) where
    go DL.Nil = ErlangEmptyList
    go (DL.Cons h t) = ErlangCons (toErl h) (go t)

instance boolToErlang :: ToErlang Boolean where
  toErl b = ErlangAtom (if b then "true" else "false")

instance intToErlang :: ToErlang Int where
  toErl i = ErlangInt (DBI.fromInt i)

instance bigIntToErlang :: ToErlang DBI.BigInt where
  toErl = ErlangInt

instance stringToErlang :: ToErlang String where
  toErl str =
    toErl $ map (ErlangInt <<< DBI.fromInt <<< codePointToInt) (DS.toCodePointArray str)

instance bufferToErlang :: ToErlang Buffer where
  toErl = ErlangBinary

instance numberToErlang :: ToErlang Number where
  toErl = ErlangFloat

instance unitToErlang :: ToErlang Unit where
  toErl _ = ErlangTuple []

---- FromErlang class ----

-- | Conversion from Erlang term
class FromErlang e where
  fromErl :: ErlangTerm -> DM.Maybe e

instance erlangFromErlang :: FromErlang ErlangTerm where
  fromErl x = DM.Just x

instance weakErlangFromErlang :: FromErlang WeakErlangTerm where
  fromErl x = DM.Just (WeakErlangTerm x)

instance intFromErlang :: FromErlang Int where
  fromErl (ErlangInt bi) = bigIntToInt bi
  fromErl _ = DM.Nothing

instance bigIntFromErlang :: FromErlang DBI.BigInt where
  fromErl (ErlangInt bi) = DM.Just bi
  fromErl _ = DM.Nothing

instance numberFromErlang :: FromErlang Number where
  fromErl (ErlangFloat f) = DM.Just f
  fromErl _ = DM.Nothing

instance unfoldableFromErlang :: (FromErlang e, Unfoldable f) => FromErlang (f e) where
  fromErl = go DL.Nil where
    go acc ErlangEmptyList = DM.Just (DL.toUnfoldable (DL.reverse acc))
    go acc (ErlangCons eh t) | DM.Just h <- fromErl eh = go (DL.Cons h acc) t
    go _ _ = DM.Nothing

instance stringFromErlang :: FromErlang String where
  fromErl t =
    fromErl t >>=
    traverse (\x -> case x of
               ErlangInt di ->
                 map (DSCP.codePointFromChar) (bigIntToInt di >>= DC.fromCharCode)
               _ -> DM.Nothing
           )
       <#> (DSCP.fromCodePointArray)

---- Constructors ----

-- | Alias for ErlangEmptyList
nil :: ErlangTerm
nil = ErlangEmptyList

-- | Alias for ErlangCons
cons :: forall h t. ToErlang h => ToErlang t => h -> t -> ErlangTerm
cons h t = ErlangCons (toErl h) (toErl t)

-- | Infix version of `cons`
infixr 6 cons as :::

-- | Generic tuple constructor
tup :: forall e f. ToErlang e => Foldable f => f e -> ErlangTerm
tup f = ErlangTuple (map toErl $ DA.fromFoldable $ f)

bin :: forall f. Foldable f => f Int -> ErlangTerm
bin e = ErlangBinary (unsafePerformEffect (fromArray (DA.fromFoldable e)))

---- Type tests ----

-- | `is_list/1` equivalent
isEList :: ErlangTerm -> Boolean
isEList ErlangEmptyList = true
isEList (ErlangCons _ _) = true
isEList _ = false

-- | `is_list/1` equivalent
isEInt :: ErlangTerm -> Boolean
isEInt (ErlangInt _) = true
isEInt _ = false

-- | `is_list/1` equivalent
isEFloat :: ErlangTerm -> Boolean
isEFloat (ErlangFloat _) = true
isEFloat _ = false

-- | `is_list/1` equivalent
isENum :: ErlangTerm -> Boolean
isENum (ErlangInt _) = true
isENum (ErlangFloat _) = true
isENum _ = false

-- | `is_atom/1` equivalent
isEAtom :: ErlangTerm -> Boolean
isEAtom (ErlangAtom _) = true
isEAtom _ = false

-- | `is_binary/1` equivalent
isEBinary :: ErlangTerm -> Boolean
isEBinary (ErlangBinary _) = true
isEBinary _ = false

-- | `is_tuple/1` equivalent
isETuple :: ErlangTerm -> Boolean
isETuple (ErlangTuple _) = true
isETuple _ = false

-- | `is_function/1` equivalent
isEFun :: ErlangTerm -> Boolean
isEFun (ErlangFun _ _) = true
isEFun _ = false

-- | `is_function/2` equivalent
isEFunA :: ErlangTerm -> ErlangTerm -> Boolean
isEFunA (ErlangFun a0 _) (ErlangInt a1) = DBI.fromInt a0 == a1
isEFunA _ _ = false

-- | `is_map/1` equivalent
isEMap :: ErlangTerm -> Boolean
isEMap (ErlangMap _) = true
isEMap _ = false

-- | `is_pid/1` equivalent
isEPID :: ErlangTerm -> Boolean
isEPID (ErlangPID _) = true
isEPID _ = false

-- | `is_reference/1` equivalent
isEReference :: ErlangTerm -> Boolean
isEReference (ErlangReference _) = true
isEReference _ = false
