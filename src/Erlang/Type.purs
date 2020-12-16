module Erlang.Type where

import Prelude
import Node.Buffer (Buffer, toArray, fromArray, toArray, concat)
import Data.List as DL
import Data.Array as DA
import Data.BigInt as DBI
import Data.Maybe as DM
import Data.Map as Map
import Data.Char as DC
import Data.Tuple as DT
import Data.String.CodePoints as DSCP
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)

type ErlangFun = Array ErlangTerm -> ErlangTerm

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

instance showErlangTerm :: Show ErlangTerm where
    show (ErlangInt a) =
        DBI.toString a
    show (ErlangFloat a) =
        show a
    show term  | DM.Just l <- erlangListToList term =
        show l
    show (ErlangCons h t) =
        "[" <> show h <> "|" <> show t <> "]"
    show ErlangEmptyList =
        "[]"
    show (ErlangBinary a) =
        show $ unsafePerformEffect $ toArray a
    show (ErlangTuple a) =
        show a
    show (ErlangFun arity _) =
        "<some_function/" <> show arity <> ">"
    show (ErlangAtom atom) =
        atom
    show (ErlangMap m) =
        show m
    show (ErlangReference a) =
        show a
    show (ErlangPID a) =
        show a

weakNumEq :: ErlangTerm -> ErlangTerm -> Boolean
weakNumEq (ErlangInt a) (ErlangFloat b) = (DBI.toNumber a) == b
weakNumEq (ErlangFloat a) (ErlangInt b) = a == (DBI.toNumber b)
weakNumEq x y = eqErlangTerm weakNumEq x y

strongNumEq :: ErlangTerm -> ErlangTerm -> Boolean
strongNumEq (ErlangInt _) (ErlangFloat _) = false
strongNumEq (ErlangFloat _) (ErlangInt _) = false
strongNumEq x y = eqErlangTerm strongNumEq x y

eqArraysWith
  :: (ErlangTerm -> ErlangTerm -> Boolean)
  -> Array ErlangTerm -> Array ErlangTerm -> Boolean
eqArraysWith e a1 a2 =
  let go i = case DT.Tuple (DA.index a1 i) (DA.index a2 i) of
        DT.Tuple (DM.Just x1) (DM.Just x2) ->
          if e x1 x2 then go (i + 1) else false
        _ -> true
  in if DA.length a1 == DA.length a2 then go 0 else false

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


instance eqErlangTermInst :: Eq ErlangTerm where
    eq = eqErlangTerm strongNumEq


-- floatCmp :: Number -> Number -> Ordering
-- floatCmp a b = if floatEq a b then EQ else compare a b

weakNumCmp :: ErlangTerm -> ErlangTerm -> Ordering
weakNumCmp (ErlangInt a) (ErlangFloat b) = compare (DBI.toNumber a) b
weakNumCmp (ErlangFloat a) (ErlangInt b) = compare a (DBI.toNumber b)
weakNumCmp x y = compareErlangTerm weakNumCmp x y

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
compareErlangTerm _   (ErlangFloat _)     _ = GT
compareErlangTerm _ _ (ErlangFloat _)       = LT
compareErlangTerm _   (ErlangInt _)       _ = GT
compareErlangTerm _ _ (ErlangInt _)         = LT


compareErlangTermTCOBreak
  :: (ErlangTerm -> ErlangTerm -> Ordering)
  ->  ErlangTerm -> ErlangTerm -> Ordering
compareErlangTermTCOBreak numCmp = compareErlangTerm numCmp

instance ordErlangTermInst :: Ord ErlangTerm where
    compare = compareErlangTerm strongNumCmp

weakEq :: ErlangTerm -> ErlangTerm -> Boolean
weakEq = eqErlangTerm weakNumEq

weakNEq :: ErlangTerm -> ErlangTerm -> Boolean
weakNEq = not (eqErlangTerm weakNumEq)

weakCmp :: ErlangTerm -> ErlangTerm -> Ordering
weakCmp = compareErlangTerm weakNumCmp

weakGt :: ErlangTerm -> ErlangTerm -> Boolean
weakGt a b = compareErlangTerm weakNumCmp a b == GT
weakLt :: ErlangTerm -> ErlangTerm -> Boolean
weakLt a b = compareErlangTerm weakNumCmp a b == LT
weakGeq :: ErlangTerm -> ErlangTerm -> Boolean
weakGeq a b = compareErlangTerm weakNumCmp a b /= LT
weakLeq :: ErlangTerm -> ErlangTerm -> Boolean
weakLeq a b = compareErlangTerm weakNumCmp a b /= GT

concatArrays :: Buffer -> Buffer -> Effect (Buffer)
concatArrays a b = concat [a, b]

instance semigroupErlangTerm :: Semigroup ErlangTerm where
     append (ErlangBinary a) (ErlangBinary b) = ErlangBinary $ unsafePerformEffect (concatArrays a b)
     append _ _ = unsafePerformEffect $ throw "Invalid append"


erlangListToList :: ErlangTerm -> DM.Maybe (DL.List ErlangTerm)
erlangListToList = go DL.Nil where
  go acc ErlangEmptyList = DM.Just (DL.reverse acc)
  go acc (ErlangCons h t) = go (DL.Cons h acc) t
  go _ _ = DM.Nothing

arrayToErlangList :: Array ErlangTerm -> ErlangTerm
arrayToErlangList arr = go (DL.fromFoldable arr) where
  go DL.Nil = ErlangEmptyList
  go (DL.Cons h t) = ErlangCons h (go t)

boolToTerm :: Boolean -> ErlangTerm
boolToTerm true = ErlangAtom "true"
boolToTerm false = ErlangAtom "false"
