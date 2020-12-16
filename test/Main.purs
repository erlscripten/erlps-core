module Test.Main where

import Prelude

import Effect.Aff.AVar as AVar
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Unsafe
import Effect.Ref as Ref
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff hiding (error)
import Effect.Exception(catchException)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.String.CodePoints as StrCP
import Data.String as Str
import Unsafe.Coerce
import Effect.Console as C

import Data.Time.Duration
import Data.Lazy
import Data.Either
import Data.Tuple as T
import Data.Array as A
import Data.Maybe as M
import Data.Map as Map
import Data.BigInt as DBI
import Partial.Unsafe
import Erlang.Type
import Erlang.Exception
import Erlang.Builtins as BIF
import Erlang.Invoke
import Erlang.Helpers as H
import Node.Buffer as Buf

-- BEWARE - HERE BE DRAGONS - I've lost too many hours debugging alternative helpers
-- If you think you can make a better wrapper which does not crash the testing infrastructure then please make a PR
-- If you can replace this helper with something better then please feel free to do so :)
exec_may_throw_aff :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw_aff fun args =
    let
      t = defer $ (\_ -> run_erlang fun args)
      f = defer $ (\_ -> ErlangAtom "error")
    in do
      v <- liftEffect (catchException (\_ -> pure f
                                      ) (pure t))
      pure $ force v

wololo_term :: Error -> ErlangTerm
wololo_term res = unsafeCoerce res

wololo_codepoint :: Partial => ErlangTerm -> StrCP.CodePoint
wololo_codepoint (ErlangInt res) = unsafeCoerce $ unsafePartial $ M.fromJust $ H.bigIntToInt res

print_err (Right r) = show r
print_err (Left e) =
    case show e of
        "[object Object]" ->
            case (wololo_term e) of
                ErlangTuple [a,b,stack] ->
                    let
                        m1 = show a
                        m2 = show b
                        m3 = unsafePartial $ Str.fromCodePointArray $ map wololo_codepoint $ A.fromFoldable $ M.fromJust $ erlangListToList stack
                    in
                        "[" <> m1 <> ", " <> m2 <> ", " <> m3 <> "]"
                r ->
                    show r
        r ->
            r

exec_may_throw :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw fun args = do
    res <- attempt $ exec_may_throw_aff fun args
--    liftEffect $ log $ print_err res -- Uncomment for logs :)
    case res of
        Left _ -> pure make_err
        Right r -> pure $ make_ok r

lift_aff_to_erlang_process :: forall a. (Unit -> Aff a) -> Aff (T.Tuple ErlangTerm a)
lift_aff_to_erlang_process calc = do
        -- ONLY TOUCH THIS IF YOU KNOW WHAT YOU ARE DOING!!!!!
        -- THIS IS A DIRTY HACK TO "lift" an calculation in the Aff monad to an ErlangProcess from the GLOBAL scope
        res_channel <- AVar.empty
        pid_channel <- AVar.empty
        _ <- forkAff do
            packed_pid <- exec_may_throw BIF.erlang__spawn__1 [(
                ErlangFun 0 (\ _ -> let -- TODO: Fixme - the calculation should yield to the scheduler and only then we may launch the avar. We need a jump to FFI here :(
                    a = unsafePerformEffect $ launchAff_ (
                        do
                            res <- calc unit
                            AVar.put res res_channel
                        )
                    in
                       ErlangInt $ DBI.fromInt 1))]
            -- At this point we never yielded so the process MUST be alive
            pid <- unpack_ok packed_pid
            AVar.put pid pid_channel

        pid <- AVar.take pid_channel
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "true") `shouldEqualOk` packed_is_alive

        res <- AVar.take res_channel

        delay (Milliseconds 1.0) -- force a context switch to cleanup the process :P
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "false") `shouldEqualOk` packed_is_alive
        pure $ T.Tuple pid res

make_ok term = ErlangTuple [ErlangAtom "ok", term]
make_err = ErlangAtom "error"

mkInt :: Int -> ErlangTerm
mkInt = DBI.fromInt >>> ErlangInt

mkIntList :: Array Int -> ErlangTerm
mkIntList a = arrayToErlangList $ map mkInt a

mkBInt s = ErlangInt $ unsafePartial $ M.fromJust $ DBI.fromString s
get2to32 = mkBInt "4294967296"

mkFloatList :: Array Number -> ErlangTerm
mkFloatList a = arrayToErlangList $ map ErlangFloat a

unpack_ok :: ErlangTerm -> Aff ErlangTerm
unpack_ok (ErlangTuple [ErlangAtom "ok", term]) = pure term
unpack_ok _ = do
    1 `shouldEqual` 0
    pure ErlangEmptyList

shouldEqualOk a b = make_ok a `shouldEqual` b

main :: Effect Unit
main =
    launchAff_ $ runSpec [consoleReporter] do

    let whitelist = case unit of
          _ -> M.Nothing  -- comment for whitelist :)
          _ -> M.Just ["Binaries"]
    let describe_ s = case whitelist of
          M.Nothing -> describe s
          M.Just l ->
            if A.elemIndex s l == M.Nothing then \_ -> pure unit else describe s
    describe_ "Sanity check" do
        it "one should equal one" do
            1 `shouldEqual` 1
        it "two should equal two" do
            2 `shouldEqual` 2

    describe "Comparator" do
        let nassert f x = do
              r <- exec_may_throw f x
              ErlangAtom "false" `shouldEqualOk` r
        let assert f x = do
              r <- exec_may_throw f x
              ErlangAtom "true" `shouldEqualOk` r
        it "1 == 1" $ assert BIF.erlang__op_eq [mkInt 1, mkInt 1]
        it "1 =:= 1" $ assert BIF.erlang__op_exactEq [mkInt 1, mkInt 1]
        it "1 /= 1" $ nassert BIF.erlang__op_neq [mkInt 1, mkInt 1]
        it "1 =/= 1" $ nassert BIF.erlang__op_exactNeq [mkInt 1, mkInt 1]
        it "1.0 == 1.0" $ assert BIF.erlang__op_eq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 =:= 1.0" $ assert BIF.erlang__op_exactEq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 /= 1.0" $ nassert BIF.erlang__op_neq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 =/= 1.0" $ nassert BIF.erlang__op_exactNeq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1 == 1.0" $ assert BIF.erlang__op_eq [mkInt 1, ErlangFloat 1.0]
        it "1 =:= 1.0" $ nassert BIF.erlang__op_exactEq [mkInt 1, ErlangFloat 1.0]
        it "1 /= 1.0" $ nassert BIF.erlang__op_neq [mkInt 1, ErlangFloat 1.0]
        it "1 =/= 1.0" $ assert BIF.erlang__op_exactNeq [mkInt 1, ErlangFloat 1.0]

        it "1 < 1" $ nassert BIF.erlang__op_lesser [mkInt 1, mkInt 1]
        it "1 > 1" $ nassert BIF.erlang__op_greater [mkInt 1, mkInt 1]
        it "1 =< 1" $ assert BIF.erlang__op_lesserEq [mkInt 1, mkInt 1]
        it "1 >= 1" $ assert BIF.erlang__op_greaterEq [mkInt 1, mkInt 1]
        it "1.0 < 1.0" $ nassert BIF.erlang__op_lesser [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 > 1.0" $ nassert BIF.erlang__op_greater [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 =< 1.0" $ assert BIF.erlang__op_lesserEq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 >= 1.0" $ assert BIF.erlang__op_greaterEq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1 < 1.0" $ nassert BIF.erlang__op_lesser [mkInt 1, ErlangFloat 1.0]
        it "1 > 1.0" $ nassert BIF.erlang__op_greater [mkInt 1, ErlangFloat 1.0]
        it "1 =< 1.0" $ assert BIF.erlang__op_lesserEq [mkInt 1, ErlangFloat 1.0]
        it "1 >= 1.0" $ assert BIF.erlang__op_greaterEq [mkInt 1, ErlangFloat 1.0]

    describe "Round" do
        it "0.0" do
          r <- exec_may_throw BIF.erlang__round__1 [ErlangFloat 0.0]
          mkInt 0 `shouldEqualOk` r
        it "0.1" do
          r <- exec_may_throw BIF.erlang__round__1 [ErlangFloat 0.1]
          mkInt 0 `shouldEqualOk` r
        it "0.5" do
          r <- exec_may_throw BIF.erlang__round__1 [ErlangFloat 0.5]
          mkInt 1 `shouldEqualOk` r
        it "0.7" do
          r <- exec_may_throw BIF.erlang__round__1 [ErlangFloat 0.7]
          mkInt 1 `shouldEqualOk` r
        it "-0.0" do
          r <- exec_may_throw BIF.erlang__round__1 [ErlangFloat (-0.0)]
          mkInt 0 `shouldEqualOk` r
        it "-0.1" do
          r <- exec_may_throw BIF.erlang__round__1 [ErlangFloat (-0.1)]
          mkInt 0 `shouldEqualOk` r
        it "-0.5" do
          r <- exec_may_throw BIF.erlang__round__1 [ErlangFloat (-0.5)]
          mkInt (-1) `shouldEqualOk` r
        it "-0.7" do
          r <- exec_may_throw BIF.erlang__round__1 [ErlangFloat (-0.7)]
          mkInt (-1) `shouldEqualOk` r

    describe "Operators" do
        it "-- 1" do
            r <- exec_may_throw BIF.erlang__op_unAppend [ErlangEmptyList, ErlangEmptyList]
            ErlangEmptyList `shouldEqualOk` r
        it "-- 2" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkIntList [1,2,3], mkIntList [1,2,3]]
            mkIntList [] `shouldEqualOk` r
        it "-- 3" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkIntList [1,2,3], mkIntList []]
            mkIntList [1,2,3] `shouldEqualOk` r
        it "-- 4" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkIntList [1,2,3], mkIntList [2]]
            mkIntList [1,3] `shouldEqualOk` r
        it "-- 5" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkIntList [1,2,3], mkIntList [2,1]]
            mkIntList [3] `shouldEqualOk` r
        it "-- 6" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkIntList [1,2,3], mkIntList [20]]
            mkIntList [1,2,3] `shouldEqualOk` r
        it "-- 7" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkIntList [2], mkFloatList [2.0]]
            mkIntList [2] `shouldEqualOk` r
        it "-- 8" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkFloatList [2.0], mkFloatList [2.0]]
            mkIntList [] `shouldEqualOk` r
        it "-- 9" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkIntList [2], ErlangCons (ErlangFloat 0.1) (ErlangFloat 0.1)]
            make_err `shouldEqual` r
        it "-- 10" do
            r <- exec_may_throw BIF.erlang__op_unAppend [mkIntList [], ErlangCons (ErlangFloat 0.1) (ErlangFloat 0.1)]
            make_err `shouldEqual` r
        it "-- 11" do
            r <- exec_may_throw BIF.erlang__op_unAppend [ErlangCons (ErlangFloat 0.1) (ErlangFloat 0.1), mkIntList []]
            make_err `shouldEqual` r

        it "comparators 1" do
            r1 <- exec_may_throw BIF.erlang__op_lesser [ErlangAtom "asdf", mkInt 0]
            ErlangAtom "false" `shouldEqualOk` r1
            r2 <- exec_may_throw BIF.erlang__op_greater [ErlangAtom "asdf", mkInt 0]
            ErlangAtom "true" `shouldEqualOk` r2
        it "comparators 2" do
            r1 <- exec_may_throw BIF.erlang__op_lesser [ErlangAtom "x", mkInt 0]
            ErlangAtom "false" `shouldEqualOk` r1
            r2 <- exec_may_throw BIF.erlang__op_greater [ErlangAtom "x", mkInt 0]
            ErlangAtom "true" `shouldEqualOk` r2

        it "[0.1 + 0.2] -- [0.3] == [0.30000000000000004]" do
            lr <- exec_may_throw BIF.erlang__op_plus [ErlangFloat 0.1, ErlangFloat 0.2]
            l <- unpack_ok lr
            rr <- exec_may_throw BIF.erlang__op_unAppend [ErlangCons l ErlangEmptyList, mkFloatList [2.0]]
            r <- unpack_ok rr
            rr <- exec_may_throw BIF.erlang__length__1 [r]
            mkInt 1 `shouldEqualOk` rr
        it "lists__keymember__3 float 1" do
            -- true = lists:keymember(1.0, 1, [{1}])
            r <- exec_may_throw BIF.lists__keymember__3 [ErlangFloat 1.0, mkInt 1, ErlangCons (ErlangTuple [mkInt 1]) ErlangEmptyList]
            ErlangAtom "true" `shouldEqualOk` r
        it "lists__keymember__3 float 2" do
            -- true = lists:keymember({1.0}, 1, [{{1}}])
            r <- exec_may_throw BIF.lists__keymember__3 [ErlangTuple [ErlangFloat 1.0], mkInt 1, ErlangCons (ErlangTuple [ErlangTuple [mkInt 1]]) ErlangEmptyList]
            ErlangAtom "true" `shouldEqualOk` r
        it "lists__keymember__3 float 3" do
            -- true = lists:keymember({1.0}, 1, [{{1}}])
            r <- exec_may_throw BIF.lists__keymember__3 [mkFloatList [1.0], mkInt 1, ErlangCons (ErlangTuple [mkIntList [1]]) ErlangEmptyList]
            ErlangAtom "true" `shouldEqualOk` r

        it "lists__keyfind__3 float 1" do
            -- {1} = lists:keyfind(1.0, 1, [{1}])
            r <- exec_may_throw BIF.lists__keyfind__3 [ErlangFloat 1.0, mkInt 1, ErlangCons (ErlangTuple [mkInt 1]) ErlangEmptyList]
            ErlangTuple [mkInt 1] `shouldEqualOk` r
        it "lists__keysearch__3 float 1" do
            -- {value, {1}} = lists:keysearch(1.0, 1, [{1}])
            r <- exec_may_throw BIF.lists__keysearch__3 [ErlangFloat 1.0, mkInt 1, ErlangCons (ErlangTuple [mkInt 1]) ErlangEmptyList]
            ErlangTuple [ErlangAtom "value", ErlangTuple [mkInt 1]] `shouldEqualOk` r

        it "reverse/2 1" do
            let a = mkIntList [1,2,3,4,5]
            let b = mkIntList []
            r <- exec_may_throw BIF.lists__reverse__2 [a, b]
            mkIntList [5,4,3,2,1] `shouldEqualOk` r
        it "reverse/2 2" do
            let a = mkIntList [1,2,3,4,5]
            let b = mkIntList [6,7,8,9,10]
            r <- exec_may_throw BIF.lists__reverse__2 [a, b]
            mkIntList [5,4,3,2,1,6,7,8,9,10] `shouldEqualOk` r

        it "++ 1" do
            let a = mkIntList [1,2,3,4,5]
            let b = mkIntList []
            r <- exec_may_throw BIF.erlang__op_append [a, b]
            mkIntList [1,2,3,4,5] `shouldEqualOk` r
        it "++ 2" do
            let a = mkIntList [1,2,3,4,5]
            let b = mkIntList [6,7,8,9,10]
            r <- exec_may_throw BIF.erlang__op_append [a, b]
            mkIntList [1,2,3,4,5,6,7,8,9,10] `shouldEqualOk` r


    describe "Process dictionary" do
        it "1" do
            r1 <- exec_may_throw BIF.erlang__get__0 []
            ErlangEmptyList `shouldEqualOk` r1
            r2 <- exec_may_throw BIF.erlang__get__1 [mkInt 10]
            ErlangAtom "undefined" `shouldEqualOk` r2
            r3 <- exec_may_throw BIF.erlang__put__2 [mkInt 1, mkInt 2]
            ErlangAtom "undefined" `shouldEqualOk` r3
            r4 <- exec_may_throw BIF.erlang__get__1 [mkInt 1]
            mkInt 2 `shouldEqualOk` r4
            r5 <- exec_may_throw BIF.erlang__get__1 [mkInt 2]
            ErlangAtom "undefined" `shouldEqualOk` r5
            r6 <- exec_may_throw BIF.erlang__get__0 []
            arrayToErlangList [ErlangTuple [mkInt 1, mkInt 2]] `shouldEqualOk` r6
            r7 <- exec_may_throw BIF.erlang__put__2 [mkInt 1, mkInt 20]
            mkInt 2 `shouldEqualOk` r7
            r8 <- exec_may_throw BIF.erlang__get__1 [mkInt 1]
            mkInt 20 `shouldEqualOk` r8
            r9 <- exec_may_throw BIF.erlang__get__1 [mkInt 2]
            ErlangAtom "undefined" `shouldEqualOk` r9
            r10 <- exec_may_throw BIF.erlang__get__0 []
            arrayToErlangList [ErlangTuple [mkInt 1, mkInt 20]] `shouldEqualOk` r10
            r11 <- exec_may_throw BIF.erlang__get_keys__0 []
            mkIntList [1] `shouldEqualOk` r11
            r12 <- exec_may_throw BIF.erlang__get_keys__1 [mkInt 20]
            mkIntList [1] `shouldEqualOk` r12
            r13 <- exec_may_throw BIF.erlang__get_keys__1 [mkInt 1]
            mkIntList [] `shouldEqualOk` r13
            r14 <- exec_may_throw BIF.erlang__erase__0 []
            arrayToErlangList [ErlangTuple [mkInt 1, mkInt 20]] `shouldEqualOk` r14
            r15 <- exec_may_throw BIF.erlang__erase__0 []
            mkIntList [] `shouldEqualOk` r15
            r16 <- exec_may_throw BIF.erlang__erase__1 [mkInt 1]
            ErlangAtom "undefined" `shouldEqualOk` r16
            r17 <- exec_may_throw BIF.erlang__put__2 [mkInt 1, mkInt 2]
            ErlangAtom "undefined" `shouldEqualOk` r17
            r18 <- exec_may_throw BIF.erlang__erase__1 [mkInt 1]
            mkInt 2 `shouldEqualOk` r18
            r19 <- exec_may_throw BIF.erlang__erase__1 [mkInt 1]
            ErlangAtom "undefined" `shouldEqualOk` r19
            _ <- exec_may_throw BIF.erlang__put__2 [mkInt 1, mkInt 10]
            _ <- exec_may_throw BIF.erlang__put__2 [mkInt 2, mkInt 20]
            _ <- exec_may_throw BIF.erlang__put__2 [mkInt 3, mkInt 30]
            r20 <- exec_may_throw BIF.erlang__erase__1 [mkInt 2]
            mkInt 20 `shouldEqualOk` r20
            r21 <- exec_may_throw BIF.erlang__erase__1 [mkInt 2]
            ErlangAtom "undefined" `shouldEqualOk` r21
            r22 <- exec_may_throw BIF.erlang__erase__1 [mkInt 1]
            mkInt 10 `shouldEqualOk` r22
            r23 <- exec_may_throw BIF.erlang__erase__1 [mkInt 1]
            ErlangAtom "undefined" `shouldEqualOk` r23
            r24 <- exec_may_throw BIF.erlang__get__1 [mkInt 3]
            mkInt 30 `shouldEqualOk` r24
            r25 <- exec_may_throw BIF.erlang__erase__0 []
            arrayToErlangList [ErlangTuple [mkInt 3, mkInt 30]] `shouldEqualOk` r25

    describe "Hashing..." do
        it "phash/2 - empty list" do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangEmptyList, get2to32]
            mkInt 2 `shouldEqualOk` r
        it "phash/2 - number 0" do
            r <- exec_may_throw BIF.erlang__phash__2 [mkInt 0, get2to32]
            mkInt 1 `shouldEqualOk` r
        it "phash/2 - number 1" do
            r <- exec_may_throw BIF.erlang__phash__2 [mkInt 1, get2to32]
            mkBInt "2788898428" `shouldEqualOk` r
        it "phash/2 - number 12345678" do
            r <- exec_may_throw BIF.erlang__phash__2 [mkBInt "12345678", get2to32]
            mkBInt "71480002" `shouldEqualOk` r
        it "phash/2 - number -12345678" do
            r <- exec_may_throw BIF.erlang__phash__2 [mkBInt "-12345678", get2to32]
            mkBInt "752010448" `shouldEqualOk` r
        it "phash/2 - number 11111111111111111111111111111" do
            r <- exec_may_throw BIF.erlang__phash__2 [mkBInt "11111111111111111111111111111", get2to32]
            mkBInt "1065806199" `shouldEqualOk` r
        it "phash/2 - number -11111111111111111111111111111" do
            r <- exec_may_throw BIF.erlang__phash__2 [mkBInt "-11111111111111111111111111111", get2to32]
            mkBInt "1051343595" `shouldEqualOk` r
        it "phash/2 - tuple {}" do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangTuple [], get2to32]
            mkBInt "1" `shouldEqualOk` r
        it "erlang:phash({1,2,3}, 1 bsl 32)" do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangTuple [mkInt 1, mkInt 2, mkInt 3], get2to32]
            mkBInt "4267354534" `shouldEqualOk` r
        it "erlang:phash({1,2,3,{1,2},{},4}, 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangTuple [mkInt 1, mkInt 2, mkInt 3, ErlangTuple [mkInt 1, mkInt 2], ErlangTuple [], mkInt 4], get2to32]
            mkBInt "1228626870" `shouldEqualOk` r
        it "erlang:phash([1,2,3], 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [arrayToErlangList [mkInt 1, mkInt 2, mkInt 3], get2to32]
            mkBInt "3336869158" `shouldEqualOk` r
        it "erlang:phash([1], 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [arrayToErlangList [mkInt 1], get2to32]
            mkBInt "2952798237" `shouldEqualOk` r
        it "erlang:phash([{}], 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [arrayToErlangList [ErlangTuple []], get2to32]
            mkBInt "268437512" `shouldEqualOk` r
        it "erlang:phash([{},{},{}], 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [arrayToErlangList [ErlangTuple [], ErlangTuple [], ErlangTuple []], get2to32]
            mkBInt "268437512" `shouldEqualOk` r
        it "erlang:phash([{},{},{},[]], 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [arrayToErlangList [ErlangTuple [], ErlangTuple [], ErlangTuple [], ErlangEmptyList], get2to32]
            mkBInt "2952798237" `shouldEqualOk` r
        it "erlang:phash([1000,2000,3000,4000], 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [arrayToErlangList [mkInt 1000, mkInt 2000, mkInt 3000, mkInt 4000], get2to32]
            mkBInt "225899384" `shouldEqualOk` r
        it "erlang:phash(asdf, 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangAtom "asdf", get2to32]
            mkBInt "428455" `shouldEqualOk` r
        it "erlang:phash(begwrrew, 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangAtom "begwrrew", get2to32]
            mkBInt "200187464" `shouldEqualOk` r
        it "erlang:phash(1.0, 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangFloat 1.0, get2to32]
            mkBInt "1072693249" `shouldEqualOk` r
        it "erlang:phash(0.0, 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangFloat 0.0, get2to32]
            mkBInt "1" `shouldEqualOk` r
        it "erlang:phash(1337.1337, 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangFloat 1337.1337, get2to32]
            mkBInt "2821978480" `shouldEqualOk` r
        it "erlang:phash([1|2], 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [ErlangCons (mkInt 1) (mkInt 2), get2to32]
            mkBInt "2402949552" `shouldEqualOk` r
        it "erlang:phash([{1,2},2,{3,4},1.0,test,{test}], 1 bsl 32)." do
            r <- exec_may_throw BIF.erlang__phash__2 [arrayToErlangList [ErlangTuple [mkInt 1, mkInt 2], mkInt 2, ErlangTuple [mkInt 3, mkInt 4], ErlangFloat 1.0, ErlangAtom "test", ErlangTuple [ErlangAtom "test"]], get2to32]
            mkBInt "1005103067" `shouldEqualOk` r
        it "erlang:phash(200, 1337)." do
            r <- exec_may_throw BIF.erlang__phash__2 [mkInt 200, mkInt 1337]
            mkBInt "521" `shouldEqualOk` r

    describe "Ensure loaded" do
      it "unsuccessful" do
        ErlangTuple [ErlangAtom "error", ErlangAtom "nofile"] `shouldEqual` BIF.code__ensure_loaded__1 [ErlangAtom "tralalalalala"]
      it "successful" do
        ErlangTuple [ErlangAtom "module", ErlangAtom "erlang_io"] `shouldEqual` BIF.code__ensure_loaded__1 [ErlangAtom "erlang_io"]
        ErlangTuple [ErlangAtom "module", ErlangAtom "erlang_io"] `shouldEqual` BIF.code__ensure_loaded__1 [ErlangAtom "erlang_io"]

    describe "make_tuple" do
      it "{[],aa,[],[],zz} = erlang:make_tuple(5, [], [{2, ignored}, {5,zz}, {2, aa}])." do
            let a = ErlangTuple [mkInt 2, ErlangAtom "ignored"]
            let b = ErlangTuple [mkInt 5, ErlangAtom "zz"]
            let c = ErlangTuple [mkInt 2, ErlangAtom "aa"]
            let d = ErlangTuple [ErlangEmptyList, ErlangAtom "aa", ErlangEmptyList, ErlangEmptyList, ErlangAtom "zz"]
            r <- exec_may_throw BIF.erlang__make_tuple__3 [mkInt 5, ErlangEmptyList, arrayToErlangList [a,b,c]]
            d `shouldEqualOk` r
      it "{1} = erlang:make_tuple(1, [], [{1,1}])." do
            r <- exec_may_throw BIF.erlang__make_tuple__3 [mkInt 1, ErlangEmptyList, arrayToErlangList [ErlangTuple [mkInt 1, mkInt 1]]]
            ErlangTuple [mkInt 1] `shouldEqualOk` r
      it "{} = erlang:make_tuple(0,0,[])." do
            r <- exec_may_throw BIF.erlang__make_tuple__3 [mkInt 0, mkInt 0, ErlangEmptyList]
            ErlangTuple [] `shouldEqualOk` r
      it "{0} = erlang:make_tuple(1,0,[])." do
            r <- exec_may_throw BIF.erlang__make_tuple__3 [mkInt 1, mkInt 0, ErlangEmptyList]
            ErlangTuple [mkInt 0] `shouldEqualOk` r

    describe "apply/3" do
        it "calls biffs" do
            r <- exec_may_throw BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "hd", arrayToErlangList [mkIntList [1]]]
            mkInt 1 `shouldEqualOk` r
        it "calls +" do
            r <- exec_may_throw BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "+", arrayToErlangList [mkInt 1, mkInt 2]]
            mkInt 3 `shouldEqualOk` r
        it "calls -" do
            r <- exec_may_throw BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "-", arrayToErlangList [mkInt 5, mkInt 2]]
            mkInt 3 `shouldEqualOk` r
        it "calls *" do
            r <- exec_may_throw BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "*", arrayToErlangList [mkInt 2, mkInt 3]]
            mkInt 6 `shouldEqualOk` r
        it "calls unary neg" do
            r <- exec_may_throw BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "-", arrayToErlangList [mkInt 3]]
            mkInt (-3) `shouldEqualOk` r
        it "calls float" do
            r <- exec_may_throw BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "float", arrayToErlangList [mkInt 3]]
            ErlangFloat 3.0 `shouldEqualOk` r
