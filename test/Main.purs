module Test.Main where

import Erlang.TestUtil (err, exec, shouldEqualOk, testExecOk, unpackOk, testExecErr)
import Erlang.Type (ErlangTerm(..), bin, nil, toErl, onElement, weakEq, weakGt)
import Erlang.Builtins as BIF
import Erlang.Utils(runtimeError)

import Prelude
import Effect.Aff (launchAff_)
import Data.BigInt as DBI
import Data.Map as Map
import Data.Maybe as DM
import Data.Array as DA
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
    launchAff_ $ runSpec [consoleReporter] do

    describe "Comparator" do
        let nassert = testExecOk (ErlangAtom "false")
        let assert = testExecOk (ErlangAtom "true")
        it "1 == 1" $ assert BIF.erlang__op_eq [toErl 1, toErl 1]
        it "1 =:= 1" $ assert BIF.erlang__op_exactEq [toErl 1, toErl 1]
        it "1 /= 1" $ nassert BIF.erlang__op_neq [toErl 1, toErl 1]
        it "1 =/= 1" $ nassert BIF.erlang__op_exactNeq [toErl 1, toErl 1]
        it "1.0 == 1.0" $ assert BIF.erlang__op_eq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 =:= 1.0" $ assert BIF.erlang__op_exactEq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 /= 1.0" $ nassert BIF.erlang__op_neq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 =/= 1.0" $ nassert BIF.erlang__op_exactNeq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1 == 1.0" $ assert BIF.erlang__op_eq [toErl 1, ErlangFloat 1.0]
        it "1 =:= 1.0" $ nassert BIF.erlang__op_exactEq [toErl 1, ErlangFloat 1.0]
        it "1 /= 1.0" $ nassert BIF.erlang__op_neq [toErl 1, ErlangFloat 1.0]
        it "1 =/= 1.0" $ assert BIF.erlang__op_exactNeq [toErl 1, ErlangFloat 1.0]

        it "1 < 1" $ nassert BIF.erlang__op_lesser [toErl 1, toErl 1]
        it "1 > 1" $ nassert BIF.erlang__op_greater [toErl 1, toErl 1]
        it "1 =< 1" $ assert BIF.erlang__op_lesserEq [toErl 1, toErl 1]
        it "1 >= 1" $ assert BIF.erlang__op_greaterEq [toErl 1, toErl 1]
        it "1.0 < 1.0" $ nassert BIF.erlang__op_lesser [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 > 1.0" $ nassert BIF.erlang__op_greater [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 =< 1.0" $ assert BIF.erlang__op_lesserEq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1.0 >= 1.0" $ assert BIF.erlang__op_greaterEq [ErlangFloat 1.0, ErlangFloat 1.0]
        it "1 < 1.0" $ nassert BIF.erlang__op_lesser [toErl 1, ErlangFloat 1.0]
        it "1 > 1.0" $ nassert BIF.erlang__op_greater [toErl 1, ErlangFloat 1.0]
        it "1 =< 1.0" $ assert BIF.erlang__op_lesserEq [toErl 1, ErlangFloat 1.0]
        it "1 >= 1.0" $ assert BIF.erlang__op_greaterEq [toErl 1, ErlangFloat 1.0]

    describe "Round" do
        it "0.0" do
          testExecOk (toErl 0) BIF.erlang__round__1 [ErlangFloat 0.0]
        it "0.1" do
          testExecOk (toErl 0) BIF.erlang__round__1 [ErlangFloat 0.1]
        it "0.5" do
          testExecOk (toErl 1) BIF.erlang__round__1 [ErlangFloat 0.5]
        it "0.7" do
          testExecOk (toErl 1) BIF.erlang__round__1 [ErlangFloat 0.7]
        it "-0.0" do
          testExecOk (toErl 0) BIF.erlang__round__1 [ErlangFloat (-0.0)]
        it "-0.1" do
          testExecOk (toErl 0) BIF.erlang__round__1 [ErlangFloat (-0.1)]
        it "-0.5" do
          testExecOk (toErl (-1)) BIF.erlang__round__1 [ErlangFloat (-0.5)]
        it "-0.7" do
          testExecOk (toErl (-1)) BIF.erlang__round__1 [ErlangFloat (-0.7)]

    describe "Operators" do
        it "-- 1" do
            testExecOk (ErlangEmptyList) BIF.erlang__op_unAppend [ErlangEmptyList, ErlangEmptyList]
        it "-- 2" do
            testExecOk (nil) BIF.erlang__op_unAppend [toErl [1,2,3], toErl [1,2,3]]
        it "-- 3" do
            testExecOk (toErl [1,2,3]) BIF.erlang__op_unAppend [toErl [1,2,3], nil]
        it "-- 4" do
            testExecOk (toErl [1,3]) BIF.erlang__op_unAppend [toErl [1,2,3], toErl [2]]
        it "-- 5" do
            testExecOk (toErl [3]) BIF.erlang__op_unAppend [toErl [1,2,3], toErl [2,1]]
        it "-- 6" do
            testExecOk (toErl [1,2,3]) BIF.erlang__op_unAppend [toErl [1,2,3], toErl [20]]
        it "-- 7" do
            testExecOk (toErl [2]) BIF.erlang__op_unAppend [toErl [2], toErl [2.0]]
        it "-- 8" do
            testExecOk (nil) BIF.erlang__op_unAppend [toErl [2.0], toErl [2.0]]
        it "-- 9" do
            testExecErr BIF.erlang__op_unAppend [toErl [2], ErlangCons (ErlangFloat 0.1) (ErlangFloat 0.1)]
        it "-- 10" do
            testExecErr BIF.erlang__op_unAppend [nil, ErlangCons (ErlangFloat 0.1) (ErlangFloat 0.1)]
        it "-- 11" do
            testExecErr BIF.erlang__op_unAppend [ErlangCons (ErlangFloat 0.1) (ErlangFloat 0.1), nil]

        it "comparators 1" do
            r1 <- exec BIF.erlang__op_lesser [ErlangAtom "asdf", toErl 0]
            ErlangAtom "false" `shouldEqualOk` r1
            r2 <- exec BIF.erlang__op_greater [ErlangAtom "asdf", toErl 0]
            ErlangAtom "true" `shouldEqualOk` r2
        it "comparators 2" do
            r1 <- exec BIF.erlang__op_lesser [ErlangAtom "x", toErl 0]
            ErlangAtom "false" `shouldEqualOk` r1
            r2 <- exec BIF.erlang__op_greater [ErlangAtom "x", toErl 0]
            ErlangAtom "true" `shouldEqualOk` r2

        it "[0.1 + 0.2] -- [0.3] == [0.30000000000000004]" do
            lr <- exec BIF.erlang__op_plus [ErlangFloat 0.1, ErlangFloat 0.2]
            l <- unpackOk lr
            rr <- exec BIF.erlang__op_unAppend [ErlangCons l ErlangEmptyList, toErl [2.0]]
            r <- unpackOk rr
            rrr <- exec BIF.erlang__length__1 [r]
            toErl 1 `shouldEqualOk` rrr
        it "lists__keymember__3 float 1" do
            -- true = lists:keymember(1.0, 1, [{1}])
            testExecOk (ErlangAtom "true") BIF.lists__keymember__3 [ErlangFloat 1.0, toErl 1, ErlangCons (ErlangTuple [toErl 1]) ErlangEmptyList]
        it "lists__keymember__3 float 2" do
            -- true = lists:keymember({1.0}, 1, [{{1}}])
            testExecOk (ErlangAtom "true") BIF.lists__keymember__3 [ErlangTuple [ErlangFloat 1.0], toErl 1, ErlangCons (ErlangTuple [ErlangTuple [toErl 1]]) ErlangEmptyList]
        it "lists__keymember__3 float 3" do
            -- true = lists:keymember({1.0}, 1, [{{1}}])
            r <- exec BIF.lists__keymember__3 [toErl [1.0], toErl 1, ErlangCons (ErlangTuple [toErl [1]]) ErlangEmptyList]
            ErlangAtom "true" `shouldEqualOk` r

        it "lists__keyfind__3 float 1" do
            -- {1} = lists:keyfind(1.0, 1, [{1}])
            r <- exec BIF.lists__keyfind__3 [ErlangFloat 1.0, toErl 1, ErlangCons (ErlangTuple [toErl 1]) ErlangEmptyList]
            ErlangTuple [toErl 1] `shouldEqualOk` r
        it "lists__keysearch__3 float 1" do
            -- {value, {1}} = lists:keysearch(1.0, 1, [{1}])
            r <- exec BIF.lists__keysearch__3 [ErlangFloat 1.0, toErl 1, ErlangCons (ErlangTuple [toErl 1]) ErlangEmptyList]
            ErlangTuple [ErlangAtom "value", ErlangTuple [toErl 1]] `shouldEqualOk` r

        it "reverse/2 1" do
            let a = toErl [1,2,3,4,5]
            let b = nil
            testExecOk (toErl [5,4,3,2,1]) BIF.lists__reverse__2 [a, b]
        it "reverse/2 2" do
            let a = toErl [1,2,3,4,5]
            let b = toErl [6,7,8,9,10]
            testExecOk (toErl [5,4,3,2,1,6,7,8,9,10]) BIF.lists__reverse__2 [a, b]

        it "++ 1" do
            let a = toErl [1,2,3,4,5]
            let b = nil
            testExecOk (toErl [1,2,3,4,5]) BIF.erlang__op_append [a, b]
        it "++ 2" do
            let a = toErl [1,2,3,4,5]
            let b = toErl [6,7,8,9,10]
            testExecOk (toErl [1,2,3,4,5,6,7,8,9,10]) BIF.erlang__op_append [a, b]


    describe "Process dictionary" do
        it "1" do
            r1 <- exec BIF.erlang__get__0 []
            ErlangEmptyList `shouldEqualOk` r1
            r2 <- exec BIF.erlang__get__1 [toErl 10]
            ErlangAtom "undefined" `shouldEqualOk` r2
            r3 <- exec BIF.erlang__put__2 [toErl 1, toErl 2]
            ErlangAtom "undefined" `shouldEqualOk` r3
            r4 <- exec BIF.erlang__get__1 [toErl 1]
            toErl 2 `shouldEqualOk` r4
            r5 <- exec BIF.erlang__get__1 [toErl 2]
            ErlangAtom "undefined" `shouldEqualOk` r5
            r6 <- exec BIF.erlang__get__0 []
            toErl [ErlangTuple [toErl 1, toErl 2]] `shouldEqualOk` r6
            r7 <- exec BIF.erlang__put__2 [toErl 1, toErl 20]
            toErl 2 `shouldEqualOk` r7
            r8 <- exec BIF.erlang__get__1 [toErl 1]
            toErl 20 `shouldEqualOk` r8
            r9 <- exec BIF.erlang__get__1 [toErl 2]
            ErlangAtom "undefined" `shouldEqualOk` r9
            r10 <- exec BIF.erlang__get__0 []
            toErl [ErlangTuple [toErl 1, toErl 20]] `shouldEqualOk` r10
            r11 <- exec BIF.erlang__get_keys__0 []
            toErl [1] `shouldEqualOk` r11
            r12 <- exec BIF.erlang__get_keys__1 [toErl 20]
            toErl [1] `shouldEqualOk` r12
            r13 <- exec BIF.erlang__get_keys__1 [toErl 1]
            nil `shouldEqualOk` r13
            r14 <- exec BIF.erlang__erase__0 []
            toErl [ErlangTuple [toErl 1, toErl 20]] `shouldEqualOk` r14
            r15 <- exec BIF.erlang__erase__0 []
            nil `shouldEqualOk` r15
            r16 <- exec BIF.erlang__erase__1 [toErl 1]
            ErlangAtom "undefined" `shouldEqualOk` r16
            r17 <- exec BIF.erlang__put__2 [toErl 1, toErl 2]
            ErlangAtom "undefined" `shouldEqualOk` r17
            r18 <- exec BIF.erlang__erase__1 [toErl 1]
            toErl 2 `shouldEqualOk` r18
            r19 <- exec BIF.erlang__erase__1 [toErl 1]
            ErlangAtom "undefined" `shouldEqualOk` r19
            _ <- exec BIF.erlang__put__2 [toErl 1, toErl 10]
            _ <- exec BIF.erlang__put__2 [toErl 2, toErl 20]
            _ <- exec BIF.erlang__put__2 [toErl 3, toErl 30]
            r20 <- exec BIF.erlang__erase__1 [toErl 2]
            toErl 20 `shouldEqualOk` r20
            r21 <- exec BIF.erlang__erase__1 [toErl 2]
            ErlangAtom "undefined" `shouldEqualOk` r21
            r22 <- exec BIF.erlang__erase__1 [toErl 1]
            toErl 10 `shouldEqualOk` r22
            r23 <- exec BIF.erlang__erase__1 [toErl 1]
            ErlangAtom "undefined" `shouldEqualOk` r23
            r24 <- exec BIF.erlang__get__1 [toErl 3]
            toErl 30 `shouldEqualOk` r24
            r25 <- exec BIF.erlang__erase__0 []
            toErl [ErlangTuple [toErl 3, toErl 30]] `shouldEqualOk` r25

    describe "Hashing..." do
        let mkBInt s = case DBI.fromString s of
              DM.Just i -> ErlangInt i
              DM.Nothing -> runtimeError $ "Bad int " <> s
            get2to32 = mkBInt "4294967296"
        it "phash/2 - empty list" do
            testExecOk (toErl 2) BIF.erlang__phash__2 [ErlangEmptyList, get2to32]
        it "phash/2 - number 0" do
            testExecOk (toErl 1) BIF.erlang__phash__2 [toErl 0, get2to32]
        it "phash/2 - number 1" do
            testExecOk (mkBInt "2788898428") BIF.erlang__phash__2 [toErl 1, get2to32]
        it "phash/2 - number 12345678" do
            testExecOk (mkBInt "71480002") BIF.erlang__phash__2 [mkBInt "12345678", get2to32]
        it "phash/2 - number -12345678" do
            testExecOk (mkBInt "752010448") BIF.erlang__phash__2 [mkBInt "-12345678", get2to32]
        it "phash/2 - number 11111111111111111111111111111" do
            testExecOk (mkBInt "1065806199") BIF.erlang__phash__2 [mkBInt "11111111111111111111111111111", get2to32]
        it "phash/2 - number -11111111111111111111111111111" do
            testExecOk (mkBInt "1051343595") BIF.erlang__phash__2 [mkBInt "-11111111111111111111111111111", get2to32]
        it "phash/2 - tuple {}" do
            testExecOk (mkBInt "1") BIF.erlang__phash__2 [ErlangTuple [], get2to32]
        it "erlang:phash({1,2,3}, 1 bsl 32)" do
            testExecOk (mkBInt "4267354534") BIF.erlang__phash__2 [ErlangTuple [toErl 1, toErl 2, toErl 3], get2to32]
        it "erlang:phash({1,2,3,{1,2},{},4}, 1 bsl 32)." do
            testExecOk (mkBInt "1228626870") BIF.erlang__phash__2 [ErlangTuple [toErl 1, toErl 2, toErl 3, ErlangTuple [toErl 1, toErl 2], ErlangTuple [], toErl 4], get2to32]
        it "erlang:phash([1,2,3], 1 bsl 32)." do
            testExecOk (mkBInt "3336869158") BIF.erlang__phash__2 [toErl [toErl 1, toErl 2, toErl 3], get2to32]
        it "erlang:phash([1], 1 bsl 32)." do
            testExecOk (mkBInt "2952798237") BIF.erlang__phash__2 [toErl [toErl 1], get2to32]
        it "erlang:phash([{}], 1 bsl 32)." do
            testExecOk (mkBInt "268437512") BIF.erlang__phash__2 [toErl [ErlangTuple []], get2to32]
        it "erlang:phash([{},{},{}], 1 bsl 32)." do
            testExecOk (mkBInt "268437512") BIF.erlang__phash__2 [toErl [ErlangTuple [], ErlangTuple [], ErlangTuple []], get2to32]
        it "erlang:phash([{},{},{},[]], 1 bsl 32)." do
            testExecOk (mkBInt "2952798237") BIF.erlang__phash__2 [toErl [ErlangTuple [], ErlangTuple [], ErlangTuple [], ErlangEmptyList], get2to32]
        it "erlang:phash([1000,2000,3000,4000], 1 bsl 32)." do
            testExecOk (mkBInt "225899384") BIF.erlang__phash__2 [toErl [toErl 1000, toErl 2000, toErl 3000, toErl 4000], get2to32]
        it "erlang:phash(a, 1 bsl 32)." do
            testExecOk (mkBInt "98") BIF.erlang__phash__2 [ErlangAtom "a", get2to32]
        it "erlang:phash(b, 1 bsl 32)." do
            testExecOk (mkBInt "99") BIF.erlang__phash__2 [ErlangAtom "b", get2to32]
        it "erlang:phash(r, 1 bsl 32)." do
            testExecOk (mkBInt "115") BIF.erlang__phash__2 [ErlangAtom "r", get2to32]
        it "erlang:phash(aa, 1 bsl 32)." do
            testExecOk (mkBInt "1650") BIF.erlang__phash__2 [ErlangAtom "aa", get2to32]
        it "erlang:phash(aaa, 1 bsl 32)." do
            testExecOk (mkBInt "26482") BIF.erlang__phash__2 [ErlangAtom "aaa", get2to32]
        it "erlang:phash(asdf, 1 bsl 32)." do
            testExecOk (mkBInt "428455") BIF.erlang__phash__2 [ErlangAtom "asdf", get2to32]
        it "erlang:phash(aaaaa, 1 bsl 32)." do
            testExecOk (mkBInt "6780786") BIF.erlang__phash__2 [ErlangAtom "aaaaa", get2to32]
        it "erlang:phash(aaaaaa, 1 bsl 32)." do
            testExecOk (mkBInt "108492658") BIF.erlang__phash__2 [ErlangAtom "aaaaaa", get2to32]
        it "erlang:phash(aaaaaaa, 1 bsl 32)." do
            testExecOk (mkBInt "125269778") BIF.erlang__phash__2 [ErlangAtom "aaaaaaa", get2to32]
        it "erlang:phash(begwrrew, 1 bsl 32)." do
            testExecOk (mkBInt "200187464") BIF.erlang__phash__2 [ErlangAtom "begwrrew", get2to32]
        it "erlang:phash(aaaaaaaaa, 1 bsl 32)." do
            testExecOk (mkBInt "125243394") BIF.erlang__phash__2 [ErlangAtom "aaaaaaaaa", get2to32]
        it "erlang:phash(1.0, 1 bsl 32)." do
            testExecOk (mkBInt "1072693249") BIF.erlang__phash__2 [ErlangFloat 1.0, get2to32]
        it "erlang:phash(0.0, 1 bsl 32)." do
            testExecOk (mkBInt "1") BIF.erlang__phash__2 [ErlangFloat 0.0, get2to32]
        it "erlang:phash(-0.0, 1 bsl 32)." do
            testExecOk (mkBInt "1") BIF.erlang__phash__2 [ErlangFloat (-0.0), get2to32]
        it "erlang:phash(1337.1337, 1 bsl 32)." do
            testExecOk (mkBInt "2821978480") BIF.erlang__phash__2 [ErlangFloat 1337.1337, get2to32]
        it "erlang:phash([1|2], 1 bsl 32)." do
            testExecOk (mkBInt "2402949552") BIF.erlang__phash__2 [ErlangCons (toErl 1) (toErl 2), get2to32]
        it "erlang:phash([{1,2},2,{3,4},1.0,test,{test}], 1 bsl 32)." do
            testExecOk (mkBInt "1005103067") BIF.erlang__phash__2 [toErl [ErlangTuple [toErl 1, toErl 2], toErl 2, ErlangTuple [toErl 3, toErl 4], ErlangFloat 1.0, ErlangAtom "test", ErlangTuple [ErlangAtom "test"]], get2to32]
        it "erlang:phash(200, 1337)." do
            testExecOk (mkBInt "521") BIF.erlang__phash__2 [toErl 200, toErl 1337]
        it "erlang:phash(<<>>, 1 bsl 32)." do
            testExecOk (mkBInt "1") BIF.erlang__phash__2 [bin [], get2to32]
        it "erlang:phash(<<1,3,3,7>>, 1 bsl 32)." do
            testExecOk (mkBInt "3003004243") BIF.erlang__phash__2 [bin [1,3,3,7], get2to32]
        it "erlang:phash(#{}, 1 bsl 32)." do
            testExecOk (mkBInt "1113425985") BIF.erlang__phash__2 [ErlangMap Map.empty, get2to32]
        it "performance" do
            testExecOk (mkBInt "3430193160") BIF.erlang__phash__2 [toErl (DA.replicate 1024 1337), get2to32]

    describe "Ensure loaded" do
      it "unsuccessful" do
        ErlangTuple [ErlangAtom "error", ErlangAtom "nofile"] `shouldEqual` BIF.code__ensure_loaded__1 [ErlangAtom "tralalalalala"]
      it "successful" do
        ErlangTuple [ErlangAtom "module", ErlangAtom "erlang_io"] `shouldEqual` BIF.code__ensure_loaded__1 [ErlangAtom "erlang_io"]
        ErlangTuple [ErlangAtom "module", ErlangAtom "erlang_io"] `shouldEqual` BIF.code__ensure_loaded__1 [ErlangAtom "erlang_io"]

    describe "make_tuple" do
      it "{[],aa,[],[],zz} = erlang:make_tuple(5, [], [{2, ignored}, {5,zz}, {2, aa}])." do
            let a = ErlangTuple [toErl 2, ErlangAtom "ignored"]
            let b = ErlangTuple [toErl 5, ErlangAtom "zz"]
            let c = ErlangTuple [toErl 2, ErlangAtom "aa"]
            let d = ErlangTuple [ErlangEmptyList, ErlangAtom "aa", ErlangEmptyList, ErlangEmptyList, ErlangAtom "zz"]
            testExecOk (d) BIF.erlang__make_tuple__3 [toErl 5, ErlangEmptyList, toErl [a,b,c]]
      it "{1} = erlang:make_tuple(1, [], [{1,1}])." do
            testExecOk (ErlangTuple [toErl 1]) BIF.erlang__make_tuple__3 [toErl 1, nil, toErl [ErlangTuple [toErl 1, toErl 1]]]
      it "{} = erlang:make_tuple(0,0,[])." do
            testExecOk (ErlangTuple []) BIF.erlang__make_tuple__3 [toErl 0, toErl 0, ErlangEmptyList]
      it "{0} = erlang:make_tuple(1,0,[])." do
            testExecOk (ErlangTuple [toErl 0]) BIF.erlang__make_tuple__3 [toErl 1, toErl 0, ErlangEmptyList]

    describe "apply/3" do
        it "calls biffs" do
            testExecOk (toErl 1) BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "hd", toErl [toErl [1]]]
        it "calls +" do
            testExecOk (toErl 3) BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "+", toErl [toErl 1, toErl 2]]
        it "calls -" do
            testExecOk (toErl 3) BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "-", toErl [toErl 5, toErl 2]]
        it "calls *" do
            testExecOk (toErl 6) BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "*", toErl [toErl 2, toErl 3]]
        it "calls unary neg" do
            testExecOk (toErl (-3)) BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "-", toErl [toErl 3]]
        it "calls float" do
            testExecOk (ErlangFloat 3.0) BIF.erlang__apply__3 [ErlangAtom "erlang", ErlangAtom "float", toErl [toErl 3]]

    describe "list_to_binary" do
        it "proper list 1" do
            testExecOk (bin [1,2,3,4]) BIF.erlang__list_to_binary__1 [toErl [1,2,3,4]]
        it "proper list 2" do
            testExecOk (bin [1,2,3,4,6,7]) BIF.erlang__list_to_binary__1 [toErl [toErl [1,2,3,4], toErl 6, toErl 7]]
        it "proper list 3" do
            testExecOk (bin [1,2,3,4,6,7,1,2,3,4,6,7]) BIF.erlang__list_to_binary__1 [toErl [toErl [1,2,3,4], toErl 6, toErl 7, bin [1,2,3,4,6,7]]]
        it "direct binary" do
            testExecErr BIF.erlang__list_to_binary__1 [bin [1,3,3,7]]
        it "improperList 1" do
            -- end of IOLIST can be a binary
            testExecOk (bin [1,2]) BIF.erlang__list_to_binary__1 [ErlangCons (toErl 1) (bin [2])]
        it "improperList 2" do
            -- end of IOLIST is either a binary or a empty list
            testExecErr BIF.erlang__list_to_binary__1 [ErlangCons (toErl 1) (toErl 2)]
        it "iolists contain bytes not ints ;)" do
            r1 <- exec BIF.erlang__list_to_binary__1 [toErl [255]]
            bin [255] `shouldEqualOk` r1
            r2 <- exec BIF.erlang__list_to_binary__1 [toErl [256]]
            err `shouldEqual` r2

    describe "iolist_to_binary" do
        it "proper list 1" do
            testExecOk (bin [1,2,3,4]) BIF.erlang__iolist_to_binary__1 [toErl [1,2,3,4]]
        it "proper list 2" do
            testExecOk (bin [1,2,3,4,6,7]) BIF.erlang__iolist_to_binary__1 [toErl [toErl [1,2,3,4], toErl 6, toErl 7]]
        it "proper list 3" do
            testExecOk (bin [1,2,3,4,6,7,1,2,3,4,6,7]) BIF.erlang__iolist_to_binary__1 [toErl [toErl [1,2,3,4], toErl 6, toErl 7, bin [1,2,3,4,6,7]]]
        it "direct binary" do
            testExecOk (bin [1,3,3,7]) BIF.erlang__iolist_to_binary__1 [bin [1,3,3,7]]
        it "improperList 1" do
            -- end of IOLIST can be a binary
            testExecOk (bin [1,2]) BIF.erlang__iolist_to_binary__1 [ErlangCons (toErl 1) (bin [2])]
        it "improperList 2" do
            -- end of IOLIST is either a binary or a empty list
            testExecErr BIF.erlang__iolist_to_binary__1 [ErlangCons (toErl 1) (toErl 2)]
        it "iolists contain bytes not ints ;)" do
            r1 <- exec BIF.erlang__iolist_to_binary__1 [toErl [255]]
            bin [255] `shouldEqualOk` r1
            r2 <- exec BIF.erlang__iolist_to_binary__1 [toErl [256]]
            err `shouldEqual` r2

    describe "iolist_size" do
        it "sample" do
            testExecOk (toErl 4) BIF.erlang__iolist_size__1 [toErl [1,2,3,4]]

    describe "iolist_to_iovec" do
        it "sample" do
            testExecOk ((toErl [bin [1,2,3,4]])) BIF.erlang__iolist_to_iovec__1 [toErl [1,2,3,4]]

    describe "helpers" do
        it "onElement" do
            true `shouldEqual` onElement (toErl 1) (ErlangTuple [ErlangAtom "a", ErlangAtom "b"]) weakEq (ErlangAtom "a")
            true `shouldEqual` onElement (toErl 2) (ErlangTuple [ErlangAtom "a", ErlangAtom "b"]) weakEq (ErlangAtom "b")
            false `shouldEqual` onElement (toErl 3) (ErlangTuple [ErlangAtom "a", ErlangAtom "b"]) weakEq (ErlangAtom "b")
            true `shouldEqual` onElement (toErl 1) (ErlangTuple [toErl 10]) weakGt (toErl 5)
            false `shouldEqual` onElement (toErl 1) (ErlangTuple [toErl 5]) weakGt (toErl 10)
