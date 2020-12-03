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
    -- liftEffect $ log $ print_err res -- Uncomment for logs :)
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
