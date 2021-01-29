-- | Common utilities for testing transpiled projects
module Erlang.TestUtil where

import Data.Show

import Control.Monad.Error.Class (class MonadThrow)
import Data.BigInt as DBI
import Data.Either (Either(..))
import Data.Lazy (defer, force)
import Data.Maybe as DM
import Data.String.CodePoints as DSCP
import Data.Tuple as DT
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay, forkAff, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (catchException)
import Effect.Unsafe (unsafePerformEffect)
import Erlang.Builtins as BIF
import Erlang.Invoke (run_erlang)
import Erlang.Type (ErlangFun, ErlangTerm(..), fromErl)
import Erlang.Utils (runtimeError)
import Erlang.Utils as Util
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<>))
import Test.Spec.Assertions (fail, shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

-- BEWARE - HERE BE DRAGONS - I've lost too many hours debugging alternative helpers
-- If you think you can make a better wrapper which does not crash the testing infrastructure then please make a PR
-- If you can replace this helper with something better then please feel free to do so :)
execAff :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
execAff fun args =
    let
      t = defer $ (\_ -> run_erlang fun args)
      f = defer $ (\_ -> ErlangAtom "error")
    in do
      v <- liftEffect (catchException (\_ -> pure f
                                      ) (pure t))
      pure $ force v

-- | Converts thrown error into ErlangTerm
wololoTerm :: Error -> ErlangTerm
wololoTerm res = unsafeCoerce res

-- | Converts ErlangTerm (presumably, integer) into codepoint
wololoCodepoint :: ErlangTerm -> DSCP.CodePoint
wololoCodepoint (ErlangInt bres) | DM.Just res <- Util.bigIntToInt bres = unsafeCoerce res
wololoCodepoint e = Util.runtimeError $ "wololo: bad codepoint: " <> show e


printErr :: forall t31. Show t31 => Either Error t31 -> String
printErr (Right r) = show r
printErr (Left e) =
  case show e of
    "[object Object]" ->
      case (wololoTerm e) of
        ErlangTuple [a,b,stack] ->
          let
            m1 = show a
            m2 = show b
            m3 = case fromErl stack of
                      DM.Just st -> st
                      DM.Nothing -> show stack
          in "[" <> m1 <> ", " <> m2 <> ", " <> m3 <> "]"
        r -> show r
    r -> r

exec :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec fun args = do
   res <- attempt $ execAff fun args
--   liftEffect $ log $ printErr res -- Uncomment for logs :)
   case res of
     Left _ -> pure err
     Right r -> pure $ makeOk r

liftAffToErlangProcess :: forall a. (Unit -> Aff a) -> Aff (DT.Tuple ErlangTerm a)
liftAffToErlangProcess calc = do
        -- ONLY TOUCH THIS IF YOU KNOW WHAT YOU ARE DOING!!!!!
        -- THIS IS A DIRTY HACK TO "lift" an calculation in the Aff monad to an ErlangProcess from the GLOBAL scope
        res_channel <- AVar.empty
        pid_channel <- AVar.empty
        _ <- forkAff do
            packedPid <- exec BIF.erlang__spawn__1 [(
                ErlangFun 0 (\ _ -> let -- TODO: Fixme - the calculation should yield to the scheduler and only then we may launch the avar. We need a jump to FFI here :(
                    a = unsafePerformEffect $ launchAff_ (
                        do
                            res <- calc unit
                            AVar.put res res_channel
                        )
                    in
                       ErlangInt $ DBI.fromInt 1))]
            -- At this point we never yielded so the process MUST be alive
            pid <- unpackOk packedPid
            AVar.put pid pid_channel

        pid <- AVar.take pid_channel
        packed_is_alive <- exec BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "true") `shouldEqualOk` packed_is_alive

        res <- AVar.take res_channel

        delay (Milliseconds 1.0) -- force a context switch to cleanup the process :P
        alive <- exec BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "false") `shouldEqualOk` alive
        pure $ DT.Tuple pid res

ok :: ErlangTerm
ok = ErlangAtom "ok"

makeOk :: ErlangTerm -> ErlangTerm
makeOk term = ErlangTuple [ok, term]

err :: ErlangTerm
err = ErlangAtom "error"

shouldEqualOk :: forall m. MonadThrow Error m => ErlangTerm -> ErlangTerm -> m Unit
shouldEqualOk a b = makeOk a `shouldEqual` b

unpackOk :: forall m. MonadThrow Error m => ErlangTerm -> m ErlangTerm
unpackOk (ErlangTuple [ErlangAtom "ok", t]) = pure t
unpackOk t = do
  fail $ "Expected {ok, _}, got " <> show t
  runtimeError "Unpack ok: not ok"


testExecOk :: ErlangTerm -> ErlangFun -> Array ErlangTerm -> Aff Unit
testExecOk exp fun args = do
  r <- exec fun args
  exp `shouldEqualOk` r

testExecErr :: ErlangFun -> Array ErlangTerm -> Aff Unit
testExecErr fun args = do
  r <- exec fun args
  err `shouldEqual` r
