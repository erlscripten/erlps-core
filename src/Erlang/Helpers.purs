-- | Collection of helpers mainly used internally in transpiled modules
module Erlang.Helpers
       ( falsifyErrors
       , flmap
       , findMissingKey
       ) where

import Erlang.Type (ErlangTerm(..))
import Erlang.Exception as EXC
import Data.Maybe as DM
import Data.Array as DA
import Data.Map as Map

import Partial.Unsafe (unsafePartial)
import Prelude (Unit, not, ($))

foreign import falsifyErrorsImpl :: ErlangTerm -> (Unit -> ErlangTerm) -> ErlangTerm

-- | Turns all exceptions into `false` atoms. Useful in guards
falsifyErrors :: (Unit -> ErlangTerm) -> ErlangTerm
falsifyErrors = falsifyErrorsImpl (ErlangAtom "false")

-- | Efficient flatmap implementation used in list comprehensions
flmap :: (Partial => ErlangTerm -> ErlangTerm) -> ErlangTerm -> ErlangTerm
flmap f list = unsafePartial $ erflat (ermap list ErlangEmptyList) ErlangEmptyList where
  ermap :: Partial => ErlangTerm -> ErlangTerm -> ErlangTerm
  ermap ErlangEmptyList acc = acc
  ermap (ErlangCons h t) acc = ermap t (ErlangCons (f h) acc)

  erflat :: Partial => ErlangTerm -> ErlangTerm -> ErlangTerm
  erflat ErlangEmptyList acc = acc
  erflat (ErlangCons ErlangEmptyList rest) acc = erflat rest acc
  erflat (ErlangCons (ErlangCons h t) rest) acc = erflat (ErlangCons t rest) (ErlangCons h acc)

-- | Out of given collection of keys find a one that does not have associated
-- | value in a map
findMissingKey :: ErlangTerm -> Array ErlangTerm -> DM.Maybe ErlangTerm
findMissingKey (ErlangMap m) keys =
  DA.find (\key -> not (Map.member key m)) keys
findMissingKey t _ = DM.Just (EXC.badmap t)
