-- | Collection of helpers mainly used internally in transpiled modules
module Erlang.Helpers
       ( falsifyErrors
       , flmap
       , findMissingKey
       ) where

import Data.Array as DA
import Data.Map as Map
import Data.Maybe as DM
import Erlang.Exception as EXC
import Erlang.Type (ErlangTerm(..))
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, not, ($))

foreign import falsifyErrorsImpl :: ErlangTerm -> (Unit -> ErlangTerm) -> ErlangTerm

-- | Turns all exceptions into `false` atoms. Useful in guards
falsifyErrors :: (Unit -> ErlangTerm) -> ErlangTerm
falsifyErrors = falsifyErrorsImpl (ErlangAtom "false")

-- | Efficient flatmap implementation used in list comprehensions
flmap :: (Partial => ErlangTerm -> ErlangTerm) -> ErlangTerm -> ErlangTerm
flmap f list = unsafePartial $ goRev (goFlatMap list ErlangEmptyList ErlangEmptyList) ErlangEmptyList where

  goFlatMap :: Partial => ErlangTerm -> ErlangTerm -> ErlangTerm -> ErlangTerm
  goFlatMap ErlangEmptyList ErlangEmptyList acc = acc
  goFlatMap rest (ErlangCons h t) acc = goFlatMap rest t (ErlangCons h acc)
  goFlatMap (ErlangCons h t) ErlangEmptyList acc = goFlatMap t (f h) acc

  goRev :: Partial => ErlangTerm -> ErlangTerm -> ErlangTerm
  goRev ErlangEmptyList acc = acc
  goRev (ErlangCons h t) acc = goRev t (ErlangCons h acc)

-- | Out of given collection of keys find a one that does not have associated
-- | value in a map
findMissingKey :: ErlangTerm -> Array ErlangTerm -> DM.Maybe ErlangTerm
findMissingKey (ErlangMap m) keys =
  DA.find (\key -> not (Map.member key m)) keys
findMissingKey t _ = DM.Just (EXC.badmap t)
