module CoDoKurwy where

import Erlang.Type
import Erlang.Helpers
import Data.Array
import Data.Map as Map
import Prelude
import Data.BigInt as DBI

i = DBI.fromInt >>> ErlangInt

wtf =
  Map.delete (ErlangAtom "a") $ Map.fromFoldable (
    zip
    [ErlangAtom "a", ErlangAtom "b", ErlangAtom "c", make_string "a", make_string "b", make_string "c"]
    [i 0, i 0, i 0, i 0, i 0, i 0])

lol =
  Map.fromFoldable (
    zip
    [ErlangAtom "b", ErlangAtom "c", make_string "a", make_string "b", make_string "c"]
    [i 0, i 0, i 0, i 0, i 0])
