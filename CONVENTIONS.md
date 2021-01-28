# CONVENTIONS

## Functions pretending to be original erlang functions

For example builtins and manually transpiled mocks. This is the only 
case when snake_case is allowed. The function should be prefixed with
`erlps__` and suffixed with `__n` where `n` is arity of the function.
Functions like that should not be parial – they should handle
`function_clause`, `badarg` and `badarity` errors by themselves keeping
as much compatibility with original Erlang implementations as possible.

## Imports

The following conventions for import aliases shoud be followed:

 - `import Data.Maybe as DM`
 - `import Data.Tuple as DT`
 - `import Data.Array as DA`
 - `import Data.List as DL`
 - `import Data.BigInt as DBI`
 - `import Data.String as DS`
 - `import Data.String.CodePoints as DSCP`
 
 - `import Data.Char as Char`
 - `import Data.Int as Int`
 - `import Data.Map as Map`
 - `import Data.Set as Set`
 
 - `import Erlang.Helpers as H`
 - `import Erlang.Builtins as BIF`
 - `import Erlang.Binary as BIN`
 - `import Erlang.Exception as EXC`
 - `import Erlang.Type as ERL`
 - `import Erlang.Utils as Util`
