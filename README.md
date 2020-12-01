# erlps-core - ERTS emulation in Purescript

- Erlang type definitions
- Erlang BIF's
- Erlang IO server
- Mocks for erlang's unicode and b64 module
- Basic process emulation using https://github.com/elixirscript/processes

This purescript package is part of the erlscripten project/ecosystem.

# Module documentation
Published on pursuit

# Contributing
Process emulation relies on ES6 generators - we use babel to transpile our FFI modules.
ES6 FFI code should be placed in es6/ and transpiled to es5. 
1. Install babel - ```npm install```
2. Transpile ES6 modules - ```npm run babel```
3. Run tests - ```npm test```
