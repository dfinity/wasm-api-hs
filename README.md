# High-level Haskell bindings to [Wasm C API](https://github.com/WebAssembly/wasm-c-api)

This package provides high-level bindings to Wasm C API.
Note that the implementation of the API is not provided: you are supposed to pick the one you like and link your binary against the corresponding library.

Some good implementations to choose from are:

  * [Wasmtime](https://github.com/bytecodealliance/wasmtime/tree/main/crates/c-api)
  * [Wasmer](https://github.com/wasmerio/wasmer/tree/master/lib/c-api)

## Example

```wat
(module
  (func $hello (import "" "hello"))
  (func (export "run") (call $hello)))
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import qualified Data.ByteString   as BS
import           Data.Foldable     (for_)
import qualified Data.Map.Strict   as Map
import           Text.Printf       (printf)
import           Wasm.API

unwrap :: Exception e => Either e a -> IO a
unwrap = either throwIO pure

helloCallback :: [Value] -> IO [Value]
helloCallback _ = do
  putStrLn "Calling back..."
  putStrLn "> Hello World!"
  pure []

main :: IO ()
main = do
  engine <- newEngine
  store <- newStore engine
  moduleBytes <- BS.readFile "./examples/hello.wasm"
  m <- unwrap =<< newModule store moduleBytes

  putStrLn "MODULE EXPORTS:"
  putStrLn "==============="
  exports <- moduleExports m
  for_ exports $ \(name, e) -> do
    printf "  %s -> %s\n" (show name) (show e)

  putStrLn "MODULE IMPORTS:"
  putStrLn "==============="
  imports <- moduleImports m
  for_ imports $ \(mod, name, e) -> do
    printf "  %s.%s -> %s\n" (show mod) (show name) (show e)

  helloFunc <- wrapFunc store (FuncType [] []) helloCallback
  inst <- unwrap =<< newInstance store m (Map.fromList [(("", "hello"), ExternFunc helloFunc)])
  externs <- instanceExports inst
  let Just (ExternFunc run) = lookup "run" externs
  putStrLn ""
  unwrap =<< call run []
```

Outputs:

```
MODULE EXPORTS:
===============
  "run" -> ExternFuncType (FuncType {funcTypeParams = [], funcTypeResults = []})
MODULE IMPORTS:
===============
  ""."hello" -> ExternFuncType (FuncType {funcTypeParams = [], funcTypeResults = []})

Calling back...
> Hello World!
```
