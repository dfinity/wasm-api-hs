{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import           Data.Foldable   (for_)
import           Text.Printf     (printf)
import           Wasm.API

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
  m <- newModule store moduleBytes

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
  inst <- newInstance store m [("", "hello", ExternFunc helloFunc)]
  externs <- instanceExports inst
  let Just (ExternFunc run) = lookup "run" externs

  putStrLn "================================================================================"

  call run []

  putStrLn "================================================================================"
  putStrLn "Great success!"
