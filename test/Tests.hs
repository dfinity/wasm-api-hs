import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Wasm.API

import qualified Data.ByteString  as BS

memPage :: IO Memory
memPage = do
  store <- newEngine >>= newStore
  newMemory store (MemoryType $ Limits 1 1)

tests :: TestTree
tests =
  testGroup "API"
  [ testGroup "memory"
    [ testCase "write/read" $ do
        mem <- memPage
        writeMemory mem 10 (BS.pack [97..101])
        contents <- readMemory mem 1 20
        assertEqual "contents" (BS.pack $ replicate 9 0 ++ [97..101] ++ replicate 6 0) contents 

    , testCase "write/read past end is ignored" $ do
        mem <- memPage
        writeMemory mem 0 (BS.replicate 100000 1)
        contents <- readMemory mem 0 100000
        assertEqual "contents" (BS.replicate 65536 1) contents

    , testCase "can grow within bounds" $ do
        store <- newEngine >>= newStore
        mem <- newMemory store (MemoryType $ Limits 1 2)
        memorySize mem >>= (@=? 1)
        growMemory mem 1 >>= (@=? True)
        memorySize mem >>= (@=? 2)
        growMemory mem 1 >>= (@=? False)
        memorySize mem >>= (@=? 2)
    ]
  ]

main :: IO ()
main = defaultMain tests
