import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Wasm.API

import qualified Data.ByteString  as BS

mkMem :: Limits -> IO Memory
mkMem lims = do
  store <- newEngine >>= newStore
  newMemory store $ MemoryType lims

tests :: TestTree
tests =
  testGroup "API"
  [ testGroup "memory"
    [ testCase "write/read" $ do
        mem <- mkMem $ Limits 1 1
        writeMemory mem 10 (BS.pack [97..101])
        contents <- readMemory mem 1 20
        assertEqual "contents" (BS.pack $ replicate 9 0 ++ [97..101] ++ replicate 6 0) contents

    , testCase "write/read past end is ignored" $ do
        mem <- mkMem $ Limits 1 1
        writeMemory mem 0 (BS.replicate 100000 1)
        contents <- readMemory mem 0 100000
        assertEqual "contents" (BS.replicate 65536 1) contents

    , testCase "can grow within bounds" $ do
        mem <- mkMem $ Limits 1 2
        memorySize mem >>= (@=? 1)
        growMemory mem 1 >>= (@=? True)
        memorySize mem >>= (@=? 2)
        growMemory mem 1 >>= (@=? False)
        memorySize mem >>= (@=? 2)

    , testCase "can get type" $ do
        let lims = Limits 2 3
        mem <- mkMem lims
        memoryType mem @?= (MemoryType lims)
    ]
  ]

main :: IO ()
main = defaultMain tests
