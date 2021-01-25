{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Wasm.API
  ( Store
  , Module
  , Instance
  , Func
  , Global
  , Table
  , Memory
  , Limits(..)
  , Value(..)
  , ValueType(..)
  , Extern(..)
  , Mutability(..)
  , FuncType(..)
  , GlobalType(..)
  , TableType(..)
  , MemoryType(..)
  , NumPages(..)
  -- * Recoverable errors
  , InvalidModule
  , NewInstanceError
  , Trap
  -- * Unrecoverable errors
  , UnknownValueKind
  , UnknownExternKind
  , UnknownMutability
  , WrongResultsArity
  -- * Manipulating modules and instances
  , newEngine
  , newStore
  , newModule
  , newInstance
  , moduleImports
  , moduleExports
  , instanceExports
  -- * Creating and calling functions
  , call
  , wrapFunc
  -- * Manipulating instance memory
  , newMemory
  , memorySize
  , memoryDataSize
  , writeMemory
  , readMemory
  , unsafeUseMemory
  , growMemory
  ) where

import           Control.Exception        (Exception, SomeException, bracket,
                                           finally, handle, throw, throwIO)
import           Control.Monad            (when)

import           Foreign.C.Types
import qualified Foreign.Concurrent
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable         (peek)

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as IBS
import qualified Data.ByteString.Unsafe   as UBS
import           Data.Coerce              (coerce)
import           Data.Foldable            (find, for_)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Foreign        as T
import           Data.Traversable         (for)
import           Data.Typeable
import           Data.Word                (Word32, Word64, Word8)
import           System.IO.Unsafe         (unsafePerformIO)

import qualified Wasm.API.Raw             as Raw

-- | A newtype for number of Wasm memory pages (64KiB).
newtype NumPages = NumPages { numPagesToWord32 :: Word32 }
  deriving (Show, Eq, Ord, Num, Bounded)

newtype Engine = Engine { enginePtr :: ForeignPtr Raw.WasmEngine }

-- | A store is a collection of host-provided objects and instantiated
-- Wasm modules.
--
-- Stores are often treated as a "single unit" and items within a
-- store are all allowed to reference one another. References across
-- stores cannot currently be created. For example you cannot
-- instantiate a module in one store with an instance from another
-- store.
data Store
  = Store
    { storePtr    :: ForeignPtr Raw.WasmStore
    , storeEngine:: Engine
    }

data Module
  = Module
    { modulePtr   :: ForeignPtr Raw.WasmModule
    , moduleStore:: Store
    }

data Instance
  = Instance
    { instancePtr    :: ForeignPtr Raw.WasmInstance
    , instanceStore  :: Store
    , instanceModule :: Module
    }

data Func
  = Func
    { funcStore :: Store
    , funcPtr   :: ForeignPtr Raw.WasmFunc
    }

data Global
   = Global
     { globalStore :: Store
     , globalPtr   :: ForeignPtr Raw.WasmGlobal
     }

data Table
  = Table
    { tableStore :: Store
    , tablePtr   :: ForeignPtr Raw.WasmTable
    }

data Memory
  = Memory
    { memoryStore :: Store
    , memoryPtr   :: ForeignPtr Raw.WasmMemory
    }

-- | Limits for tables/memories in Wasm modules.
--
-- Memory is measured in number of Wasm pages (64KiB).
-- Tables are measured in number of entries.
data Limits
  = Limits
    { limitsMin :: Word32
    , limitsMax :: Word32
    } deriving (Show, Eq)

data Value
 = I32 Word32
 | I64 Word64
 | F32 Float
 | F64 Double
 deriving (Show, Eq)

data ValueType
  = I32Type
  | I64Type
  | F32Type
  | F64Type
  | AnyRefType
  | FuncRefType
 deriving (Show, Eq)

data Extern
  = ExternFunc Func
  | ExternTable Table
  | ExternGlobal Global
  | ExternMemory Memory

data Mutability
  = Const
  | Var
  deriving (Show, Eq)

data FuncType
  = FuncType
    { funcTypeParams  :: [ValueType]
    , funcTypeResults :: [ValueType]
    } deriving (Show, Eq)

data GlobalType
  = GlobalType
    { globalType       :: ValueType
    , globalMutability :: Mutability
    } deriving (Show, Eq)

data TableType
  = TableType
    { tableTypeElement :: ValueType
    , tableTypeLimits  :: Limits
    } deriving (Show, Eq)

data MemoryType
  = MemoryType
    { memoryTypeLimits :: Limits
    } deriving (Show, Eq)

data ExternType
  = ExternFuncType FuncType
  | ExternGlobalType GlobalType
  | ExternTableType TableType
  | ExternMemoryType MemoryType
  deriving (Show, Eq)

newtype InvalidModule = InvalidModule ByteString deriving (Show, Typeable)
instance Exception InvalidModule

newtype UnknownValueKind = UnknownValueKind Word8 deriving (Show, Typeable)
instance Exception UnknownValueKind

newtype UnknownExternKind = UnknownExternKind Word8 deriving (Show, Typeable)
instance Exception UnknownExternKind

newtype UnknownMutability = UnknownMutability Word8 deriving (Show, Typeable)
instance Exception UnknownMutability

newtype Trap = Trap Text deriving (Show, Typeable)
instance Exception Trap

data NewInstanceError
  = UnresolvedImport (Text, Text)
  | StartTrapped Trap
  deriving (Show, Typeable)

instance Exception NewInstanceError

data WrongResultsArity
  = WrongResultsArity
    { declaredResultsArity :: Int
    , actualResultsArity   :: Int
    } deriving (Show, Typeable)

instance Exception WrongResultsArity

-- TODO: do we need masking to avoid leaks in case of async exceptions?

-- | Creates a new engine with the default configuration.
newEngine :: IO Engine
newEngine = do
  enginePtr <- newForeignPtr Raw.deleteEngine =<< Raw.newEngine
  pure $ Engine enginePtr

-- | Creates a new store within the specified engine.
newStore :: Engine -> IO Store
newStore engine =
  withForeignPtr (enginePtr engine) $ \ptr -> do
    rawPtr <- Raw.newStore ptr
    -- NOTE: we could have used Foreign.Ptr.NewForeignPtr here, but we
    -- want to be able to attach more Haskell finalizers to the store
    -- pointer, and one can't mix C and Haskell finalizers on the same
    -- pointer.
    storePtr <- Foreign.Concurrent.newForeignPtr rawPtr (Raw.deleteStore rawPtr)
    pure $ Store { storePtr = storePtr, storeEngine = engine}

-- | Compiles a raw WebAssembly binary to a @Module@.
--
-- This function will validate and compile the provided binary. The
-- returned @Module@ is ready for instantiation after this call
-- returns.
--
-- This function may fail if the provided binary is not a WebAssembly
-- binary or if it does not pass validation. In these cases this
-- function returns @InvalidModule@.
newModule :: Store -> ByteString -> IO (Either InvalidModule Module)
newModule store moduleBytes =
  withByteVec moduleBytes $ \byteVecPtr ->
    withForeignPtr (storePtr store) $ \ptr -> do
        rawModulePtr <- Raw.newModule ptr byteVecPtr

        if rawModulePtr == nullPtr
        then pure $ Left $ InvalidModule moduleBytes
        else do
          modulePtr <- newForeignPtr Raw.deleteModule rawModulePtr
          pure $ Right $ Module { modulePtr = modulePtr, moduleStore = store }

-- | Instantiates a @Module@ with the provided imports into the @Store@.
--
-- Instantiation includes invoking the start function of a Wasm
-- module.  If @start@ traps then this function returns @StartTrapped@
-- error.
--
-- The provided imports must contain at least all the imports returned
-- by the corresponding @moduleImports@, otherwise this function
-- returns @UnresolvedImport@ error.
newInstance :: Store
            -> Module
            -> Map (Text, Text) Extern
            -> IO (Either NewInstanceError Instance)
newInstance s m deps = do
  imports <- moduleImports m
  case resolveImports imports deps of
    Left e        -> pure $ Left e
    Right imports ->
      withForeignPtr (storePtr s) $ \sPtr ->
        withForeignPtr (modulePtr m) $ \mPtr ->
        withExternArray imports $ \arr ->
          alloca $ \trapPtrPtr -> do
          p <- Raw.newInstance sPtr mPtr arr trapPtrPtr
          trapPtr <- peek trapPtrPtr
          checkTrap trapPtr >>= \case
            Just trap -> do
              deleteTrap trapPtr
              pure $ Left $ StartTrapped trap
            Nothing -> do
              fptr <- newForeignPtr Raw.deleteInstancePtr p
              pure $ Right $ Instance { instancePtr = fptr
                                      , instanceStore = s
                                      , instanceModule = m
                                      }

checkTrap :: Ptr Raw.WasmTrap -> IO (Maybe Trap)
checkTrap trapPtr =
  if trapPtr /= nullPtr
  then bracket Raw.allocateEmptyByteVec Raw.deepFreeByteVec $ \outPtr -> do
          Raw.trapMessage trapPtr outPtr
          msg <- vecToName outPtr
          pure $ Just $ Trap msg
  else pure Nothing

-- | Returns the exports of an instance.
--
-- The list will have the same length as @moduleExports@ called on the
-- original module.  Each element is 1:1 matched with the elements in
-- the list of @moduleExports@.
instanceExports :: Instance -> IO [(Text, Extern)]
instanceExports inst = do
  exports <- moduleExports (instanceModule inst)
  withForeignPtr (instancePtr inst) $ \instPtr ->
    bracket (Raw.newUninitExternVec (fromIntegral $ length exports)) Raw.deepFreeExternVec $ \vecPtr -> do
      Raw.instanceExports instPtr vecPtr
      n <- Raw.externVecSize vecPtr
      externs <- for [1..n] $ \i ->
        Raw.indexExternVec vecPtr (fromIntegral $ pred i) >>= getExtern (instanceStore inst)
      pure $ zipWith (\(name, _t) e -> (name, e)) exports externs

resolveImports :: [(Text, Text, ExternType)]
               -> Map (Text, Text) Extern
               -> Either NewInstanceError [Extern]
resolveImports imports deps = traverse findDep imports
  where findDep (mod, name, _) =
          case M.lookup (mod, name) deps of
            Just v  -> Right v -- TODO: check that type of v matches t
            Nothing -> Left $ UnresolvedImport (mod, name)

-- | Returns the list of exports that this module provides.
moduleExports :: Module -> IO [(Text, ExternType)]
moduleExports m =
  withForeignPtr (modulePtr m) $ \mPtr ->
    bracket Raw.allocateExportTypeVec Raw.deepFreeExportTypeVec $ \v -> do
      Raw.moduleExports mPtr v
      n <- Raw.exportTypeVecSize v
      for [1 .. n] $ \i -> do
        e <- Raw.indexExportTypeVec v (fromIntegral $ pred i)
        name <- Raw.exportTypeName e >>= vecToName
        typ <- Raw.exportTypeType e >>= getExternType
        pure (name, typ)

-- | Returns the list of imports that this module expects.
moduleImports :: Module -> IO [(Text, Text, ExternType)]
moduleImports m =
  withForeignPtr (modulePtr m) $ \mPtr ->
    bracket Raw.allocateImportTypeVec Raw.deepFreeImportTypeVec $ \v -> do
      Raw.moduleImports mPtr v
      n <- Raw.importTypeVecSize v
      for [1 .. n] $ \i -> do
        e <- Raw.indexImportTypeVec v (fromIntegral $ pred i)
        mod <- Raw.importTypeModule e >>= vecToName
        name <- Raw.importTypeName e >>= vecToName
        typ <- Raw.importTypeType e >>= getExternType
        pure (mod, name, typ)

-- | Calls the provided function with the arguments given.
--
-- This function is used to call WebAssembly from the host.  The
-- parameter list contain exactly @funcParamArity@ arguments, and the
-- result list will contain exactly @funcResultsArity@ values.
--
-- This function returns a @Trap@ on the following occasions:
--   - Any of the arguments do not have the correct type.
--   - Any of the arguments come from a different store than the @Func@ provided.
--   - Function execution traps.
call :: Func -> [Value] -> IO (Either Trap [Value])
call f args =
  withForeignPtr (funcPtr f) $ \fPtr -> do
    paramArity <- Raw.funcParamArity fPtr
    resultArity <- Raw.funcResultArity fPtr
    bracket (makeValueArray args) Raw.deleteValueArray $ \paramsPtr ->
      bracket (Raw.newValueArray resultArity) Raw.deleteValueArray $ \resultsPtr ->
        bracket (Raw.callFunc fPtr paramsPtr resultsPtr) deleteTrap $ \trapPtr -> do
          checkTrap trapPtr >>= \case
            Just trap -> pure $ Left trap
            Nothing   -> Right <$> getValueArray (fromIntegral resultArity) resultsPtr

-- | Returns the number of values the provided function takes as input.
funcParamArity :: Func -> Int
funcParamArity f = fromIntegral $ unsafePerformIO (withForeignPtr (funcPtr f) Raw.funcParamArity)

-- | Returns the number of values the provided function produces as output.
funcResultArity :: Func -> Int
funcResultArity f = fromIntegral $ unsafePerformIO (withForeignPtr (funcPtr f) Raw.funcResultArity)

-- | Converts a Haskell function into a @Func@.
--
-- Any exceptions thrown by the function will be converted to a trap.
wrapFunc :: Store -> FuncType -> ([Value] -> IO [Value]) -> IO Func
wrapFunc store funcType f = do
  callback <- Raw.mkWasmFuncCallback (w store)
  wasmFuncType <- makeFuncType funcType
  withForeignPtr (storePtr store) $ \sPtr -> do
    f <- Raw.newFunc sPtr wasmFuncType callback
    Foreign.Concurrent.addForeignPtrFinalizer (storePtr store) (freeHaskellFunPtr callback)
    Func store <$> newForeignPtr Raw.deleteFuncPtr f

  where inArity = length $ funcTypeParams funcType
        outArity = length $ funcTypeResults funcType

        w store inValPtr outValPtr = do
          args <- getValueArray inArity inValPtr
          handle (\(e :: SomeException) -> newTrap store (T.encodeUtf8 $ T.pack $ show e)) $ do
            results <- f args
            when (length results /= outArity) $
              throwIO $ WrongResultsArity { declaredResultsArity = outArity
                                          , actualResultsArity = length results
                                          }
            setValueArray results outValPtr
            pure nullPtr

-- | Creates a new memory in the provided store.
newMemory :: Store -> MemoryType -> IO Memory
newMemory s ty =
  withForeignPtr (storePtr s) $ \pStore ->
    withMemoryType ty $ \pMemTy -> do
      p <- newForeignPtr Raw.deleteMemoryPtr =<< Raw.newMemory pStore pMemTy
      pure $ Memory { memoryStore = s
                    , memoryPtr = p
                    }

-- | Returns the size of the provided memory in pages.
memorySize :: Memory -> IO NumPages
memorySize mem =
  withForeignPtr (memoryPtr mem) (\pMem -> NumPages <$> Raw.memorySize pMem)

-- | Returns the size of the provided memory in bytes.
memoryDataSize :: Memory -> IO Word32
memoryDataSize mem =
  withForeignPtr (memoryPtr mem) (\pMem -> Raw.memoryDataSize pMem)

-- | Modify memory by writing the bytestring starting at specified offset.
--
-- The portion of the bytestring that spans beyond the memory data size is ignored.
writeMemory :: Memory -> Word32 -> ByteString -> IO ()
writeMemory mem offset bytes =
  unsafeUseMemory mem $ \pBytes memLen ->
    UBS.unsafeUseAsCStringLen bytes $ \(pStr, strLen) -> do
      let n = if memLen < offset then 0 else min (memLen - offset) (fromIntegral strLen)
      IBS.memcpy (plusPtr pBytes $ fromIntegral offset) (castPtr pStr) (fromIntegral n)

-- | Read the portion of memory specified by offset and size.
--
-- The read request is clipped to the memory size, i.e. the read returns the data located in
-- the intersection of ranges @[offset, offset + size)@ and @[0, memoryDataSize mem)@.
readMemory :: Memory -> Word32 -> Word32 -> IO ByteString
readMemory mem offset size =
  unsafeUseMemory mem $ \pBytes memLen -> do
    let n = fromIntegral $ if memLen < offset then 0 else min size (memLen - offset)
    IBS.create n (\dst -> IBS.memcpy dst (plusPtr pBytes $ fromIntegral offset) n)

-- | Directly applies an operation to the memory contents.
--
-- The operation  accepts a pointer to memory data and the number of
-- bytes available.
unsafeUseMemory :: Memory -> (Ptr Word8 -> Word32 -> IO a) -> IO a
unsafeUseMemory mem action =
  withForeignPtr (memoryPtr mem) $ \pMem -> do
    pData <- Raw.memoryData pMem
    pSize <- Raw.memoryDataSize pMem
    action pData pSize

-- | Attempts to grow the provided memory by delta memory pages.
-- This function is similar to @memory.grow@ Wasm instruction.
-- Returns @true@ if grow succeeded, @false@ otherwise.
growMemory :: Memory -> NumPages -> IO Bool
growMemory mem delta =
  withForeignPtr (memoryPtr mem) $ \pMem ->
    toBool <$> Raw.growMemory pMem (numPagesToWord32 delta)
  where toBool (CBool b) = b /= 0

deleteTrap :: Ptr Raw.WasmTrap -> IO ()
deleteTrap ptr = when (ptr /= nullPtr) $ Raw.deleteTrap ptr

withExtern :: Extern -> (Ptr Raw.WasmExtern -> IO a) -> IO a
withExtern extern action =
  case extern of
    ExternFunc f -> withForeignPtr (funcPtr f) (\fp -> Raw.funcAsExtern fp >>= action)
    ExternGlobal g -> withForeignPtr (globalPtr g) (\gp -> Raw.globalAsExtern gp >>= action)
    ExternTable t -> withForeignPtr (tablePtr t) (\tp -> Raw.tableAsExtern tp >>= action)
    ExternMemory m -> withForeignPtr (memoryPtr m) (\mp -> Raw.memoryAsExtern mp >>= action)

withExternArray :: [Extern] -> (Ptr (Ptr Raw.WasmExtern) -> IO a) -> IO a
withExternArray externs action = go externs []
  where go (x:xs) ptrs = withExtern x (\p -> go xs (p:ptrs))
        go [] ptrs     = withArray (reverse ptrs) action

getExtern :: Store -> Ptr Raw.WasmExtern -> IO Extern
getExtern store ptr = do
  kind <- Raw.externKind ptr
  if | kind == Raw.externKindFunc -> ExternFunc <$> (Raw.externAsFunc ptr >>= getFunc)
     | kind == Raw.externKindGlobal -> ExternGlobal <$> (Raw.externAsGlobal ptr >>= getGlobal)
     | kind == Raw.externKindTable -> ExternTable <$> (Raw.externAsTable ptr >>= getTable)
     | kind == Raw.externKindMemory -> ExternMemory <$> (Raw.externAsMemory ptr >>= getMemory)
     | otherwise -> throwIO $ UnknownExternKind kind
  where getFunc p = Func store <$> (Raw.copyFunc p >>= newForeignPtr Raw.deleteFuncPtr)
        getGlobal p = Global store <$> (Raw.copyGlobal p >>= newForeignPtr Raw.deleteGlobalPtr)
        getTable p = Table store <$> (Raw.copyTable p >>= newForeignPtr Raw.deleteTablePtr)
        getMemory p = Memory store <$> (Raw.copyMemory p >>= newForeignPtr Raw.deleteMemoryPtr)


setValue :: Ptr Raw.WasmValue -> Value -> IO ()
setValue ptr v = case v of
                   I32 x -> Raw.setValueI32 ptr x
                   I64 x -> Raw.setValueI64 ptr x
                   F32 x -> Raw.setValueF32 ptr (CFloat x)
                   F64 x -> Raw.setValueF64 ptr (CDouble x)

makeValueArray :: [Value] -> IO (Ptr Raw.WasmValue)
makeValueArray vals = do
  let n = length vals
  arr <- Raw.newValueArray (fromIntegral n)
  setValueArray vals arr
  pure arr

setValueArray :: [Value] -> Ptr Raw.WasmValue -> IO ()
setValueArray vals arr = do
  let n = length vals
  for_ (zip [0..] vals) $ \(i, v) -> do
    p <- Raw.indexValueArray arr (fromIntegral i)
    setValue p v

getValue :: Ptr Raw.WasmValue -> IO Value
getValue ptr = do
  kind <- Raw.valueKind ptr
  if | kind == Raw.valueKindI32 -> I32 <$> Raw.getValueI32 ptr
     | kind == Raw.valueKindI64 -> I64 <$> Raw.getValueI64 ptr
     | kind == Raw.valueKindF32 -> F32 . coerce <$> Raw.getValueF32 ptr
     | kind == Raw.valueKindF64 -> F64 . coerce <$> Raw.getValueF64 ptr
     | otherwise -> throwIO $ UnknownValueKind kind

getValueArray :: Int -> Ptr Raw.WasmValue -> IO [Value]
getValueArray n arr =
  for [0 .. n-1] $ \i -> do
    p <- Raw.indexValueArray arr (fromIntegral i)
    getValue p

getExternType :: Ptr Raw.WasmExternType -> IO ExternType
getExternType p = do
  kind <- Raw.externTypeKind p
  if | kind == Raw.externKindFunc -> ExternFuncType <$> (getFuncType =<< Raw.externTypeAsFuncType p)
     | kind == Raw.externKindGlobal -> ExternGlobalType <$> (getGlobalType =<< Raw.externTypeAsGlobalType p)
     | kind == Raw.externKindTable -> ExternTableType <$> (getTableType =<< Raw.externTypeAsTableType p)
     | kind == Raw.externKindMemory -> ExternMemoryType <$> (getMemoryType =<< Raw.externTypeAsMemoryType p)
     | otherwise -> throwIO $ UnknownExternKind kind

getLimits :: Ptr Raw.WasmLimits -> IO Limits
getLimits p = Limits <$> Raw.limitsMin p <*> Raw.limitsMax p

getFuncType :: Ptr Raw.WasmFuncType -> IO FuncType
getFuncType p = FuncType <$> (getTypes =<< Raw.funcTypeParams p)
                         <*> (getTypes =<< Raw.funcTypeResults p)
  where getTypes typeVec = do
          n <- Raw.valueTypeVecSize typeVec
          for [1 .. n] $ \i -> do
            t <- Raw.indexValueTypeVec typeVec (fromIntegral $ pred i)
            getValueType t

makeFuncType :: FuncType -> IO (Ptr Raw.WasmFuncType)
makeFuncType ft = do
  bracket (mkTypeVec $ funcTypeParams ft) Raw.deepFreeValueTypeVec $ \params ->
    bracket (mkTypeVec $ funcTypeResults ft) Raw.deepFreeValueTypeVec $ \results ->
      Raw.newFuncType params results

  where mkTypeVec types = do
          let n = length types
          vecPtr <- Raw.newUninitValueTypeVec (fromIntegral n)
          for_ (zip [0..] types) $ \(i, t) -> do
            valTypePtr <- newValueType t
            Raw.setValueTypeVec vecPtr (fromIntegral i) valTypePtr
          pure vecPtr

getTableType :: Ptr Raw.WasmTableType -> IO TableType
getTableType p = do
  limits <- Raw.tableTypeLimits p >>= getLimits
  typ <- Raw.tableTypeElement p >>= getValueType
  pure $ TableType { tableTypeLimits = limits
                   , tableTypeElement = typ
                   }

getValueType :: Ptr Raw.WasmValueType -> IO ValueType
getValueType p = Raw.valueTypeKind p >>= classifyValueType
  where classifyValueType v | v == Raw.valueKindI32 = pure I32Type
                            | v == Raw.valueKindI64 = pure I64Type
                            | v == Raw.valueKindF32 = pure F32Type
                            | v == Raw.valueKindF64 = pure F64Type
                            | v == Raw.valueKindAnyRef = pure AnyRefType
                            | v == Raw.valueKindFuncRef = pure FuncRefType
                            | otherwise = throwIO $ UnknownValueKind v

newValueType :: ValueType -> IO (Ptr Raw.WasmValueType)
newValueType v = Raw.newValueType kind
  where kind = case v of
                 I32Type     -> Raw.valueKindI32
                 I64Type     -> Raw.valueKindI64
                 F32Type     -> Raw.valueKindF32
                 F64Type     -> Raw.valueKindF64
                 AnyRefType  -> Raw.valueKindAnyRef
                 FuncRefType -> Raw.valueKindFuncRef

getGlobalType :: Ptr Raw.WasmGlobalType -> IO GlobalType
getGlobalType p = GlobalType <$> (Raw.globalTypeContent p >>= getValueType)
                             <*> (Raw.globalTypeMutability p >>= classifyMutability)
  where classifyMutability m | m == Raw.mutabilityConst = pure Const
                             | m == Raw.mutabilityVar = pure Var
                             | otherwise = throwIO $ UnknownMutability m

getMemoryType :: Ptr Raw.WasmMemoryType -> IO MemoryType
getMemoryType p = MemoryType <$> (getLimits =<< Raw.memoryTypeLimits p)

withLimits :: Limits -> (Ptr Raw.WasmLimits -> IO a) -> IO a
withLimits l action = do
  bracket Raw.newLimits Raw.deleteLimits $ \pLim -> do
    Raw.limitsSet pLim (limitsMin l) (limitsMax l)
    action pLim

withMemoryType :: MemoryType -> (Ptr Raw.WasmMemoryType -> IO a) -> IO a
withMemoryType ty action =
  withLimits (memoryTypeLimits ty) $ \pLim ->
    bracket (Raw.newMemoryType pLim) Raw.deleteMemoryType action

bsToVec :: ByteString -> IO (Ptr Raw.WasmByteVec)
bsToVec bs =
  UBS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    Raw.deepAllocateByteVec (fromIntegral len) ptr

withByteVec :: ByteString -> (Ptr Raw.WasmByteVec -> IO a) -> IO a
withByteVec bs = bracket (bsToVec bs) Raw.deepFreeByteVec

vecToBs :: Ptr Raw.WasmByteVec -> IO ByteString
vecToBs ptr = do
  (bytesPtr, size) <- vecToCStringLen ptr
  IBS.create size (\buf -> IBS.memcpy buf (castPtr bytesPtr) size)

vecToCStringLen :: Ptr Raw.WasmByteVec -> IO (Ptr CChar, Int)
vecToCStringLen ptr = (,) <$> Raw.byteVecData ptr <*> (fromIntegral <$> Raw.byteVecSize ptr)

vecToName :: Ptr Raw.WasmByteVec -> IO Text
vecToName ptr = vecToCStringLen ptr >>= T.peekCStringLen

newTrap :: Store -> ByteString -> IO (Ptr Raw.WasmTrap)
newTrap store bs = withForeignPtr (storePtr store) $ \sPtr ->
    withByteVec bs $ \msg -> Raw.newTrap sPtr msg

