{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  -- * Recoverable errors
  , InvalidModule
  , UnresolvedImport
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
  , call
  , wrapFunc
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
import           Data.Int                 (Int32, Int64)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Foreign        as T
import           Data.Traversable         (for)
import           Data.Typeable
import           Data.Word                (Word32, Word8)
import           System.IO.Unsafe         (unsafePerformIO)

import qualified Wasm.API.Raw             as Raw

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
    , limitxMax :: Word32
    } deriving (Show, Eq)

data Value
 = I32 Int32
 | I64 Int64
 | F32 Float
 | F64 Double
 deriving (Show, Eq)

data ValueType
  = TypeI32
  | TypeI64
  | TypeF32
  | TypeF64
  | TypeAnyRef
  | TypeFuncRef
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

newtype UnresolvedImport = UnresolvedImport (Text, Text) deriving (Show, Typeable)
instance Exception UnresolvedImport

newtype Trap = Trap Text deriving (Show, Typeable)
instance Exception Trap

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
-- function throws @InvalidModule@ exception.
newModule :: Store -> ByteString -> IO Module
newModule store moduleBytes =
  withByteVec moduleBytes $ \byteVecPtr ->
    withForeignPtr (storePtr store) $ \ptr -> do
        rawModulePtr <- Raw.newModule ptr byteVecPtr

        when (rawModulePtr == nullPtr) $
          throwIO $ InvalidModule moduleBytes

        modulePtr <- newForeignPtr Raw.deleteModule rawModulePtr

        pure $ Module { modulePtr = modulePtr, moduleStore = store }

-- | Instantiates a @Module@ with the provided imports into the @Store@.
--
-- Instantiation includes invoking the start function of a Wasm
-- module.  If @start@ traps then this function throws @Trap@
-- exception.
--
-- The provided imports must contain at least all the imports returned
-- by the corresponding @moduleImports@, otherwise this function
-- throws @UnresolvedImport@ exception.
newInstance :: Store -> Module -> [(Text, Text, Extern)] -> IO Instance
newInstance s m deps = do
  imports <- moduleImports m
  let externs = resolveImports imports deps
  withForeignPtr (storePtr s) $ \sPtr ->
    withForeignPtr (modulePtr m) $ \mPtr ->
      withExternArray externs $ \arr ->
        alloca $ \trapPtrPtr -> do
          p <- Raw.newInstance sPtr mPtr arr trapPtrPtr
          trapPtr <- peek trapPtrPtr
          throwOnTrap trapPtr `finally` deleteTrap trapPtr
          fptr <- newForeignPtr Raw.deleteInstancePtr p
          pure $ Instance { instancePtr = fptr
                          , instanceStore = s
                          , instanceModule = m
                          }

throwOnTrap :: Ptr Raw.WasmTrap -> IO ()
throwOnTrap trapPtr =
  when (trapPtr /= nullPtr) $
    bracket Raw.allocateEmptyByteVec Raw.deepFreeByteVec $ \outPtr -> do
      Raw.trapMessage trapPtr outPtr
      msg <- vecToName outPtr
      throwIO $ Trap msg

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

resolveImports :: [(Text, Text, ExternType)] -> [(Text, Text, Extern)] -> [Extern]
resolveImports imports deps = map findDep imports
  where findDep (mod, name, _) =
          case find (\(mod', name', _) -> mod == mod' && name == name') deps of
            Just (_, _, v) -> v -- TODO: check that type of v matches t
            Nothing        -> throw $ UnresolvedImport (mod, name)

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
-- This function throws @Trap@ exception on the following occasions:
--   - Any of the arguments do not have the correct type.
--   - Any of the arguments come from a different store than the @Func@ provided.
--   - Function execution traps.
call :: Func -> [Value] -> IO [Value]
call f args =
  withForeignPtr (funcPtr f) $ \fPtr -> do
    paramArity <- Raw.funcParamArity fPtr
    resultArity <- Raw.funcResultArity fPtr
    bracket (makeValueArray args) Raw.deleteValueArray $ \paramsPtr ->
      bracket (Raw.newValueArray resultArity) Raw.deleteValueArray $ \resultsPtr ->
        bracket (Raw.callFunc fPtr paramsPtr resultsPtr) deleteTrap $ \trapPtr -> do
          throwOnTrap trapPtr
          getValueArray (fromIntegral resultArity) resultsPtr

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

        w store _env inValPtr outValPtr = do
          args <- getValueArray inArity inValPtr
          handle (\(e :: SomeException) -> newTrap store (T.encodeUtf8 $ T.pack $ show e)) $ do
            results <- f args
            when (length results /= outArity) $
              throwIO $ WrongResultsArity { declaredResultsArity = outArity
                                          , actualResultsArity = length results
                                          }
            setValueArray results outValPtr
            pure nullPtr

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
  let r | kind == Raw.externKindFunc = ExternFunc <$> (Raw.externAsFunc ptr >>= getFunc)
        | kind == Raw.externKindGlobal = ExternGlobal <$> (Raw.externAsGlobal ptr >>= getGlobal)
        | kind == Raw.externKindTable = ExternTable <$> (Raw.externAsTable ptr >>= getTable)
        | kind == Raw.externKindMemory = ExternMemory <$> (Raw.externAsMemory ptr >>= getMemory)
        | otherwise = throwIO $ UnknownExternKind kind
  r
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
  let r | kind == Raw.valueKindI32 = I32 <$> Raw.getValueI32 ptr
        | kind == Raw.valueKindI64 = I64 <$> Raw.getValueI64 ptr
        | kind == Raw.valueKindF32 = F32 . coerce <$> Raw.getValueF32 ptr
        | kind == Raw.valueKindF64 = F64 . coerce <$> Raw.getValueF64 ptr
        | otherwise = throwIO $ UnknownValueKind kind
  r

getValueArray :: Int -> Ptr Raw.WasmValue -> IO [Value]
getValueArray n arr =
  for [0 .. n-1] $ \i -> do
    p <- Raw.indexValueArray arr (fromIntegral i)
    getValue p

getExternType :: Ptr Raw.WasmExternType -> IO ExternType
getExternType p = do
  kind <- Raw.externTypeKind p
  let r | kind == Raw.externKindFunc = ExternFuncType <$> (getFuncType =<< Raw.externTypeAsFuncType p)
        | kind == Raw.externKindGlobal = ExternGlobalType <$> (getGlobalType =<< Raw.externTypeAsGlobalType p)
        | kind == Raw.externKindTable = ExternTableType <$> (getTableType =<< Raw.externTypeAsTableType p)
        | kind == Raw.externKindMemory = ExternMemoryType <$> (getMemoryType =<< Raw.externTypeAsMemoryType p)
        | otherwise = throwIO $ UnknownExternKind kind
  r

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
  where classifyValueType v | v == Raw.valueKindI32 = pure TypeI32
                            | v == Raw.valueKindI64 = pure TypeI64
                            | v == Raw.valueKindF32 = pure TypeF32
                            | v == Raw.valueKindF64 = pure TypeF64
                            | v == Raw.valueKindAnyRef = pure TypeAnyRef
                            | v == Raw.valueKindFuncRef = pure TypeFuncRef
                            | otherwise = throwIO $ UnknownValueKind v

newValueType :: ValueType -> IO (Ptr Raw.WasmValueType)
newValueType v = Raw.newValueType kind
  where kind = case v of
                 TypeI32     -> Raw.valueKindI32
                 TypeI64     -> Raw.valueKindI64
                 TypeF32     -> Raw.valueKindF32
                 TypeF64     -> Raw.valueKindF64
                 TypeAnyRef  -> Raw.valueKindAnyRef
                 TypeFuncRef -> Raw.valueKindFuncRef

getGlobalType :: Ptr Raw.WasmGlobalType -> IO GlobalType
getGlobalType p = GlobalType <$> (Raw.globalTypeContent p >>= getValueType)
                             <*> (Raw.globalTypeMutability p >>= classifyMutability)
  where classifyMutability m | m == Raw.mutabilityConst = pure Const
                             | m == Raw.mutabilityVar = pure Var
                             | otherwise = throwIO $ UnknownMutability m

getMemoryType :: Ptr Raw.WasmMemoryType -> IO MemoryType
getMemoryType p = MemoryType <$> (getLimits =<< Raw.memoryTypeLimits p)

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

