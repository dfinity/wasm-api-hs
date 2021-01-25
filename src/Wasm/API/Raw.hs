{-# LANGUAGE ForeignFunctionInterface #-}
module Wasm.API.Raw where

import           Data.Word   (Word8, Word32, Word64)
import           Foreign.C
import           Foreign.Ptr

data WasmEngine
data WasmStore
data WasmModule
data WasmInstance
data WasmExportType
data WasmExportTypeVec
data WasmImportType
data WasmImportTypeVec
data WasmExternType
data WasmFuncType
data WasmGlobalType
data WasmTableType
data WasmMemoryType
data WasmValueType
data WasmValueTypeVec
data WasmExtern
data WasmExternVec
data WasmFunc
data WasmGlobal
data WasmTable
data WasmLimits
data WasmMemory
data WasmTrap
data WasmByteVec
data WasmValue

type WasmCallback
  =  Ptr WasmValue
  -> Ptr WasmValue
  -> IO (Ptr WasmTrap)

-- Externs

externKindFunc, externKindGlobal, externKindTable, externKindMemory :: Word8
externKindModule, externKindInstance :: Word8
externKindFunc = 0
externKindGlobal = 1
externKindTable = 2
externKindMemory = 3
externKindModule = 4
externKindInstance = 5

foreign import ccall unsafe "wasm.h wasm_extern_kind"
  externKind :: Ptr WasmExtern -> IO Word8

foreign import ccall unsafe "wasm.h wasm_extern_as_func"
  externAsFunc :: Ptr WasmExtern -> IO (Ptr WasmFunc)

foreign import ccall unsafe "wasm.h wasm_extern_as_global"
  externAsGlobal :: Ptr WasmExtern -> IO (Ptr WasmGlobal)

foreign import ccall unsafe "wasm.h wasm_extern_as_table"
  externAsTable :: Ptr WasmExtern -> IO (Ptr WasmTable)

foreign import ccall unsafe "wasm.h wasm_extern_as_memory"
  externAsMemory :: Ptr WasmExtern -> IO (Ptr WasmMemory)

foreign import ccall unsafe "wasm.h wasm_func_as_extern"
  funcAsExtern :: Ptr WasmFunc -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm.h wasm_global_as_extern"
  globalAsExtern :: Ptr WasmGlobal -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm.h wasm_table_as_extern"
  tableAsExtern :: Ptr WasmTable -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm.h wasm_memory_as_extern"
  memoryAsExtern :: Ptr WasmMemory -> IO (Ptr WasmExtern)

-- Extern vec

foreign import ccall unsafe "hwasm.h hwasm_extern_vec_allocate_uninit"
  newUninitExternVec :: CSize -> IO (Ptr WasmExternVec)

foreign import ccall unsafe "hwasm.h hwasm_extern_vec_free"
  freeExternVec :: Ptr WasmExternVec -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_extern_vec_deep_free"
  deepFreeExternVec :: Ptr WasmExternVec -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_extern_vec_index"
  indexExternVec :: Ptr WasmExternVec -> CPtrdiff -> IO (Ptr WasmExtern)

foreign import ccall unsafe "hwasm.h hwasm_extern_vec_set"
  setExternVec :: Ptr WasmExternVec -> CPtrdiff -> Ptr WasmExtern -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_extern_vec_size"
  externVecSize :: Ptr WasmExternVec -> IO CSize

-- Functions

foreign import ccall "wrapper"
  mkWasmFuncCallback :: WasmCallback -> IO (FunPtr WasmCallback)

foreign import ccall "wasm.h wasm_func_new"
  newFunc :: Ptr WasmStore
          -> Ptr WasmFuncType
          -> FunPtr WasmCallback
          -> IO (Ptr WasmFunc)

foreign import ccall "wasm.h wasm_func_copy"
  copyFunc :: Ptr WasmFunc -> IO (Ptr WasmFunc)

foreign import ccall "wasm.h &wasm_func_delete"
  deleteFuncPtr :: FunPtr (Ptr WasmFunc -> IO ())

foreign import ccall "wasm.h wasm_func_call"
  callFunc :: Ptr WasmFunc -> Ptr WasmValue -> Ptr WasmValue -> IO (Ptr WasmTrap)

foreign import ccall "wasm.h wasm_func_param_arity"
  funcParamArity :: Ptr WasmFunc -> IO CSize

foreign import ccall "wasm.h wasm_func_result_arity"
  funcResultArity :: Ptr WasmFunc -> IO CSize

foreign import ccall "wasm.h wasm_functype_new"
  newFuncType :: Ptr WasmValueTypeVec -> Ptr WasmValueTypeVec -> IO (Ptr WasmFuncType)

-- Tables

foreign import ccall "wasm.h wasm_table_copy"
  copyTable:: Ptr WasmTable -> IO (Ptr WasmTable)

foreign import ccall "wasm.h &wasm_table_delete"
  deleteTablePtr :: FunPtr (Ptr WasmTable -> IO ())

-- Memory

foreign import ccall "wasm.h wasm_memory_copy"
  copyMemory :: Ptr WasmMemory -> IO (Ptr WasmMemory)

foreign import ccall "wasm.h &wasm_memory_delete"
  deleteMemoryPtr :: FunPtr (Ptr WasmMemory -> IO ())

foreign import ccall "wasm.h wasm_memory_new"
  newMemory :: Ptr WasmStore -> Ptr WasmMemoryType -> IO (Ptr WasmMemory)

foreign import ccall "wasm.h wasm_memory_size"
  memorySize :: Ptr WasmMemory -> IO Word32

foreign import ccall "wasm.h wasm_memory_data"
  memoryData :: Ptr WasmMemory -> IO (Ptr Word8)

foreign import ccall "wasm.h wasm_memory_data_size"
  memoryDataSize :: Ptr WasmMemory -> IO Word32

foreign import ccall "wasm.h wasm_memory_grow"
  growMemory :: Ptr WasmMemory -> Word32 -> IO CBool

foreign import ccall "wasm.h wasm_memorytype_new"
  newMemoryType :: Ptr WasmLimits -> IO (Ptr WasmMemoryType)

foreign import ccall "wasm.h wasm_memorytype_delete"
  deleteMemoryType :: Ptr WasmMemoryType -> IO ()

-- Traps

foreign import ccall unsafe "wasm.h wasm_trap_new"
  newTrap :: Ptr WasmStore -> Ptr WasmByteVec -> IO (Ptr WasmTrap)


foreign import ccall unsafe "wasm.h wasm_trap_delete"
  deleteTrap :: Ptr WasmTrap -> IO ()

foreign import ccall unsafe "wasm.h wasm_trap_message"
  trapMessage :: Ptr WasmTrap -> Ptr WasmByteVec -> IO ()

-- Extern types

foreign import ccall unsafe "wasm.h wasm_externtype_kind"
  externTypeKind :: Ptr WasmExternType -> IO Word8

foreign import ccall unsafe "wasm.h wasm_externtype_as_functype"
  externTypeAsFuncType :: Ptr WasmExternType -> IO (Ptr WasmFuncType)

foreign import ccall unsafe "wasm.h wasm_externtype_as_globaltype"
  externTypeAsGlobalType :: Ptr WasmExternType -> IO (Ptr WasmGlobalType)

foreign import ccall unsafe "wasm.h wasm_externtype_as_tabletype"
  externTypeAsTableType :: Ptr WasmExternType -> IO (Ptr WasmTableType)

foreign import ccall unsafe "wasm.h wasm_externtype_as_memorytype"
  externTypeAsMemoryType :: Ptr WasmExternType -> IO (Ptr WasmMemoryType)

foreign import ccall unsafe "wasm.h wasm_tabletype_limits"
  tableTypeLimits :: Ptr WasmTableType -> IO (Ptr WasmLimits)

foreign import ccall unsafe "wasm.h wasm_tabletype_element"
  tableTypeElement :: Ptr WasmTableType -> IO (Ptr WasmValueType)

foreign import ccall unsafe "wasm.h wasm_valtype_kind"
  valueTypeKind :: Ptr WasmValueType -> IO Word8

foreign import ccall unsafe "wasm.h wasm_valtype_new"
  newValueType :: Word8 -> IO (Ptr WasmValueType)

foreign import ccall unsafe "wasm.h wasm_memorytype_limits"
  memoryTypeLimits :: Ptr WasmMemoryType -> IO (Ptr WasmLimits)

foreign import ccall unsafe "wasm.h wasm_functype_params"
  funcTypeParams :: Ptr WasmFuncType -> IO (Ptr WasmValueTypeVec)

foreign import ccall unsafe "wasm.h wasm_functype_results"
  funcTypeResults :: Ptr WasmFuncType -> IO (Ptr WasmValueTypeVec)

-- Limits

foreign import ccall unsafe "hwasm.h hwasm_limits_min"
  limitsMin :: Ptr WasmLimits -> IO Word32

foreign import ccall unsafe "hwasm.h hwasm_limits_max"
  limitsMax :: Ptr WasmLimits -> IO Word32

foreign import ccall unsafe "hwasm.h hwasm_limits_set"
  limitsSet :: Ptr WasmLimits -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_limits_new"
  newLimits :: IO (Ptr WasmLimits)

foreign import ccall unsafe "hwasm.h hwasm_limits_set"
  deleteLimits :: Ptr WasmLimits -> IO ()

-- Byte vectors

foreign import ccall unsafe "hwasm.h hwasm_byte_vec_allocate_empty"
  allocateEmptyByteVec :: IO (Ptr WasmByteVec)

foreign import ccall unsafe "hwasm.h hwasm_byte_vec_deep_allocate"
  deepAllocateByteVec :: CSize -> Ptr CChar -> IO (Ptr WasmByteVec)

foreign import ccall unsafe "hwasm.h &hwasm_byte_vec_deep_free"
  deepFreeByteVecPtr :: FunPtr (Ptr WasmByteVec -> IO ())

foreign import ccall unsafe "hwasm.h hwasm_byte_vec_deep_free"
  deepFreeByteVec:: Ptr WasmByteVec -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_byte_vec_size"
  byteVecSize :: Ptr WasmByteVec -> IO CSize

foreign import ccall unsafe "hwasm.h hwasm_byte_vec_data"
  byteVecData :: Ptr WasmByteVec -> IO (Ptr CChar)

foreign import ccall unsafe "wasm.h wasm_byte_vec_new"
  newByteVec :: CSize -> Ptr CChar -> Ptr WasmByteVec -> IO ()

foreign import ccall unsafe "wasm.h &wasm_byte_vec_delete"
  deleteByteVecPtr :: FunPtr (Ptr WasmByteVec -> IO ())

foreign import ccall unsafe "wasm.h wasm_byte_vec_delete"
  deleteByteVec:: Ptr WasmByteVec -> IO ()

-- Value type vectors

foreign import ccall unsafe "hwasm.h hwasm_valtype_vec_allocate_uninit"
  newUninitValueTypeVec :: CSize -> IO (Ptr WasmValueTypeVec)

foreign import ccall unsafe "hwasm.h hwasm_valtype_vec_deep_free"
  deepFreeValueTypeVec :: Ptr WasmValueTypeVec -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_valtype_vec_index"
  indexValueTypeVec :: Ptr WasmValueTypeVec -> CPtrdiff -> IO (Ptr WasmValueType)

foreign import ccall unsafe "hwasm.h hwasm_valtype_vec_set"
  setValueTypeVec :: Ptr WasmValueTypeVec -> CPtrdiff -> Ptr WasmValueType -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_valtype_vec_size"
  valueTypeVecSize :: Ptr WasmValueTypeVec -> IO CSize

-- Export types

foreign import ccall unsafe "wasm.h wasm_exporttype_name"
  exportTypeName :: Ptr WasmExportType -> IO (Ptr WasmByteVec)

foreign import ccall unsafe "wasm.h wasm_exporttype_type"
  exportTypeType :: Ptr WasmExportType -> IO (Ptr WasmExternType)

-- Export type vectors

foreign import ccall unsafe "hwasm.h hwasm_exporttype_vec_size"
  exportTypeVecSize :: Ptr WasmExportTypeVec -> IO CSize 

foreign import ccall unsafe "hwasm.h hwasm_exporttype_vec_index"
  indexExportTypeVec :: Ptr WasmExportTypeVec -> CPtrdiff -> IO (Ptr WasmExportType)

foreign import ccall unsafe "wasm.h wasm_exporttype_vec_delete"
  deleteExportTypeVec :: Ptr WasmExportTypeVec -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_allocate_exporttype_vec"
  allocateExportTypeVec :: IO (Ptr WasmExportTypeVec)

foreign import ccall unsafe "hwasm.h hwasm_deep_free_exporttype_vec"
  deepFreeExportTypeVec :: Ptr WasmExportTypeVec -> IO ()

-- Import types

foreign import ccall unsafe "wasm.h wasm_importtype_module"
  importTypeModule :: Ptr WasmImportType -> IO (Ptr WasmByteVec)

foreign import ccall unsafe "wasm.h wasm_importtype_name"
  importTypeName :: Ptr WasmImportType -> IO (Ptr WasmByteVec)

foreign import ccall unsafe "wasm.h wasm_importtype_type"
  importTypeType :: Ptr WasmImportType -> IO (Ptr WasmExternType)

-- Import type vectors

foreign import ccall unsafe "hwasm.h hwasm_importtype_vec_size"
  importTypeVecSize :: Ptr WasmImportTypeVec -> IO CSize 

foreign import ccall unsafe "hwasm.h hwasm_importtype_vec_index"
  indexImportTypeVec :: Ptr WasmImportTypeVec -> CPtrdiff -> IO (Ptr WasmImportType)

foreign import ccall unsafe "wasm.h wasm_importtype_vec_delete"
  deleteImportTypeVec :: Ptr WasmImportTypeVec -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_allocate_importtype_vec"
  allocateImportTypeVec :: IO (Ptr WasmImportTypeVec)

foreign import ccall unsafe "hwasm.h hwasm_deep_free_importtype_vec"
  deepFreeImportTypeVec :: Ptr WasmImportTypeVec -> IO ()

-- Values

valueKindI32, valueKindI64, valueKindF32, valueKindF64 :: Word8
valueKindI32 = 0
valueKindI64 = 1
valueKindF32 = 2
valueKindF64 = 3

valueKindAnyRef, valueKindFuncRef :: Word8
valueKindAnyRef = 128
valueKindFuncRef = 129

foreign import ccall unsafe "hwasm.h hwasm_set_value_i32"
  setValueI32 :: Ptr WasmValue -> Word32 -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_set_value_i64"
  setValueI64 :: Ptr WasmValue -> Word64 -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_set_value_f32"
  setValueF32 :: Ptr WasmValue -> CFloat -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_set_value_f64"
  setValueF64 :: Ptr WasmValue -> CDouble -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_value_kind"
  valueKind :: Ptr WasmValue -> IO Word8

foreign import ccall unsafe "hwasm.h hwasm_get_value_i32"
  getValueI32 :: Ptr WasmValue -> IO Word32

foreign import ccall unsafe "hwasm.h hwasm_get_value_i64"
  getValueI64 :: Ptr WasmValue -> IO Word64

foreign import ccall unsafe "hwasm.h hwasm_get_value_f32"
  getValueF32 :: Ptr WasmValue -> IO CFloat

foreign import ccall unsafe "hwasm.h hwasm_get_value_f64"
  getValueF64 :: Ptr WasmValue -> IO CDouble

foreign import ccall unsafe "hwasm.h hwasm_value_array_new"
  newValueArray :: CSize -> IO (Ptr WasmValue)

foreign import ccall unsafe "hwasm.h hwasm_value_array_delete"
  deleteValueArray :: Ptr WasmValue -> IO ()

foreign import ccall unsafe "hwasm.h hwasm_value_array_index"
  indexValueArray :: Ptr WasmValue -> CPtrdiff -> IO (Ptr WasmValue)

-- Globals

mutabilityConst, mutabilityVar :: Word8
mutabilityConst = 0
mutabilityVar = 1

foreign import ccall unsafe "wasm.h wasm_global_get"
  getGlobal :: Ptr WasmGlobal -> Ptr WasmValue -> IO ()

foreign import ccall unsafe "wasm.h wasm_global_set"
  setGlobal :: Ptr WasmGlobal -> Ptr WasmValue -> IO ()

foreign import ccall unsafe "wasm.h wasm_global_copy"
  copyGlobal :: Ptr WasmGlobal -> IO (Ptr WasmGlobal)

foreign import ccall unsafe "wasm.h &wasm_global_delete"
  deleteGlobalPtr :: FunPtr (Ptr WasmGlobal -> IO ())

foreign import ccall unsafe "wasm.h wasm_globaltype_mutability"
  globalTypeMutability :: Ptr WasmGlobalType -> IO Word8

foreign import ccall unsafe "wasm.h wasm_globaltype_content"
  globalTypeContent :: Ptr WasmGlobalType -> IO (Ptr WasmValueType)

-- Engine, Store, Module, etc.

foreign import ccall unsafe "wasm.h wasm_engine_new"
  newEngine :: IO (Ptr WasmEngine)

foreign import ccall unsafe "wasm.h &wasm_engine_delete"
  deleteEngine :: FunPtr (Ptr WasmEngine -> IO ())

foreign import ccall unsafe "wasm.h wasm_store_new"
  newStore :: Ptr WasmEngine -> IO (Ptr WasmStore)

foreign import ccall unsafe "wasm.h wasm_store_delete"
  deleteStore:: Ptr WasmStore -> IO ()

foreign import ccall unsafe "wasm.h &wasm_store_delete"
  deleteStorePtr :: FunPtr (Ptr WasmStore -> IO ())

foreign import ccall unsafe "wasm.h wasm_module_new"
  newModule :: Ptr WasmStore -> Ptr WasmByteVec -> IO (Ptr WasmModule)

foreign import ccall unsafe "wasm.h wasm_module_validate"
  validateModule :: Ptr WasmStore -> Ptr WasmByteVec -> IO Bool

foreign import ccall unsafe "wasm.h wasm_module_exports"
  moduleExports :: Ptr WasmModule -> Ptr WasmExportTypeVec -> IO ()

foreign import ccall unsafe "wasm.h wasm_module_imports"
  moduleImports :: Ptr WasmModule -> Ptr WasmImportTypeVec -> IO ()

foreign import ccall unsafe "wasm.h &wasm_module_delete"
  deleteModule :: FunPtr (Ptr WasmModule -> IO ())

foreign import ccall unsafe "wasm.h wasm_instance_new"
  newInstance :: Ptr WasmStore -> Ptr WasmModule -> Ptr (Ptr WasmExtern) -> Ptr (Ptr WasmTrap) -> IO (Ptr WasmInstance)

foreign import ccall unsafe "wasm.h wasm_instance_exports"
  instanceExports :: Ptr WasmInstance -> Ptr WasmExternVec -> IO ()

foreign import ccall unsafe "wasm.h &wasm_instance_delete"
  deleteInstancePtr :: FunPtr (Ptr WasmInstance -> IO ())
