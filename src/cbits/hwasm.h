#ifndef HWASM_H
#define HWASM_H

#include "wasm.h"

// Byte vectors

wasm_byte_vec_t* hwasm_byte_vec_deep_allocate(size_t size, char* data);
wasm_byte_vec_t* hwasm_byte_vec_allocate_empty();
void   hwasm_byte_vec_deep_free(wasm_byte_vec_t*);
size_t hwasm_byte_vec_size(wasm_byte_vec_t *);
char*  hwasm_byte_vec_data(wasm_byte_vec_t *);

// Values

void hwasm_set_value_i32(wasm_val_t *dst, int32_t value);
void hwasm_set_value_i64(wasm_val_t *dst, int64_t value);
void hwasm_set_value_f32(wasm_val_t *dst, float32_t value);
void hwasm_set_value_f64(wasm_val_t *dst, float64_t value);

uint8_t hwasm_value_kind(wasm_val_t *src);
size_t hwasm_value_size();
int32_t hwasm_get_value_i32(wasm_val_t *dst);
int64_t hwasm_get_value_i64(wasm_val_t *dst);
float32_t hwasm_get_value_f32(wasm_val_t *dst);
float64_t hwasm_get_value_f64(wasm_val_t *dst);

wasm_val_t* hwasm_value_array_new(size_t size);
void hwasm_value_array_delete(wasm_val_t *arr);
wasm_val_t* hwasm_value_vec_index(wasm_val_t arr[], ptrdiff_t idx);

// Export type

size_t hwasm_exporttype_vec_size(wasm_exporttype_vec_t *vec);
wasm_exporttype_t* hwasm_exporttype_vec_index(wasm_exporttype_vec_t *vec, ptrdiff_t idx);
wasm_exporttype_vec_t* hwasm_allocate_exporttype_vec();
void hwasm_deep_free_exporttype_vec(wasm_exporttype_vec_t* v);

// Import type vec

size_t hwasm_importtype_vec_size(wasm_importtype_vec_t *vec);
wasm_importtype_t* hwasm_importtype_vec_index(wasm_importtype_vec_t *vec, ptrdiff_t idx);
wasm_importtype_vec_t* hwasm_allocate_importtype_vec();
void hwasm_deep_free_importtype_vec(wasm_importtype_vec_t* v);

// Limits

uint32_t hwasm_limits_min(const wasm_limits_t*);
uint32_t hwasm_limits_max(const wasm_limits_t*);
void hwasm_limits_set(wasm_limits_t*, uint32_t, uint32_t);
wasm_limits_t* hwasm_limits_new();
void hwasm_limits_delete(wasm_limits_t*);

// Value type vec

const wasm_valtype_t* hwasm_valtype_vec_index(const wasm_valtype_vec_t *v, ptrdiff_t idx);
void hwasm_valtype_vec_set(wasm_valtype_vec_t *v, ptrdiff_t idx, wasm_valtype_t* val);
size_t hwasm_valtype_vec_size(const wasm_valtype_vec_t *v);
wasm_valtype_vec_t* hwasm_valtype_vec_allocate_uninit(size_t);
void hwasm_valtype_vec_deep_free(wasm_valtype_vec_t* v);

// Extern vec

const wasm_extern_t* hwasm_extern_vec_index(const wasm_extern_vec_t *v, ptrdiff_t idx);
void hwasm_extern_vec_set(wasm_extern_vec_t *v, ptrdiff_t idx, wasm_extern_t* val);
size_t hwasm_extern_vec_size(const wasm_extern_vec_t *v);
wasm_extern_vec_t* hwasm_extern_vec_allocate_uninit(size_t);
void hwasm_extern_vec_deep_free(wasm_extern_vec_t* v);
void hwasm_extern_vec_free(wasm_extern_vec_t* v);

#endif // HWASM_H
