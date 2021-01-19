#include <stdlib.h>
#include "hwasm.h"

wasm_byte_vec_t* hwasm_byte_vec_deep_allocate(size_t size, char* data) {
  wasm_byte_vec_t* buf = calloc(1, sizeof(wasm_byte_vec_t));
  wasm_byte_vec_new(buf, size, data);
  return buf;
}

wasm_byte_vec_t* hwasm_byte_vec_allocate_empty() {
  wasm_byte_vec_t* buf = calloc(1, sizeof(wasm_byte_vec_t));
  wasm_byte_vec_new_empty(buf);
  return buf;
}

void hwasm_byte_vec_deep_free(wasm_byte_vec_t* ptr) {
  wasm_byte_vec_delete(ptr);
  free(ptr);
}

size_t hwasm_byte_vec_size(wasm_byte_vec_t *v) {
  return v->size;
}

char* hwasm_byte_vec_data(wasm_byte_vec_t *v) {
  return v->data;
}

void hwasm_set_value_i32(wasm_val_t *dst, int32_t value) {
  dst->kind = WASM_I32;
  dst->of.i32 = value;
}

void hwasm_set_value_i64(wasm_val_t *dst, int64_t value) {
  dst->kind = WASM_I64;
  dst->of.i64 = value;
}
void hwasm_set_value_f32(wasm_val_t *dst, float32_t value) {
  dst->kind = WASM_F32;
  dst->of.f32 = value;
}
void hwasm_set_value_f64(wasm_val_t *dst, float64_t value) {
  dst->kind = WASM_F64;
  dst->of.f64 = value;
}

size_t hwasm_value_size() {
  return sizeof(wasm_val_t);
}

uint8_t hwasm_value_kind(wasm_val_t *src) {
  return src->kind;
}

int32_t hwasm_get_value_i32(wasm_val_t *src) {
  return src->of.i32;
}

int64_t hwasm_get_value_i64(wasm_val_t *src) {
  return src->of.i64;
}

float32_t hwasm_get_value_f32(wasm_val_t *src) {
  return src->of.f32;
}

float64_t hwasm_get_value_f64(wasm_val_t *src) {
  return src->of.f64;
}

wasm_val_t* hwasm_value_array_new(size_t size) {
  return calloc(size, sizeof(wasm_val_t));
}

void hwasm_value_array_delete(wasm_val_t *arr) {
  free(arr);
}

wasm_val_t* hwasm_value_array_index(wasm_val_t src[], ptrdiff_t idx) {
  return &src[idx];
}

// Export type vec

size_t hwasm_exporttype_vec_size(wasm_exporttype_vec_t *vec) {
  return vec->size;
}

wasm_exporttype_t* hwasm_exporttype_vec_index(wasm_exporttype_vec_t *vec, ptrdiff_t idx) {
  return vec->data[idx];
}

wasm_exporttype_vec_t* hwasm_allocate_exporttype_vec() {
  wasm_exporttype_vec_t* v = calloc(1, sizeof(wasm_exporttype_vec_t));
  wasm_exporttype_vec_new_empty(v);
  return v;
}

void hwasm_deep_free_exporttype_vec(wasm_exporttype_vec_t* v) {
  wasm_exporttype_vec_delete(v);
  free(v);
}

// Limits

uint32_t hwasm_limits_min(const wasm_limits_t *limits) {
  return limits->min;
}

uint32_t hwasm_limits_max(const wasm_limits_t *limits) {
  return limits->max;
}

void hwasm_limits_set(wasm_limits_t *limits, uint32_t min, uint32_t max) {
  limits->min = min;
  limits->max = max;
}

wasm_limits_t* hwasm_limits_new() {
  wasm_limits_t* limits = calloc(1, sizeof(wasm_limits_t));
  limits->max = wasm_limits_max_default;
  return limits;
}

void hwasm_limits_delete(wasm_limits_t *limits) {
  free(limits);
}

// Valtype vec

const wasm_valtype_t* hwasm_valtype_vec_index(const wasm_valtype_vec_t *v, ptrdiff_t idx) {
  return v->data[idx];
}

void hwasm_valtype_vec_set(wasm_valtype_vec_t *v, ptrdiff_t idx, wasm_valtype_t* val) {
  v->data[idx] = val;
}

size_t hwasm_valtype_vec_size(const wasm_valtype_vec_t *v) {
  return v->size;
}

wasm_valtype_vec_t* hwasm_valtype_vec_allocate_uninit(size_t size) {
  wasm_valtype_vec_t *v = calloc(1, sizeof(wasm_valtype_vec_t));
  wasm_valtype_vec_new_uninitialized(v, size);
  return v;
}

void hwasm_valtype_vec_deep_free(wasm_valtype_vec_t* v) {
  wasm_valtype_vec_delete(v);
  free(v);
}

// Import type vec

size_t hwasm_importtype_vec_size(wasm_importtype_vec_t *vec) {
  return vec->size;
}

wasm_importtype_t* hwasm_importtype_vec_index(wasm_importtype_vec_t *vec, ptrdiff_t idx) {
  return vec->data[idx];
}

wasm_importtype_vec_t* hwasm_allocate_importtype_vec() {
  wasm_importtype_vec_t* v = calloc(1, sizeof(wasm_importtype_vec_t));
  wasm_importtype_vec_new_empty(v);
  return v;
}

void hwasm_deep_free_importtype_vec(wasm_importtype_vec_t* v) {
  wasm_importtype_vec_delete(v);
  free(v);
}

// Extport vec

const wasm_extern_t* hwasm_extern_vec_index(const wasm_extern_vec_t *v, ptrdiff_t idx) {
  return v->data[idx];
}

void hwasm_extern_vec_set(wasm_extern_vec_t *v, ptrdiff_t idx, wasm_extern_t* val) {
  v->data[idx] = val;
}

size_t hwasm_extern_vec_size(const wasm_extern_vec_t *v) {
  return v->size;
}

wasm_extern_vec_t* hwasm_extern_vec_allocate_uninit(size_t size) {
  wasm_extern_vec_t *v = calloc(1, sizeof(wasm_extern_vec_t));
  wasm_extern_vec_new_uninitialized(v, size);
  return v;
}

void hwasm_extern_vec_deep_free(wasm_extern_vec_t* v) {
  wasm_extern_vec_delete(v);
  free(v);
}

void hwasm_extern_vec_free(wasm_extern_vec_t* v) {
  free(v);
}
