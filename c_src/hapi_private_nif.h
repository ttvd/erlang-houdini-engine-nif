/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL

#pragma once

#if !defined(HAPI_PRIVATE_NIF_H)
#define HAPI_PRIVATE_NIF_H

#include "erl_nif.h"
#include "xxhash.h"
#include "HAPI.h"


// Create an atom.
ERL_NIF_TERM hapi_priv_make_atom(ErlNifEnv* env, const char* atom_name);

// Create ok atom.
ERL_NIF_TERM hapi_priv_make_atom_ok(ErlNifEnv* env);

// Create nil atom.
ERL_NIF_TERM hapi_priv_make_atom_nil(ErlNifEnv* env);

// Create a boolean atom.
ERL_NIF_TERM hapi_priv_make_bool(ErlNifEnv* env, bool value);

// Create a list from float array.
ERL_NIF_TERM hapi_priv_make_float_list(ErlNifEnv* env, const float* data, uint32_t size);

// Create a list from double array.
ERL_NIF_TERM hapi_priv_make_double_list(ErlNifEnv* env, const double* data, uint32_t size);

// Create a list from int array.
ERL_NIF_TERM hapi_priv_make_int_list(ErlNifEnv* env, const int32_t* data, uint32_t size);

// Create a string.
ERL_NIF_TERM hapi_priv_make_string(ErlNifEnv* env, const char* string);

// Create a float.
ERL_NIF_TERM hapi_priv_make_float(ErlNifEnv* env, float value);

// Create a double
ERL_NIF_TERM hapi_priv_make_double(ErlNifEnv* env, double value);

// Create an integer
ERL_NIF_TERM hapi_priv_make_int(ErlNifEnv* env, int32_t value);

// Create a character
ERL_NIF_TERM hapi_priv_make_char(ErlNifEnv* env, char value);

// Check atom's value against passed value. Returns true if succeeds. Returns status of comparison by pointer.
bool hapi_priv_check_atom(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status);

// Return true or false by pointer if atom is nil.
bool hapi_priv_get_nil(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status);

// Get value of boolean atom. Return true if succeeds. Returns boolean by pointer.
bool hapi_priv_get_bool(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status);

// Get value of float. Return true if succeeds. Returns float by pointer.
bool hapi_priv_get_float(ErlNifEnv* env, const ERL_NIF_TERM term, float* data);

// Get value of double. Return true if succeeds. Returns double by pointer.
bool hapi_priv_get_double(ErlNifEnv* env, const ERL_NIF_TERM term, double* data);

// Get value of int. Return true if succeeds. Returns int by pointer.
bool hapi_priv_get_int(ErlNifEnv* env, const ERL_NIF_TERM term, int32_t* data);

// Get value of char. Return true if succeeds. Returns char by pointer.
bool hapi_priv_get_char(ErlNifEnv* env, const ERL_NIF_TERM term, char* data);

// Retrieve elements of double list into float array (by pointer). Return status.
bool hapi_priv_get_float_list(ErlNifEnv* env, const ERL_NIF_TERM term, float* data, uint32_t size);

// Retrieve elements of double list into double array (by pointer). Return status.
bool hapi_priv_get_double_list(ErlNifEnv* env, const ERL_NIF_TERM term, double* data, uint32_t size);

// Retrieve elements of integer list into int array (by pointer). Return status.
bool hapi_priv_get_int_list(ErlNifEnv* env, const ERL_NIF_TERM term, int32_t* data, uint32_t size);

// Return string and length by pointer, caller is responsible for clean up.
//bool hapi_priv_get_string_list(ErlNifEnv* env, const ERL_NIF_TERM term, char** string, uint32_t* string_length);


#endif //!defined(HAPI_PRIVATE_NIF_H)
