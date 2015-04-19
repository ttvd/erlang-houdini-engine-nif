#pragma once

#if !defined(HAPI_PRIVATE_NIF_H)
#define HAPI_PRIVATE_NIF_H

#include "erl_nif.h"
#include "xxhash.h"
#include "HAPI.h"


// Create an atom.
ERL_NIF_TERM hapi_private_make_atom(ErlNifEnv* env, const char* atom_name);

// Create a boolean atom.
ERL_NIF_TERM hapi_private_make_atom_bool(ErlNifEnv* env, bool value);

// Create an atom with hash.
ERL_NIF_TERM hapi_private_make_hash_tuple(ErlNifEnv* env, const char* atom_name);

// Create a result / int tuple if hapi call was successful, otherwise return hapi result atom.
ERL_NIF_TERM hapi_private_make_result_tuple_int(ErlNifEnv* env, HAPI_Result result, int32_t value);

// Create a result / double tuple if hapi call was successful, otherwise return hapi result atom.
ERL_NIF_TERM hapi_private_make_result_tuple_double(ErlNifEnv* env, HAPI_Result result, double value);

// Create a result / string tuple if hapi call was successful, otherwise return hapi result atom.
ERL_NIF_TERM hapi_private_make_result_tuple_string(ErlNifEnv* env, HAPI_Result result, const char* value);

// Create a result / bool tuple if hapi call was successful, otherwise return hapi result atom.
ERL_NIF_TERM hapi_private_make_result_tuple_bool(ErlNifEnv* env, HAPI_Result result, bool value);

// Return true or false by pointer if given atom has a specified value.
bool hapi_private_check_atom_value(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status);

// Return true or false by pointer if atom is nil.
bool hapi_private_check_nil(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status);

// Return boolean value by pointer.
bool hapi_private_check_bool(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status);

// Return string and length by pointer, caller is responsible for clean up.
bool hapi_private_get_string(ErlNifEnv* env, const ERL_NIF_TERM term, char** string, uint32_t* string_length);


#endif //!defined(HAPI_PRIVATE_NIF_H)
