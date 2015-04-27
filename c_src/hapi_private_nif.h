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
ERL_NIF_TERM hapi_make_atom(ErlNifEnv* env, const char* atom_name);

// Create ok atom.
ERL_NIF_TERM hapi_make_atom_ok(ErlNifEnv* env);

// Create a boolean atom.
ERL_NIF_TERM hapi_make_atom_bool(ErlNifEnv* env, bool value);

// Create a list from float array.
ERL_NIF_TERM hapi_make_list_float(ErlNifEnv* env, uint32_t size, const float* data);

// Check atom's value against passed value. Returns true if succeeds. Returns status of comparison by pointer.
bool hapi_check_atom(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status);

#endif //!defined(HAPI_PRIVATE_NIF_H)
