#pragma once

#if !defined(HAPI_PRIVATE_NIF_H)
#define HAPI_PRIVATE_NIF_H

#include "erl_nif.h"
#include "xxhash.h"
#include "HAPI.h"


// Create an atom.
ERL_NIF_TERM hapi_private_make_atom(ErlNifEnv* env, const char* atom_name);

// Create an atom with hash.
ERL_NIF_TERM hapi_private_make_hash_tuple(ErlNifEnv* env, const char* atom_name);

// Return true or false by pointer if given atom has a specified value.
bool hapi_private_check_atom_value(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status);


#endif //!defined(HAPI_PRIVATE_NIF_H)
