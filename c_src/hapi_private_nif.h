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
ERL_NIF_TERM hapi_private_make_atom(ErlNifEnv* env, const char* atom_name);

// Create a boolean atom.
ERL_NIF_TERM hapi_private_make_atom_bool(ErlNifEnv* env, bool value);

#endif //!defined(HAPI_PRIVATE_NIF_H)
