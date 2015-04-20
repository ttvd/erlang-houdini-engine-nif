/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL

#pragma once

#if !defined(HAPI_HELPERS_NIF_H)
#define HAPI_HELPERS_NIF_H

#include "erl_nif.h"


// Exported helper functions.
extern ERL_NIF_TERM hapi_hash_enum_value_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_check_enum_value_hash_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


#endif //!defined(HAPI_HELPERS_NIF_H)
