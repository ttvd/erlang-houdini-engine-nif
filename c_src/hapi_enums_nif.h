#pragma once

#if !defined(HAPI_ENUMS_NIF_H)
#define HAPI_ENUMS_NIF_H

#include "erl_nif.h"
#include "HAPI.h"


// HAPI_Result conversions.
bool hapi_enum_result_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Result* result);
ERL_NIF_TERM hapi_enum_result_c_to_erl(ErlNifEnv* env, HAPI_Result result);


// HAPI_State conversions.
bool hapi_enum_state_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_State* state);
ERL_NIF_TERM hapi_enum_state_c_to_erl(ErlNifEnv* env, HAPI_State state);


#endif //!defined(HAPI_ENUMS_NIF_H)
