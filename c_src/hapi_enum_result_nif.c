#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool
hapi_enum_result_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Result* result)
{
    return true;
}


ERL_NIF_TERM
hapi_enum_result_c_to_erl(ErlNifEnv* env, HAPI_Result result)
{
    return hapi_private_make_atom(env, "ok");
}
