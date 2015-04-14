#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool
hapi_enum_state_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_State* state)
{
    return false;
}


ERL_NIF_TERM
hapi_enum_state_c_to_erl(ErlNifEnv* env, HAPI_State state)
{
    return hapi_private_make_atom(env, "ok");
}
