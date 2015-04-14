#include "erl_nif.h"
#include "HAPI.h"

#include <stdbool.h>


bool
hapi_enum_state_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_State* state)
{
    return false;
}


ERL_NIF_TERM
hapi_enum_state_c_to_erl(ErlNifEnv* env, HAPI_State state)
{

}
