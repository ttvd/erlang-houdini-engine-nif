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
    switch(state)
    {
        case HAPI_STATUS_CALL_RESULT:
        {
            return hapi_private_make_atom(env, "hapi_status_call_result");
        }

        case HAPI_STATUS_COOK_RESULT:
        {
            return hapi_private_make_atom(env, "hapi_status_cook_result");
        }

        case HAPI_STATUS_COOK_STATE:
        {
            return hapi_private_make_atom(env, "hapi_status_cook_state");
        }

        case HAPI_STATUS_MAX:
        {
            return hapi_private_make_atom(env, "hapi_status_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
