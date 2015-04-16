#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_state_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_State* state)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
        {
            nif_success = false;
            goto label_cleanup;
        }

        switch(atom_hash)
        {
            // "hapi_state_ready"
            case 3648387321:
            {
                *state = HAPI_STATE_READY;
            }

            // "hapi_state_ready_with_fatal_errors"
            case 2824922871:
            {
                *state = HAPI_STATE_READY_WITH_FATAL_ERRORS;
            }

            // "hapi_state_ready_with_cook_errors"
            case 2342148041:
            {
                *state = HAPI_STATE_READY_WITH_COOK_ERRORS;
            }

            // "hapi_state_starting_cook"
            case 2553053177:
            {
                *state = HAPI_STATE_STARTING_COOK;
            }

            // "hapi_state_cooking"
            case 3690862542:
            {
                *state = HAPI_STATE_COOKING;
            }

            // "hapi_state_starting_load"
            case 3620837358:
            {
                *state = HAPI_STATE_STARTING_LOAD;
            }

            // "hapi_state_loading"
            case 849338667:
            {
                *state = HAPI_STATE_LOADING;
            }

            // "hapi_state_max"
            case 3467810446:
            {
                *state = HAPI_STATE_MAX;
            }

            // "hapi_state_max_ready_state"
            case 35607980:
            {
                *state = HAPI_STATE_MAX_READY_STATE;
            }

            default:
            {
                break;
            }
        }
    }

label_cleanup:

    return nif_success;
}


ERL_NIF_TERM hapi_enum_state_c_to_erl(ErlNifEnv* env, HAPI_State state)
{
    switch(state)
    {
        case HAPI_STATE_READY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_ready");
        }

        case HAPI_STATE_READY_WITH_FATAL_ERRORS:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_ready_with_fatal_errors");
        }

        case HAPI_STATE_READY_WITH_COOK_ERRORS:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_ready_with_cook_errors");
        }

        case HAPI_STATE_STARTING_COOK:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_starting_cook");
        }

        case HAPI_STATE_COOKING:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_cooking");
        }

        case HAPI_STATE_STARTING_LOAD:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_starting_load");
        }

        case HAPI_STATE_LOADING:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_loading");
        }

        case HAPI_STATE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_max");
        }

        /*
        case HAPI_STATE_MAX_READY_STATE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_state_max_ready_state");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
