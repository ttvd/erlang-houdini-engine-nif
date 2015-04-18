#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <stdio.h>


bool hapi_enum_state_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_State* state)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    char* atom_value = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
        {
            nif_success = false;
            goto label_cleanup;
        }
    }
    else if(enif_is_atom(env, term))
    {
        uint32_t atom_len = 0;

        if(!enif_get_atom_length(env, term, &atom_len, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        atom_value = malloc(atom_len + 1);
        memset(atom_value, 0, atom_len + 1);

        if(!enif_get_atom(env, term, atom_value, atom_len + 1, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        atom_hash = XXH32(atom_value, strlen(atom_value), 0);
    }
    else if(!enif_get_uint(env, term, &atom_hash))
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
            break;
        }

        // "hapi_state_ready_with_fatal_errors"
        case 2824922871:
        {
            *state = HAPI_STATE_READY_WITH_FATAL_ERRORS;
            break;
        }

        // "hapi_state_ready_with_cook_errors"
        case 2342148041:
        {
            *state = HAPI_STATE_READY_WITH_COOK_ERRORS;
            break;
        }

        // "hapi_state_starting_cook"
        case 2553053177:
        {
            *state = HAPI_STATE_STARTING_COOK;
            break;
        }

        // "hapi_state_cooking"
        case 3690862542:
        {
            *state = HAPI_STATE_COOKING;
            break;
        }

        // "hapi_state_starting_load"
        case 3620837358:
        {
            *state = HAPI_STATE_STARTING_LOAD;
            break;
        }

        // "hapi_state_loading"
        case 849338667:
        {
            *state = HAPI_STATE_LOADING;
            break;
        }

        // "hapi_state_max"
        case 3467810446:
        {
            *state = HAPI_STATE_MAX;
            break;
        }

        // "hapi_state_max_ready_state"
        case 35607980:
        {
            *state = HAPI_STATE_MAX_READY_STATE;
            break;
        }

        default:
        {
            nif_success = false;
            break;
        }
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

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
