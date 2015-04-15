#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool
hapi_enum_state_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_State* state)
{
    bool nif_success = true;

    uint32_t atom_len = 0;
    char* atom_value = NULL;

    if(enif_is_atom(env, term))
    {
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

        if(!strcmp(atom_value, "hapi_state_ready"))
        {
            *state = HAPI_STATE_READY;
        }
        else if(!strcmp(atom_value, "hapi_state_ready_with_fatal_errors"))
        {
            *state = HAPI_STATE_READY_WITH_FATAL_ERRORS;
        }
        else if(!strcmp(atom_value, "hapi_state_ready_with_cook_errors"))
        {
            *state = HAPI_STATE_READY_WITH_COOK_ERRORS;
        }
        else if(!strcmp(atom_value, "hapi_state_starting_cook"))
        {
            *state = HAPI_STATE_STARTING_COOK;
        }
        else if(!strcmp(atom_value, "hapi_state_cooking"))
        {
            *state = HAPI_STATE_COOKING;
        }
        else if(!strcmp(atom_value, "hapi_state_starting_load"))
        {
            *state = HAPI_STATE_STARTING_LOAD;
        }
        else if(!strcmp(atom_value, "hapi_state_loading"))
        {
            *state = HAPI_STATE_LOADING;
        }
        else if(!strcmp(atom_value, "hapi_state_max"))
        {
            *state = HAPI_STATE_MAX;
        }
        else if(!strcmp(atom_value, "hapi_state_max_ready_state"))
        {
            *state = HAPI_STATE_MAX_READY_STATE;
        }
        else
        {
            nif_success = false;
        }
    }
    else
    {
        nif_success = false;
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    return nif_success;
}


ERL_NIF_TERM
hapi_enum_state_c_to_erl(ErlNifEnv* env, HAPI_State state)
{
    switch(state)
    {
        case HAPI_STATE_READY:
        {
            return hapi_private_make_atom(env, "hapi_state_ready");
        }

        case HAPI_STATE_READY_WITH_FATAL_ERRORS:
        {
            return hapi_private_make_atom(env, "hapi_state_ready_with_fatal_errors");
        }

        case HAPI_STATE_READY_WITH_COOK_ERRORS:
        {
            return hapi_private_make_atom(env, "hapi_state_ready_with_cook_errors");
        }

        case HAPI_STATE_STARTING_COOK:
        {
            return hapi_private_make_atom(env, "hapi_state_starting_cook");
        }

        case HAPI_STATE_COOKING:
        {
            return hapi_private_make_atom(env, "hapi_state_cooking");
        }

        case HAPI_STATE_STARTING_LOAD:
        {
            return hapi_private_make_atom(env, "hapi_state_starting_load");
        }

        case HAPI_STATE_LOADING:
        {
            return hapi_private_make_atom(env, "hapi_state_loading");
        }

        case HAPI_STATE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_state_max");
        }

        /*
        case HAPI_STATE_MAX_READY_STATE:
        {
            return hapi_private_make_atom(env, "hapi_state_max_ready_state");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
