#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool
hapi_enum_statustype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StatusType* status_type)
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

        if(!strcmp(atom_value, "hapi_status_call_result"))
        {
            *status_type = HAPI_STATUS_CALL_RESULT;
        }
        else if(!strcmp(atom_value, "hapi_status_cook_result"))
        {
            *status_type = HAPI_STATUS_COOK_RESULT;
        }
        else if(!strcmp(atom_value, "hapi_status_cook_state"))
        {
            *status_type = HAPI_STATUS_COOK_STATE;
        }
        else if(!strcmp(atom_value, "hapi_status_max"))
        {
            *status_type = HAPI_STATUS_MAX;
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
hapi_enum_statustype_c_to_erl(ErlNifEnv* env, HAPI_StatusType status_type)
{
    switch(status_type)
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
