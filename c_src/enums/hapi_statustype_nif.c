/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_StatusType enum from HAPI_Common.h

#include "../hapi_private_nif.h"
#include <string.h>


bool
hapi_make_hapi_statustype_(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StatusType* enum_result)
{
    bool nif_success = true;
    uint32_t atom_len = 0u;
    uint32_t atom_hash = 0u;
    char* atom_value = NULL;

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

    switch(atom_hash)
    {
        /* hapi_status_call_result */
        case 2001185294:
        {
            *enum_result = HAPI_STATUS_CALL_RESULT;
            break;
        }

        /* hapi_status_cook_result */
        case 2030323224:
        {
            *enum_result = HAPI_STATUS_COOK_RESULT;
            break;
        }

        /* hapi_status_cook_state */
        case 3336497966:
        {
            *enum_result = HAPI_STATUS_COOK_STATE;
            break;
        }

        /* hapi_status_max */
        case 220878498:
        {
            *enum_result = HAPI_STATUS_MAX;
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


ERL_NIF_TERM
hapi_get_hapi_statustype_c_to_erl(ErlNifEnv* env, HAPI_StatusType enum_value)
{
    switch(enum_value)
    {
        case HAPI_STATUS_CALL_RESULT:
        {
            return hapi_make_atom(env, "hapi_status_call_result");
        }

        case HAPI_STATUS_COOK_RESULT:
        {
            return hapi_make_atom(env, "hapi_status_cook_result");
        }

        case HAPI_STATUS_COOK_STATE:
        {
            return hapi_make_atom(env, "hapi_status_cook_state");
        }

        case HAPI_STATUS_MAX:
        {
            return hapi_make_atom(env, "hapi_status_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
