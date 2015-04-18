#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_status_verbosity_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StatusVerbosity* status_verbosity)
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
        // "hapi_statusverbosity_0"
        case 1281355693:
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_0;
            break;
        }

        // "hapi_statusverbosity_1"
        case 3580236127:
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_1;
            break;
        }

        // "hapi_statusverbosity_2"
        case 2340815382:
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_2;
            break;
        }

        // "hapi_statusverbosity_all"
        case 342427634:
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_ALL;
            break;
        }

        // "hapi_statusverbosity_errors"
        case 3898403122:
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_ERRORS;
            break;
        }

        // "hapi_statusverbosity_warnings"
        case 823904373:
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_WARNINGS;
            break;
        }

        // "hapi_statusverbosity_messages"
        case 753691706:
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_MESSAGES;
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


ERL_NIF_TERM hapi_enum_status_verbosity_c_to_erl(ErlNifEnv* env, HAPI_StatusVerbosity status_verbosity)
{
    switch(status_verbosity)
    {
        case HAPI_STATUSVERBOSITY_0:
        {
            return hapi_private_make_hash_tuple(env, "hapi_statusverbosity_0");
        }

        case HAPI_STATUSVERBOSITY_1:
        {
            return hapi_private_make_hash_tuple(env, "hapi_statusverbosity_1");
        }

        case HAPI_STATUSVERBOSITY_2:
        {
            return hapi_private_make_hash_tuple(env, "hapi_statusverbosity_2");
        }

        /*
        case HAPI_STATUSVERBOSITY_ALL:
        {
            return hapi_private_make_hash_tuple(env, "hapi_statusverbosity_all");
        }
        */

        /*
        case HAPI_STATUSVERBOSITY_ERRORS:
        {
            return hapi_private_make_hash_tuple(env, "hapi_statusverbosity_errors");
        }
        */

        /*
        case HAPI_STATUSVERBOSITY_WARNINGS:
        {
            return hapi_private_make_hash_tuple(env, "hapi_statusverbosity_warnings");
        }
        */

        /*
        case HAPI_STATUSVERBOSITY_MESSAGES:
        {
            return hapi_private_make_hash_tuple(env, "hapi_statusverbosity_messages");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
