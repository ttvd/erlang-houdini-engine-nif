#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_status_verbosity_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StatusVerbosity* status_verbosity)
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

        if(!strcmp(atom_value, "hapi_statusverbosity_0"))
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_0;
        }
        else if(!strcmp(atom_value, "hapi_statusverbosity_1"))
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_1;
        }
        else if(!strcmp(atom_value, "hapi_statusverbosity_2"))
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_2;
        }
        else if(!strcmp(atom_value, "hapi_statusverbosity_all"))
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_ALL;
        }
        else if(!strcmp(atom_value, "hapi_statusverbosity_errors"))
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_ERRORS;
        }
        else if(!strcmp(atom_value, "hapi_statusverbosity_warnings"))
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_WARNINGS;
        }
        else if(!strcmp(atom_value, "hapi_statusverbosity_messages"))
        {
            *status_verbosity = HAPI_STATUSVERBOSITY_MESSAGES;
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


ERL_NIF_TERM hapi_enum_status_verbosity_c_to_erl(ErlNifEnv* env, HAPI_StatusVerbosity status_verbosity)
{
    switch(status_verbosity)
    {
        case HAPI_STATUSVERBOSITY_0:
        {
            return hapi_private_make_atom(env, "hapi_statusverbosity_0");
        }

        case HAPI_STATUSVERBOSITY_1:
        {
            return hapi_private_make_atom(env, "hapi_statusverbosity_1");
        }

        case HAPI_STATUSVERBOSITY_2:
        {
            return hapi_private_make_atom(env, "hapi_statusverbosity_2");
        }

        /*
        case HAPI_STATUSVERBOSITY_ALL:
        {
            return hapi_private_make_atom(env, "hapi_statusverbosity_all");
        }
        */

        /*
        case HAPI_STATUSVERBOSITY_ERRORS:
        {
            return hapi_private_make_atom(env, "hapi_statusverbosity_errors");
        }
        */

        /*
        case HAPI_STATUSVERBOSITY_WARNINGS:
        {
            return hapi_private_make_atom(env, "hapi_statusverbosity_warnings");
        }
        */

        /*
        case HAPI_STATUSVERBOSITY_MESSAGES:
        {
            return hapi_private_make_atom(env, "hapi_statusverbosity_messages");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
