#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_storage_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StorageType* storage_type)
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

        if(!strcmp(atom_value, "hapi_storagetype_invalid"))
        {
            *storage_type = HAPI_STORAGETYPE_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_storagetype_int"))
        {
            *storage_type = HAPI_STORAGETYPE_INT;
        }
        else if(!strcmp(atom_value, "hapi_storagetype_float"))
        {
            *storage_type = HAPI_STORAGETYPE_FLOAT;
        }
        else if(!strcmp(atom_value, "hapi_storagetype_string"))
        {
            *storage_type = HAPI_STORAGETYPE_STRING;
        }
        else if(!strcmp(atom_value, "hapi_storagetype_max"))
        {
            *storage_type = HAPI_STORAGETYPE_MAX;
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


ERL_NIF_TERM hapi_enum_storage_type_c_to_erl(ErlNifEnv* env, HAPI_StorageType storage_type)
{
    switch(storage_type)
    {
        /*
        case HAPI_STORAGETYPE_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_storagetype_invalid");
        }
        */

        case HAPI_STORAGETYPE_INT:
        {
            return hapi_private_make_atom(env, "hapi_storagetype_int");
        }

        case HAPI_STORAGETYPE_FLOAT:
        {
            return hapi_private_make_atom(env, "hapi_storagetype_float");
        }

        case HAPI_STORAGETYPE_STRING:
        {
            return hapi_private_make_atom(env, "hapi_storagetype_string");
        }

        case HAPI_STORAGETYPE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_storagetype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
