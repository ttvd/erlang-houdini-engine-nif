#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_permissions_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Permissions* permissions)
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
        // "hapi_permissions_non_applicable"
        case 2395404179:
        {
            *permissions = HAPI_PERMISSIONS_NON_APPLICABLE;
            break;
        }

        // "hapi_permissions_read_write"
        case 3046338534:
        {
            *permissions = HAPI_PERMISSIONS_READ_WRITE;
            break;
        }

        // "hapi_permissions_read_only"
        case 3590233932:
        {
            *permissions = HAPI_PERMISSIONS_READ_ONLY;
            break;
        }

        // "hapi_permissions_write_only"
        case 256108734:
        {
            *permissions = HAPI_PERMISSIONS_WRITE_ONLY;
            break;
        }

        // "hapi_permissions_max"
        case 3126986835:
        {
            *permissions = HAPI_PERMISSIONS_MAX;
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


ERL_NIF_TERM hapi_enum_permissions_c_to_erl(ErlNifEnv* env, HAPI_Permissions permissions)
{
    switch(permissions)
    {
        case HAPI_PERMISSIONS_NON_APPLICABLE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_permissions_non_applicable");
        }

        case HAPI_PERMISSIONS_READ_WRITE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_permissions_read_write");
        }

        case HAPI_PERMISSIONS_READ_ONLY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_permissions_read_only");
        }

        case HAPI_PERMISSIONS_WRITE_ONLY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_permissions_write_only");
        }

        case HAPI_PERMISSIONS_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_permissions_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
