#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_permissions_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Permissions* permissions)
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
            // "hapi_permissions_non_applicable"
            case 2395404179:
            {
                *permissions = HAPI_PERMISSIONS_NON_APPLICABLE;
            }

            // "hapi_permissions_read_write"
            case 3046338534:
            {
                *permissions = HAPI_PERMISSIONS_READ_WRITE;
            }

            // "hapi_permissions_read_only"
            case 3590233932:
            {
                *permissions = HAPI_PERMISSIONS_READ_ONLY;
            }

            // "hapi_permissions_write_only"
            case 256108734:
            {
                *permissions = HAPI_PERMISSIONS_WRITE_ONLY;
            }

            // "hapi_permissions_max"
            case 3126986835:
            {
                *permissions = HAPI_PERMISSIONS_MAX;
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
