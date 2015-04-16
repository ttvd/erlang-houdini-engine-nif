#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_storage_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StorageType* storage_type)
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
            // "hapi_storagetype_invalid"
            case 2592088464:
            {
                *storage_type = HAPI_STORAGETYPE_INVALID;
            }

            // "hapi_storagetype_int"
            case 2403500183:
            {
                *storage_type = HAPI_STORAGETYPE_INT;
            }

            // "hapi_storagetype_float"
            case 604801786:
            {
                *storage_type = HAPI_STORAGETYPE_FLOAT;
            }

            // "hapi_storagetype_string"
            case 3554866194:
            {
                *storage_type = HAPI_STORAGETYPE_STRING;
            }

            // "hapi_storagetype_max"
            case 1435904941:
            {
                *storage_type = HAPI_STORAGETYPE_MAX;
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


ERL_NIF_TERM hapi_enum_storage_type_c_to_erl(ErlNifEnv* env, HAPI_StorageType storage_type)
{
    switch(storage_type)
    {
        /*
        case HAPI_STORAGETYPE_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_storagetype_invalid");
        }
        */

        case HAPI_STORAGETYPE_INT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_storagetype_int");
        }

        case HAPI_STORAGETYPE_FLOAT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_storagetype_float");
        }

        case HAPI_STORAGETYPE_STRING:
        {
            return hapi_private_make_hash_tuple(env, "hapi_storagetype_string");
        }

        case HAPI_STORAGETYPE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_storagetype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
