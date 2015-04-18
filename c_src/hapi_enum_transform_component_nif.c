#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <stdio.h>


bool hapi_enum_transform_component_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TransformComponent* transform_component)
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
        // "hapi_transform_tx"
        case 2763045797:
        {
            *transform_component = HAPI_TRANSFORM_TX;
            break;
        }

        // "hapi_transform_ty"
        case 1138742908:
        {
            *transform_component = HAPI_TRANSFORM_TY;
            break;
        }

        // "hapi_transform_tz"
        case 1366188897:
        {
            *transform_component = HAPI_TRANSFORM_TZ;
            break;
        }

        // "hapi_transform_rx"
        case 1796486426:
        {
            *transform_component = HAPI_TRANSFORM_RX;
            break;
        }

        // "hapi_transform_ry"
        case 41409567:
        {
            *transform_component = HAPI_TRANSFORM_RY;
            break;
        }

        // "hapi_transform_rz"
        case 2024189270:
        {
            *transform_component = HAPI_TRANSFORM_RZ;
            break;
        }

        // "hapi_transform_qx"
        case 3784000339:
        {
            *transform_component = HAPI_TRANSFORM_QX;
            break;
        }

        // "hapi_transform_qy"
        case 2697215009:
        {
            *transform_component = HAPI_TRANSFORM_QY;
            break;
        }

        // "hapi_transform_qz"
        case 1869023627:
        {
            *transform_component = HAPI_TRANSFORM_QZ;
            break;
        }

        // "hapi_transform_qw"
        case 1077003245:
        {
            *transform_component = HAPI_TRANSFORM_QW;
            break;
        }

        // "hapi_transform_sx"
        case 3242450707:
        {
            *transform_component = HAPI_TRANSFORM_SX;
            break;
        }

        // "hapi_transform_sy"
        case 2683523763:
        {
            *transform_component = HAPI_TRANSFORM_SY;
            break;
        }

        // "hapi_transform_sz"
        case 1336671716:
        {
            *transform_component = HAPI_TRANSFORM_SZ;
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


ERL_NIF_TERM hapi_enum_transform_component_c_to_erl(ErlNifEnv* env, HAPI_TransformComponent transform_component)
{
    switch(transform_component)
    {
        case HAPI_TRANSFORM_TX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_tx");
        }

        case HAPI_TRANSFORM_TY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_ty");
        }

        case HAPI_TRANSFORM_TZ:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_tz");
        }

        case HAPI_TRANSFORM_RX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_rx");
        }

        case HAPI_TRANSFORM_RY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_ry");
        }

        case HAPI_TRANSFORM_RZ:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_rz");
        }

        case HAPI_TRANSFORM_QX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_qx");
        }

        case HAPI_TRANSFORM_QY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_qy");
        }

        case HAPI_TRANSFORM_QZ:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_qz");
        }

        case HAPI_TRANSFORM_QW:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_qw");
        }

        case HAPI_TRANSFORM_SX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_sx");
        }

        case HAPI_TRANSFORM_SY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_sy");
        }

        case HAPI_TRANSFORM_SZ:
        {
            return hapi_private_make_hash_tuple(env, "hapi_transform_sz");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
