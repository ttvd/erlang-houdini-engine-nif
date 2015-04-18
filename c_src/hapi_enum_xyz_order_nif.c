#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_xyz_order_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_XYZOrder* xyz_order)
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
        // "hapi_xyz"
        case 2568995979:
        {
            *xyz_order = HAPI_XYZ;
            break;
        }

        // "hapi_xzy"
        case 3456057817:
        {
            *xyz_order = HAPI_XZY;
            break;
        }

        // "hapi_yxz"
        case 535519317:
        {
            *xyz_order = HAPI_YXZ;
            break;
        }

        // "hapi_yzx"
        case 2498703819:
        {
            *xyz_order = HAPI_YZX;
            break;
        }

        // "hapi_zxy"
        case 4080643183:
        {
            *xyz_order = HAPI_ZXY;
            break;
        }

        // "hapi_zyx"
        case 2804301731:
        {
            *xyz_order = HAPI_ZYX;
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


ERL_NIF_TERM hapi_enum_xyz_order_c_to_erl(ErlNifEnv* env, HAPI_XYZOrder xyz_order)
{
    switch(xyz_order)
    {
        case HAPI_XYZ:
        {
            return hapi_private_make_hash_tuple(env, "hapi_xyz");
        }

        case HAPI_XZY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_xzy");
        }

        case HAPI_YXZ:
        {
            return hapi_private_make_hash_tuple(env, "hapi_yxz");
        }

        case HAPI_YZX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_yzx");
        }

        case HAPI_ZXY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_zxy");
        }

        case HAPI_ZYX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_zyx");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
