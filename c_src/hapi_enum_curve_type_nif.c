#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_curve_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveType* curve_type)
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
        // "hapi_curvetype_invalid"
        case 2202586514:
        {
            *curve_type = HAPI_CURVETYPE_INVALID;
            break;
        }

        // "hapi_curvetype_linear"
        case 2346647114:
        {
            *curve_type = HAPI_CURVETYPE_LINEAR;
            break;
        }

        // "hapi_curvetype_nurbs"
        case 3129521528:
        {
            *curve_type = HAPI_CURVETYPE_NURBS;
            break;
        }

        // "hapi_curvetype_bezier"
        case 2343775189:
        {
            *curve_type = HAPI_CURVETYPE_BEZIER;
            break;
        }

        // "hapi_curvetype_max"
        case 2118035272:
        {
            *curve_type = HAPI_CURVETYPE_MAX;
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


ERL_NIF_TERM hapi_enum_curve_type_c_to_erl(ErlNifEnv* env, HAPI_CurveType curve_type)
{
    switch(curve_type)
    {
        /*
        case HAPI_CURVETYPE_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curvetype_invalid");
        }
        */

        case HAPI_CURVETYPE_LINEAR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curvetype_linear");
        }

        case HAPI_CURVETYPE_NURBS:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curvetype_nurbs");
        }

        case HAPI_CURVETYPE_BEZIER:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curvetype_bezier");
        }

        case HAPI_CURVETYPE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curvetype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
