/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_CurveType enum from HAPI_Common.h

#include "../hapi_private_nif.h"
#include <string.h>


bool
hapi_curvetype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveType* enum_result)
{
    bool nif_success = true;
    uint32_t atom_len = 0;
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
        /* hapi_curvetype_invalid */
        case 2202586514:
        {
            *enum_result = HAPI_CURVETYPE_INVALID;
            break;
        }

        /* hapi_curvetype_linear */
        case 2346647114:
        {
            *enum_result = HAPI_CURVETYPE_LINEAR;
            break;
        }

        /* hapi_curvetype_nurbs */
        case 3129521528:
        {
            *enum_result = HAPI_CURVETYPE_NURBS;
            break;
        }

        /* hapi_curvetype_bezier */
        case 2343775189:
        {
            *enum_result = HAPI_CURVETYPE_BEZIER;
            break;
        }

        /* hapi_curvetype_max */
        case 2118035272:
        {
            *enum_result = HAPI_CURVETYPE_MAX;
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
hapi_curvetype_c_to_erl(ErlNifEnv* env, HAPI_CurveType enum_value)
{
    switch(enum_value)
    {
        case HAPI_CURVETYPE_INVALID:
        {
            return hapi_make_atom(env, "hapi_curvetype_invalid");
        }

        case HAPI_CURVETYPE_LINEAR:
        {
            return hapi_make_atom(env, "hapi_curvetype_linear");
        }

        case HAPI_CURVETYPE_NURBS:
        {
            return hapi_make_atom(env, "hapi_curvetype_nurbs");
        }

        case HAPI_CURVETYPE_BEZIER:
        {
            return hapi_make_atom(env, "hapi_curvetype_bezier");
        }

        case HAPI_CURVETYPE_MAX:
        {
            return hapi_make_atom(env, "hapi_curvetype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
