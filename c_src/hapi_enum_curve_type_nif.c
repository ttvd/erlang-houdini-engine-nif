#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_curve_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveType* curve_type)
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

        if(!strcmp(atom_value, "hapi_curvetype_invalid"))
        {
            *curve_type = HAPI_CURVETYPE_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_curvetype_linear"))
        {
            *curve_type = HAPI_CURVETYPE_LINEAR;
        }
        else if(!strcmp(atom_value, "hapi_curvetype_nurbs"))
        {
            *curve_type = HAPI_CURVETYPE_NURBS;
        }
        else if(!strcmp(atom_value, "hapi_curvetype_bezier"))
        {
            *curve_type = HAPI_CURVETYPE_BEZIER;
        }
        else if(!strcmp(atom_value, "hapi_curvetype_max"))
        {
            *curve_type = HAPI_CURVETYPE_MAX;
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


ERL_NIF_TERM hapi_enum_curve_type_c_to_erl(ErlNifEnv* env, HAPI_CurveType curve_type)
{
    switch(curve_type)
    {
        /*
        case HAPI_CURVETYPE_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_curvetype_invalid");
        }
        */

        case HAPI_CURVETYPE_LINEAR:
        {
            return hapi_private_make_atom(env, "hapi_curvetype_linear");
        }

        case HAPI_CURVETYPE_NURBS:
        {
            return hapi_private_make_atom(env, "hapi_curvetype_nurbs");
        }

        case HAPI_CURVETYPE_BEZIER:
        {
            return hapi_private_make_atom(env, "hapi_curvetype_bezier");
        }

        case HAPI_CURVETYPE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_curvetype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
