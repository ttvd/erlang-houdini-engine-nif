#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_geo_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_GeoType* geo_type)
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

        if(!strcmp(atom_value, "hapi_geotype_invalid"))
        {
            *geo_type = HAPI_GEOTYPE_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_geotype_default"))
        {
            *geo_type = HAPI_GEOTYPE_DEFAULT;
        }
        else if(!strcmp(atom_value, "hapi_geotype_intermediate"))
        {
            *geo_type = HAPI_GEOTYPE_INTERMEDIATE;
        }
        else if(!strcmp(atom_value, "hapi_geotype_input"))
        {
            *geo_type = HAPI_GEOTYPE_INPUT;
        }
        else if(!strcmp(atom_value, "hapi_geotype_curve"))
        {
            *geo_type = HAPI_GEOTYPE_CURVE;
        }
        else if(!strcmp(atom_value, "hapi_geotype_max"))
        {
            *geo_type = HAPI_GEOTYPE_MAX;
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


ERL_NIF_TERM hapi_enum_geo_type_c_to_erl(ErlNifEnv* env, HAPI_GeoType geo_type)
{
    switch(geo_type)
    {
        /*
        case HAPI_GEOTYPE_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_geotype_invalid");
        }
        */

        case HAPI_GEOTYPE_DEFAULT:
        {
            return hapi_private_make_atom(env, "hapi_geotype_default");
        }

        case HAPI_GEOTYPE_INTERMEDIATE:
        {
            return hapi_private_make_atom(env, "hapi_geotype_intermediate");
        }

        case HAPI_GEOTYPE_INPUT:
        {
            return hapi_private_make_atom(env, "hapi_geotype_input");
        }

        case HAPI_GEOTYPE_CURVE:
        {
            return hapi_private_make_atom(env, "hapi_geotype_curve");
        }

        case HAPI_GEOTYPE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_geotype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
