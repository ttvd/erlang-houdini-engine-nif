#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <stdio.h>


bool hapi_enum_geo_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_GeoType* geo_type)
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
        // "hapi_geotype_invalid"
        case 2078624117:
        {
            *geo_type = HAPI_GEOTYPE_INVALID;
            break;
        }

        // "hapi_geotype_default"
        case 1702122176:
        {
            *geo_type = HAPI_GEOTYPE_DEFAULT;
            break;
        }

        // "hapi_geotype_intermediate"
        case 2057813327:
        {
            *geo_type = HAPI_GEOTYPE_INTERMEDIATE;
            break;
        }

        // "hapi_geotype_input"
        case 3505553948:
        {
            *geo_type = HAPI_GEOTYPE_INPUT;
            break;
        }

        // "hapi_geotype_curve"
        case 3589938837:
        {
            *geo_type = HAPI_GEOTYPE_CURVE;
            break;
        }

        // "hapi_geotype_max"
        case 2561471983:
        {
            *geo_type = HAPI_GEOTYPE_MAX;
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


ERL_NIF_TERM hapi_enum_geo_type_c_to_erl(ErlNifEnv* env, HAPI_GeoType geo_type)
{
    switch(geo_type)
    {
        /*
        case HAPI_GEOTYPE_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_geotype_invalid");
        }
        */

        case HAPI_GEOTYPE_DEFAULT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_geotype_default");
        }

        case HAPI_GEOTYPE_INTERMEDIATE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_geotype_intermediate");
        }

        case HAPI_GEOTYPE_INPUT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_geotype_input");
        }

        case HAPI_GEOTYPE_CURVE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_geotype_curve");
        }

        case HAPI_GEOTYPE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_geotype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
