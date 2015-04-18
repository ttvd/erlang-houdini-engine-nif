#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_license_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_License* license)
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
        // "hapi_license_none"
        case 2752421224:
        {
            *license = HAPI_LICENSE_NONE;
            break;
        }

        // "hapi_license_houdini_engine"
        case 3778753798:
        {
            *license = HAPI_LICENSE_HOUDINI_ENGINE;
            break;
        }

        // "hapi_license_houdini"
        case 1639045708:
        {
            *license = HAPI_LICENSE_HOUDINI;
            break;
        }

        // "hapi_license_houdini_fx"
        case 2615467121:
        {
            *license = HAPI_LICENSE_HOUDINI_FX;
            break;
        }

        // "hapi_license_houdini_engine_indie"
        case 4087197588:
        {
            *license = HAPI_LICENSE_HOUDINI_ENGINE_INDIE;
            break;
        }

        // "hapi_license_houdini_indie"
        case 238988394:
        {
            *license = HAPI_LICENSE_HOUDINI_INDIE;
            break;
        }

        // "hapi_license_max"
        case 1885498993:
        {
            *license = HAPI_LICENSE_MAX;
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


ERL_NIF_TERM hapi_enum_license_c_to_erl(ErlNifEnv* env, HAPI_License license)
{
    switch(license)
    {
        case HAPI_LICENSE_NONE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_license_none");
        }

        case HAPI_LICENSE_HOUDINI_ENGINE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_license_houdini_engine");
        }

        case HAPI_LICENSE_HOUDINI:
        {
            return hapi_private_make_hash_tuple(env, "hapi_license_houdini");
        }

        case HAPI_LICENSE_HOUDINI_FX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_license_houdini_fx");
        }

        case HAPI_LICENSE_HOUDINI_ENGINE_INDIE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_license_houdini_engine_indie");
        }

        case HAPI_LICENSE_HOUDINI_INDIE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_license_houdini_indie");
        }

        case HAPI_LICENSE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_license_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
