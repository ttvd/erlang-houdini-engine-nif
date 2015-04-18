#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_env_int_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_EnvIntType* env_int_type)
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
        // "hapi_envint_invalid"
        case 3841512571:
        {
            *env_int_type = HAPI_ENVINT_INVALID;
            break;
        }

        // "hapi_envint_version_houdini_major"
        case 729783216:
        {
            *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_MAJOR;
            break;
        }

        // "hapi_envint_version_houdini_minor"
        case 2183966821:
        {
            *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_MINOR;
            break;
        }

        // "hapi_envint_version_houdini_build"
        case 2359290413:
        {
            *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_BUILD;
            break;
        }

        // "hapi_envint_version_houdini_patch"
        case 2304600462:
        {
            *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_PATCH;
            break;
        }

        // "hapi_envint_version_orig_houdini_major"
        case 3489108176:
        {
            *env_int_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_MAJOR;
            break;
        }

        // "hapi_envint_version_orig_houdini_minor"
        case 4098809334:
        {
            *env_int_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_MINOR;
            break;
        }

        // "hapi_envint_version_orig_houdini_build"
        case 370554041:
        {
            *env_int_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_BUILD;
            break;
        }

        // "hapi_envint_version_orig_houdini_patch"
        case 1477480701:
        {
            *env_int_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_PATCH;
            break;
        }

        // "hapi_envint_version_houdini_engine_major"
        case 2437871873:
        {
            *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MAJOR;
            break;
        }

        // "hapi_envint_version_houdini_engine_minor"
        case 3017839496:
        {
            *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MINOR;
            break;
        }

        // "hapi_envint_version_houdini_engine_api"
        case 2017899821:
        {
            *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_API;
            break;
        }

        // "hapi_envint_license"
        case 4199554980:
        {
            *env_int_type = HAPI_ENVINT_LICENSE;
            break;
        }

        // "hapi_envint_max"
        case 34438215:
        {
            *env_int_type = HAPI_ENVINT_MAX;
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


ERL_NIF_TERM hapi_enum_env_int_type_c_to_erl(ErlNifEnv* env, HAPI_EnvIntType env_int_type)
{
    switch(env_int_type)
    {
        /*
        case HAPI_ENVINT_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_invalid");
        }
        */

        case HAPI_ENVINT_VERSION_HOUDINI_MAJOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_houdini_major");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_MINOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_houdini_minor");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_BUILD:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_houdini_build");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_PATCH:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_houdini_patch");
        }

        case HAPI_ENVINT_VERSION_ORIG_HOUDINI_MAJOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_orig_houdini_major");
        }

        case HAPI_ENVINT_VERSION_ORIG_HOUDINI_MINOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_orig_houdini_minor");
        }

        case HAPI_ENVINT_VERSION_ORIG_HOUDINI_BUILD:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_orig_houdini_build");
        }

        case HAPI_ENVINT_VERSION_ORIG_HOUDINI_PATCH:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_orig_houdini_patch");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MAJOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_houdini_engine_major");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MINOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_houdini_engine_minor");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_ENGINE_API:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_version_houdini_engine_api");
        }

        case HAPI_ENVINT_LICENSE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_license");
        }

        case HAPI_ENVINT_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_envint_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
