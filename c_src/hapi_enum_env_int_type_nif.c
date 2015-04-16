#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_env_int_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_EnvIntType* env_int_type)
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
            // "hapi_envint_invalid"
            case 3841512571:
            {
                *env_int_type = HAPI_ENVINT_INVALID;
            }

            // "hapi_envint_version_houdini_major"
            case 729783216:
            {
                *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_MAJOR;
            }

            // "hapi_envint_version_houdini_minor"
            case 2183966821:
            {
                *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_MINOR;
            }

            // "hapi_envint_version_houdini_build"
            case 2359290413:
            {
                *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_BUILD;
            }

            // "hapi_envint_version_houdini_patch"
            case 2304600462:
            {
                *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_PATCH;
            }

            // "hapi_envint_version_orig_houdini_major"
            case 3489108176:
            {
                *env_int_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_MAJOR;
            }

            // "hapi_envint_version_orig_houdini_minor"
            case 4098809334:
            {
                *env_int_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_MINOR;
            }

            // "hapi_envint_version_orig_houdini_build"
            case 370554041:
            {
                *env_int_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_BUILD;
            }

            // "hapi_envint_version_orig_houdini_patch"
            case 1477480701:
            {
                *env_int_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_PATCH;
            }

            // "hapi_envint_version_houdini_engine_major"
            case 2437871873:
            {
                *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MAJOR;
            }

            // "hapi_envint_version_houdini_engine_minor"
            case 3017839496:
            {
                *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MINOR;
            }

            // "hapi_envint_version_houdini_engine_api"
            case 2017899821:
            {
                *env_int_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_API;
            }

            // "hapi_envint_license"
            case 4199554980:
            {
                *env_int_type = HAPI_ENVINT_LICENSE;
            }

            // "hapi_envint_max"
            case 34438215:
            {
                *env_int_type = HAPI_ENVINT_MAX;
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
