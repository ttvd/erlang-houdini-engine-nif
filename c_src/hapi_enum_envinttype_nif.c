#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool
hapi_enum_envinttype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_EnvIntType* envint_type)
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

        if(!strcmp(atom_value, "hapi_envint_invalid"))
        {
            *envint_type = HAPI_ENVINT_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_houdini_major"))
        {
            *envint_type = HAPI_ENVINT_VERSION_HOUDINI_MAJOR;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_houdini_minor"))
        {
            *envint_type = HAPI_ENVINT_VERSION_HOUDINI_MINOR;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_houdini_build"))
        {
            *envint_type = HAPI_ENVINT_VERSION_HOUDINI_BUILD;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_houdini_patch"))
        {
            *envint_type = HAPI_ENVINT_VERSION_HOUDINI_PATCH;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_orig_houdini_major"))
        {
            *envint_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_MAJOR;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_orig_houdini_minor"))
        {
            *envint_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_MINOR;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_orig_houdini_build"))
        {
            *envint_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_BUILD;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_orig_houdini_patch"))
        {
            *envint_type = HAPI_ENVINT_VERSION_ORIG_HOUDINI_PATCH;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_houdini_engine_major"))
        {
            *envint_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MAJOR;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_houdini_engine_minor"))
        {
            *envint_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MINOR;
        }
        else if(!strcmp(atom_value, "hapi_envint_version_houdini_engine_api"))
        {
            *envint_type = HAPI_ENVINT_VERSION_HOUDINI_ENGINE_API;
        }
        else if(!strcmp(atom_value, "hapi_envint_license"))
        {
            *envint_type = HAPI_ENVINT_LICENSE;
        }
        else if(!strcmp(atom_value, "hapi_envint_max"))
        {
            *envint_type = HAPI_ENVINT_MAX;
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


ERL_NIF_TERM
hapi_enum_envinttype_c_to_erl(ErlNifEnv* env, HAPI_EnvIntType envint_type)
{
    switch(envint_type)
    {
        case HAPI_ENVINT_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_envint_invalid");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_MAJOR:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_houdini_major");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_MINOR:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_houdini_minor");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_BUILD:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_houdini_build");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_PATCH:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_houdini_patch");
        }

        case HAPI_ENVINT_VERSION_ORIG_HOUDINI_MAJOR:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_orig_houdini_major");
        }

        case HAPI_ENVINT_VERSION_ORIG_HOUDINI_MINOR:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_orig_houdini_minor");
        }

        case HAPI_ENVINT_VERSION_ORIG_HOUDINI_BUILD:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_orig_houdini_build");
        }

        case HAPI_ENVINT_VERSION_ORIG_HOUDINI_PATCH:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_orig_houdini_patch");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MAJOR:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_houdini_engine_major");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_ENGINE_MINOR:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_houdini_engine_minor");
        }

        case HAPI_ENVINT_VERSION_HOUDINI_ENGINE_API:
        {
            return hapi_private_make_atom(env, "hapi_envint_version_houdini_engine_api");
        }

        case HAPI_ENVINT_LICENSE:
        {
            return hapi_private_make_atom(env, "hapi_envint_license");
        }

        case HAPI_ENVINT_MAX:
        {
            return hapi_private_make_atom(env, "hapi_envint_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
