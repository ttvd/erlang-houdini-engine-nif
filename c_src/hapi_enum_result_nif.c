#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_result_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Result* result)
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

        if(!strcmp(atom_value, "hapi_result_success"))
        {
            *result = HAPI_RESULT_SUCCESS;
        }
        else if(!strcmp(atom_value, "hapi_result_failure"))
        {
            *result = HAPI_RESULT_FAILURE;
        }
        else if(!strcmp(atom_value, "hapi_result_already_initialized"))
        {
            *result = HAPI_RESULT_ALREADY_INITIALIZED;
        }
        else if(!strcmp(atom_value, "hapi_result_not_initialized"))
        {
            *result = HAPI_RESULT_NOT_INITIALIZED;
        }
        else if(!strcmp(atom_value, "hapi_result_cant_loadfile"))
        {
            *result = HAPI_RESULT_CANT_LOADFILE;
        }
        else if(!strcmp(atom_value, "hapi_result_parm_set_failed"))
        {
            *result = HAPI_RESULT_PARM_SET_FAILED;
        }
        else if(!strcmp(atom_value, "hapi_result_invalid_argument"))
        {
            *result = HAPI_RESULT_INVALID_ARGUMENT;
        }
        else if(!strcmp(atom_value, "hapi_result_cant_load_geo"))
        {
            *result = HAPI_RESULT_CANT_LOAD_GEO;
        }
        else if(!strcmp(atom_value, "hapi_result_cant_generate_preset"))
        {
            *result = HAPI_RESULT_CANT_GENERATE_PRESET;
        }
        else if(!strcmp(atom_value, "hapi_result_cant_load_preset"))
        {
            *result = HAPI_RESULT_CANT_LOAD_PRESET;
        }
        else if(!strcmp(atom_value, "hapi_result_asset_def_already_loaded"))
        {
            *result = HAPI_RESULT_ASSET_DEF_ALREADY_LOADED;
        }
        else if(!strcmp(atom_value, "hapi_result_no_license_found"))
        {
            *result = HAPI_RESULT_NO_LICENSE_FOUND;
        }
        else if(!strcmp(atom_value, "hapi_result_disallowed_nc_license_found"))
        {
            *result = HAPI_RESULT_DISALLOWED_NC_LICENSE_FOUND;
        }
        else if(!strcmp(atom_value, "hapi_result_disallowed_nc_asset_with_c_license"))
        {
            *result = HAPI_RESULT_DISALLOWED_NC_ASSET_WITH_C_LICENSE;
        }
        else if(!strcmp(atom_value, "hapi_result_disallowed_nc_asset_with_lc_license"))
        {
            *result = HAPI_RESULT_DISALLOWED_NC_ASSET_WITH_LC_LICENSE;
        }
        else if(!strcmp(atom_value, "hapi_result_disallowed_lc_asset_with_c_license"))
        {
            *result = HAPI_RESULT_DISALLOWED_LC_ASSET_WITH_C_LICENSE;
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


ERL_NIF_TERM hapi_enum_result_c_to_erl(ErlNifEnv* env, HAPI_Result result)
{
    switch(result)
    {
        case HAPI_RESULT_SUCCESS:
        {
            return hapi_private_make_atom(env, "hapi_result_success");
        }

        case HAPI_RESULT_FAILURE:
        {
            return hapi_private_make_atom(env, "hapi_result_failure");
        }

        case HAPI_RESULT_ALREADY_INITIALIZED:
        {
            return hapi_private_make_atom(env, "hapi_result_already_initialized");
        }

        case HAPI_RESULT_NOT_INITIALIZED:
        {
            return hapi_private_make_atom(env, "hapi_result_not_initialized");
        }

        case HAPI_RESULT_CANT_LOADFILE:
        {
            return hapi_private_make_atom(env, "hapi_result_cant_loadfile");
        }

        case HAPI_RESULT_PARM_SET_FAILED:
        {
            return hapi_private_make_atom(env, "hapi_result_parm_set_failed");
        }

        case HAPI_RESULT_INVALID_ARGUMENT:
        {
            return hapi_private_make_atom(env, "hapi_result_invalid_argument");
        }

        case HAPI_RESULT_CANT_LOAD_GEO:
        {
            return hapi_private_make_atom(env, "hapi_result_cant_load_geo");
        }

        case HAPI_RESULT_CANT_GENERATE_PRESET:
        {
            return hapi_private_make_atom(env, "hapi_result_cant_generate_preset");
        }

        case HAPI_RESULT_CANT_LOAD_PRESET:
        {
            return hapi_private_make_atom(env, "hapi_result_cant_load_preset");
        }

        case HAPI_RESULT_ASSET_DEF_ALREADY_LOADED:
        {
            return hapi_private_make_atom(env, "hapi_result_asset_def_already_loaded");
        }

        case HAPI_RESULT_NO_LICENSE_FOUND:
        {
            return hapi_private_make_atom(env, "hapi_result_no_license_found");
        }

        case HAPI_RESULT_DISALLOWED_NC_LICENSE_FOUND:
        {
            return hapi_private_make_atom(env, "hapi_result_disallowed_nc_license_found");
        }

        case HAPI_RESULT_DISALLOWED_NC_ASSET_WITH_C_LICENSE:
        {
            return hapi_private_make_atom(env, "hapi_result_disallowed_nc_asset_with_c_license");
        }

        case HAPI_RESULT_DISALLOWED_NC_ASSET_WITH_LC_LICENSE:
        {
            return hapi_private_make_atom(env, "hapi_result_disallowed_nc_asset_with_lc_license");
        }

        case HAPI_RESULT_DISALLOWED_LC_ASSET_WITH_C_LICENSE:
        {
            return hapi_private_make_atom(env, "hapi_result_disallowed_lc_asset_with_c_license");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
