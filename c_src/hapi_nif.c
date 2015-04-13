#include "erl_nif.h"
#include "HAPI.h"

#include <assert.h>


int hapi_private_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
void hapi_private_cleanup(ErlNifEnv* env, void* obj);

ERL_NIF_TERM hapi_is_initialized_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hapi_initialize_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hapi_cleanup_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


static ErlNifFunc nif_funcs[] =
{
    {"hapi_is_initialized", 0, hapi_is_initialized_impl},
    {"hapi_initialize", 5, hapi_initialize_impl},
    {"hapi_cleanup", 0, hapi_cleanup_impl}
};


static ERL_NIF_TERM g_atom_ok;
static ErlNifResourceType* hapi_handle;


static
ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* atom_name)
{
    ERL_NIF_TERM atom;

    if(enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1))
    {
        return atom;
    }

    return enif_make_atom(env, atom_name);
}


int
hapi_private_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    g_atom_ok = make_atom(env, "ok");

    hapi_handle = enif_open_resource_type(env, "hapi", "hapi_handle", &hapi_private_cleanup,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if(!hapi_handle)
    {
        return -1;
    }

    return 0;
}


ERL_NIF_INIT(hapi, nif_funcs, &hapi_private_init, NULL, NULL, NULL)


// Helper function to translate HAPI_Result into Erlang atoms.
static
ERL_NIF_TERM
hapi_private_process_result(ErlNifEnv* env, HAPI_Result result)
{
    switch(result)
    {
        case HAPI_RESULT_SUCCESS:
        {
            return make_atom(env, "hapi_result_success");
        }

        case HAPI_RESULT_FAILURE:
        {
            return make_atom(env, "hapi_result_failure");
        }

        case HAPI_RESULT_ALREADY_INITIALIZED:
        {
            return make_atom(env, "hapi_result_already_initialized");
        }

        case HAPI_RESULT_NOT_INITIALIZED:
        {
            return make_atom(env, "hapi_result_not_initialized");
        }

        case HAPI_RESULT_CANT_LOADFILE:
        {
            return make_atom(env, "hapi_result_cant_loadfile");
        }

        case HAPI_RESULT_PARM_SET_FAILED:
        {
            return make_atom(env, "hapi_result_parm_set_failed");
        }

        case HAPI_RESULT_INVALID_ARGUMENT:
        {
            return make_atom(env, "hapi_result_invalid_argument");
        }

        case HAPI_RESULT_CANT_LOAD_GEO:
        {
            return make_atom(env, "hapi_result_cant_load_geo");
        }

        case HAPI_RESULT_CANT_GENERATE_PRESET:
        {
            return make_atom(env, "hapi_result_cant_generate_preset");
        }

        case HAPI_RESULT_CANT_LOAD_PRESET:
        {
            return make_atom(env, "hapi_result_cant_load_preset");
        }

        case HAPI_RESULT_ASSET_DEF_ALREADY_LOADED:
        {
            return make_atom(env, "hapi_result_asset_def_already_loaded");
        }

        case HAPI_RESULT_NO_LICENSE_FOUND:
        {
            return make_atom(env, "hapi_result_no_license_found");
        }

        case HAPI_RESULT_DISALLOWED_NC_LICENSE_FOUND:
        {
            return make_atom(env, "hapi_result_disallowed_nc_license_found");
        }

        case HAPI_RESULT_DISALLOWED_NC_ASSET_WITH_C_LICENSE:
        {
            return make_atom(env, "hapi_result_disallowed_nc_asset_with_c_license");
        }

        case HAPI_RESULT_DISALLOWED_NC_ASSET_WITH_LC_LICENSE:
        {
            return make_atom(env, "hapi_result_disallowed_nc_asset_with_lc_license");
        }

        case HAPI_RESULT_DISALLOWED_LC_ASSET_WITH_C_LICENSE:
        {
            return make_atom(env, "hapi_result_disallowed_lc_asset_with_c_license");
        }

        default:
        {
            assert(0);
        }
    }
}


ERL_NIF_TERM
hapi_is_initialized_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return hapi_private_process_result(env, HAPI_IsInitialized());
}


static
ERL_NIF_TERM
hapi_initialize_impl_helper(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint32_t otl_search_path_length = 0;
    uint32_t dso_search_path_length = 0;

    char* otl_search_path = NULL;
    char* dso_search_path = NULL;

    if(!enif_get_list_length(env, argv[0], &otl_search_path_length) ||
        !enif_get_list_length(env, argv[1], &dso_search_path_length))
    {
        return enif_make_badarg(env);
    }

    if(otl_search_path_length > 0)
    {
        otl_search_path = malloc((otl_search_path_length + 1) * sizeof(char));

        if(enif_get_string(env, argv[0], otl_search_path, otl_search_path_length + 1, ERL_NIF_LATIN1) < 1)
        {
            goto label_cleanup;
        }
    }

    if(dso_search_path_length > 0)
    {
        dso_search_path = malloc((dso_search_path_length + 1) * sizeof(char));

        if(enif_get_string(env, argv[1], dso_search_path, dso_search_path_length + 1, ERL_NIF_LATIN1) < 1)
        {
            goto label_cleanup;
        }
    }




    /*
    const char * otl_search_path,
    const char * dso_search_path,
    const HAPI_CookOptions * cook_options,
    HAPI_Bool use_cooking_thread,
    int cooking_thread_stack_size
    */

label_cleanup:

    if(otl_search_path)
    {
        free(otl_search_path);
    }

    if(dso_search_path)
    {
        free(dso_search_path);
    }

    return g_atom_ok;
}

ERL_NIF_TERM
hapi_initialize_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_initialize_impl_helper", 0, hapi_initialize_impl_helper, argc, argv);
}


ERL_NIF_TERM
hapi_cleanup_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return hapi_private_process_result(env, HAPI_Cleanup());
}
