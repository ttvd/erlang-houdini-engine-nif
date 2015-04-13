#include "erl_nif.h"
#include "HAPI.h"

#include <assert.h>
#include <string.h>


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
hapi_private_make_atom(ErlNifEnv* env, const char* atom_name)
{
    ERL_NIF_TERM atom;

    if(enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1))
    {
        return atom;
    }

    return enif_make_atom(env, atom_name);
}


static
ERL_NIF_TERM
hapi_private_get_boolean(ErlNifEnv* env, const ERL_NIF_TERM term, int32_t* value)
{
    int32_t nif_error = 0;

    uint32_t atom_len = 0;
    char* atom_value = NULL;

    if(!enif_get_atom_length(env, term, &atom_len, ERL_NIF_LATIN1))
    {
        nif_error = 1;
        goto label_cleanup;
    }

    if(enif_get_string(env, term, atom_value, atom_len + 1, ERL_NIF_LATIN1) < 1)
    {
        nif_error = 1;
        goto label_cleanup;
    }

    *value = (!strcmp(atom_value, "true"));

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    if(nif_error)
    {
        return enif_make_badarg(env);
    }

    return g_atom_ok;
}


int
hapi_private_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    g_atom_ok = hapi_private_make_atom(env, "ok");

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
            assert(0);
        }
    }
}


// Helper function to retrieve cook option values from a record.



ERL_NIF_TERM
hapi_is_initialized_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return hapi_private_process_result(env, HAPI_IsInitialized());
}


static
ERL_NIF_TERM
hapi_initialize_impl_helper(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t nif_error = 0;

    uint32_t otl_search_path_length = 0u;
    uint32_t dso_search_path_length = 0u;

    char* otl_search_path = NULL;
    char* dso_search_path = NULL;

    int32_t cook_options_split_geos_by_group = 0;
    int32_t cook_options_max_vertices_per_primitive = 3;
    int32_t cook_options_refine_curve_to_linear = 0;
    double cook_options_curve_refine_lod = 0.0;
    int32_t cook_options_clear_errors_and_warnings = 0;
    int32_t cook_options_cook_template_geos = 0;

    if(!enif_get_list_length(env, argv[0], &otl_search_path_length) ||
        !enif_get_list_length(env, argv[1], &dso_search_path_length))
    {
        nif_error = 1;
        goto label_cleanup;
    }

    if(otl_search_path_length > 0u)
    {
        otl_search_path = malloc((otl_search_path_length + 1u) * sizeof(char));

        if(enif_get_string(env, argv[0], otl_search_path, otl_search_path_length + 1u, ERL_NIF_LATIN1) < 1)
        {
            nif_error = 1;
            goto label_cleanup;
        }
    }

    if(dso_search_path_length > 0u)
    {
        dso_search_path = malloc((dso_search_path_length + 1u) * sizeof(char));

        if(enif_get_string(env, argv[1], dso_search_path, dso_search_path_length + 1u, ERL_NIF_LATIN1) < 1)
        {
            nif_error = 1;
            goto label_cleanup;
        }
    }

    {
        int32_t tuple_size = 0;
        const ERL_NIF_TERM* tuple_cook_options = NULL;

        if(!enif_get_tuple(env, argv[2], &tuple_size, &tuple_cook_options) || (tuple_size != 7) ||
            enif_compare(tuple_cook_options[0], enif_make_atom(env, "hapi_cook_options")) ||
            !enif_is_atom(env, tuple_cook_options[1]) ||
            !hapi_private_get_boolean(env, tuple_cook_options[1], &cook_options_split_geos_by_group) ||
            !enif_get_int(env, tuple_cook_options[2], &cook_options_max_vertices_per_primitive) ||
            !hapi_private_get_boolean(env, tuple_cook_options[3], &cook_options_refine_curve_to_linear) ||
            !enif_get_double(env, tuple_cook_options[4], &cook_options_curve_refine_lod) ||
            !hapi_private_get_boolean(env, tuple_cook_options[5], &cook_options_clear_errors_and_warnings) ||
            !hapi_private_get_boolean(env, tuple_cook_options[6], &cook_options_cook_template_geos))
        {
            nif_error = 1;
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

    if(nif_error)
    {
        return enif_make_badarg(env);
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
