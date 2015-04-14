#include "erl_nif.h"
#include "HAPI.h"

#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>


ERL_NIF_TERM hapi_private_make_atom(ErlNifEnv* env, const char* atom_name);
bool hapi_private_check_atom_value(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status);

ERL_NIF_TERM hapi_is_initialized_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hapi_initialize_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hapi_cleanup_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hapi_get_inv_int_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hapi_get_status_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


static ErlNifFunc nif_funcs[] =
{
    {"is_initialized", 0, hapi_is_initialized_impl},
    {"initialize", 5, hapi_initialize_impl},
    {"cleanup", 0, hapi_cleanup_impl},
    {"get_inv_int", 1, hapi_get_inv_int_impl},
    {"get_status", 1, hapi_get_status_impl}
};


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


bool
hapi_private_check_atom_value(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status)
{
    bool nif_success = true;

    uint32_t atom_len = 0;
    char* atom_value = NULL;

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

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    if(nif_success)
    {
        *status = (bool)(!strcmp(atom_value, value));
    }

    return nif_success;
}


ERL_NIF_INIT(hapi, nif_funcs, NULL, NULL, NULL, NULL)


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


ERL_NIF_TERM
hapi_is_initialized_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return hapi_private_process_result(env, HAPI_IsInitialized());
}


static
ERL_NIF_TERM
hapi_initialize_impl_helper(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool nif_success = true;

    uint32_t otl_search_path_length = 0;
    uint32_t dso_search_path_length = 0;

    char* otl_search_path = NULL;
    char* dso_search_path = NULL;

    bool otl_search_path_nil = false;
    bool dso_search_path_nil = false;

    bool cook_options_split_geos_by_group = true;
    int32_t cook_options_max_vertices_per_primitive = -1;
    bool cook_options_refine_curve_to_linear = false;
    double cook_options_curve_refine_lod = 8.0;
    bool cook_options_clear_errors_and_warnings = false;
    bool cook_options_cook_template_geos = false;

    bool use_cooking_thread = false;
    int32_t cooking_thread_stack_size = 0;

    HAPI_Result result = HAPI_RESULT_SUCCESS;

    // Process otl search path; we require either string or nil atom.
    if(enif_is_atom(env, argv[0]))
    {
        if(hapi_private_check_atom_value(env, argv[0], "nil", &otl_search_path_nil))
        {
            if(!otl_search_path_nil)
            {
                nif_success = false;
                goto label_cleanup;
            }
        }
        else
        {
            nif_success = false;
            goto label_cleanup;
        }
    }
    else
    {
        if(!enif_get_list_length(env, argv[0], &otl_search_path_length))
        {
            nif_success = false;
            goto label_cleanup;
        }

        if(otl_search_path_length > 0)
        {
            otl_search_path = malloc(otl_search_path_length + 1);
            memset(otl_search_path, 0, otl_search_path_length + 1);

            if(enif_get_string(env, argv[0], otl_search_path, otl_search_path_length + 1, ERL_NIF_LATIN1) < 1)
            {
                nif_success = false;
                goto label_cleanup;
            }
        }
    }

    // Process dso search path; we require either string or nil atom.
    if(enif_is_atom(env, argv[1]))
    {
        if(hapi_private_check_atom_value(env, argv[1], "nil", &dso_search_path_nil))
        {
            if(!dso_search_path_nil)
            {
                nif_success = false;
                goto label_cleanup;
            }
        }
        else
        {
            nif_success = false;
            goto label_cleanup;
        }
    }
    else
    {
        if(!enif_get_list_length(env, argv[1], &dso_search_path_length))
        {
            nif_success = false;
            goto label_cleanup;
        }

        if(dso_search_path_length > 0)
        {
            dso_search_path = malloc(dso_search_path_length + 1);
            memset(dso_search_path, 0, dso_search_path_length + 1);

            if(enif_get_string(env, argv[1], dso_search_path, dso_search_path_length + 1, ERL_NIF_LATIN1) < 1)
            {
                nif_success = false;
                goto label_cleanup;
            }
        }
    }

    // Create cook options.
    HAPI_CookOptions cook_options;
    HAPI_CookOptions_Init(&cook_options);

    // Process cook options, if nil, we use default.
    if(enif_is_atom(env, argv[2]))
    {
        bool nil_cook_options = false;
        if(hapi_private_check_atom_value(env, argv[2], "nil", &nil_cook_options))
        {
            if(!nil_cook_options)
            {
                nif_success = false;
                goto label_cleanup;
            }
        }
        else
        {
            nif_success = false;
            goto label_cleanup;
        }
    }
    else
    {
        int32_t tuple_size = 0;
        const ERL_NIF_TERM* tuple_cook_options = NULL;
        bool cook_options_record = false;

        if(!enif_get_tuple(env, argv[2], &tuple_size, &tuple_cook_options) ||
            (tuple_size != 7) ||
            !hapi_private_check_atom_value(env, tuple_cook_options[0], "hapi_cook_options", &cook_options_record) ||
            !cook_options_record ||
            !enif_is_atom(env, tuple_cook_options[1]) ||
            !hapi_private_check_atom_value(env, tuple_cook_options[1], "true", &cook_options_split_geos_by_group) ||
            !enif_get_int(env, tuple_cook_options[2], &cook_options_max_vertices_per_primitive) ||
            !enif_is_atom(env, tuple_cook_options[3]) ||
            !hapi_private_check_atom_value(env, tuple_cook_options[3], "true", &cook_options_refine_curve_to_linear) ||
            !enif_get_double(env, tuple_cook_options[4], &cook_options_curve_refine_lod) ||
            !enif_is_atom(env, tuple_cook_options[5]) ||
            !hapi_private_check_atom_value(env, tuple_cook_options[5], "true", &cook_options_clear_errors_and_warnings) ||
            !enif_is_atom(env, tuple_cook_options[6]) ||
            !hapi_private_check_atom_value(env, tuple_cook_options[6], "true", &cook_options_cook_template_geos))
        {
            nif_success = false;
            goto label_cleanup;
        }

        // Set cook options.
        cook_options.splitGeosByGroup = cook_options_split_geos_by_group;
        cook_options.maxVerticesPerPrimitive = cook_options_max_vertices_per_primitive;
        cook_options.refineCurveToLinear = cook_options_refine_curve_to_linear;
        cook_options.curveRefineLOD = cook_options_curve_refine_lod;
        cook_options.clearErrorsAndWarnings = cook_options_clear_errors_and_warnings;
        cook_options.cookTemplatedGeos = cook_options_cook_template_geos;
    }

    // Process cooking thread parameter, it must be a boolean.
    if(enif_is_atom(env, argv[3]))
    {
        if(!hapi_private_check_atom_value(env, argv[3], "true", &use_cooking_thread))
        {
            nif_success = false;
            goto label_cleanup;
        }
    }
    else
    {
        nif_success = false;
        goto label_cleanup;
    }

    // Process stack size parameter.
    if(!enif_get_int(env, argv[4], &cooking_thread_stack_size))
    {
        nif_success = false;
        goto label_cleanup;
    }

    // Execute HAPI Initialize.
    result = HAPI_Initialize(otl_search_path, dso_search_path, &cook_options, use_cooking_thread, cooking_thread_stack_size);

label_cleanup:

    if(otl_search_path)
    {
        free(otl_search_path);
    }

    if(dso_search_path)
    {
        free(dso_search_path);
    }

    if(!nif_success)
    {
        return enif_make_badarg(env);
    }

    return hapi_private_process_result(env, result);
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


ERL_NIF_TERM
hapi_get_inv_int_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return hapi_private_process_result(env, HAPI_RESULT_SUCCESS);
}


ERL_NIF_TERM
hapi_get_status_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return hapi_private_process_result(env, HAPI_RESULT_SUCCESS);
}
