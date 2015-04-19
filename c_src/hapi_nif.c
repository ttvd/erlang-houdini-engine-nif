#include "hapi_private_nif.h"
#include "hapi_enums_nif.h"
#include "hapi_functions_nif.h"
#include "hapi_defines_nif.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>


// HAPI_IsInitialized equivalent.
ERL_NIF_TERM
hapi_is_initialized_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return hapi_enum_result_c_to_erl(env, HAPI_IsInitialized());
}


// Helper initialization function invoked by scheduler.
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
            !hapi_private_check_bool(env, tuple_cook_options[1], &cook_options_split_geos_by_group) ||
            !enif_get_int(env, tuple_cook_options[2], &cook_options_max_vertices_per_primitive) ||
            !enif_is_atom(env, tuple_cook_options[3]) ||
            !hapi_private_check_bool(env, tuple_cook_options[3], &cook_options_refine_curve_to_linear) ||
            !enif_get_double(env, tuple_cook_options[4], &cook_options_curve_refine_lod) ||
            !enif_is_atom(env, tuple_cook_options[5]) ||
            !hapi_private_check_bool(env, tuple_cook_options[5], &cook_options_clear_errors_and_warnings) ||
            !enif_is_atom(env, tuple_cook_options[6]) ||
            !hapi_private_check_bool(env, tuple_cook_options[6], &cook_options_cook_template_geos))
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
    if(!hapi_private_check_bool(env, argv[3], &use_cooking_thread))
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

    return hapi_enum_result_c_to_erl(env, result);
}


// HAPI_Initialize equivalent.
ERL_NIF_TERM
hapi_initialize_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_initialize_impl_helper", 0, hapi_initialize_impl_helper, argc, argv);
}


// HAPI_Cleanup equivalent.
ERL_NIF_TERM
hapi_cleanup_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return hapi_enum_result_c_to_erl(env, HAPI_Cleanup());
}


// HAPI_GetEnvInt equivalent.
ERL_NIF_TERM
hapi_get_env_int_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_EnvIntType envint_type;

    if(hapi_enum_env_int_type_erl_to_c(env, argv[0], &envint_type))
    {
        int32_t envint_value = 0;
        HAPI_Result result = HAPI_GetEnvInt(envint_type, &envint_value);

        return hapi_private_make_result_tuple_int(env, result, envint_value);
    }

    return enif_make_badarg(env);
}


// HAPI_GetStatus equivalent.
ERL_NIF_TERM
hapi_get_status_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_StatusType status_type;

    if(hapi_enum_status_type_erl_to_c(env, argv[0], &status_type))
    {
        int32_t status_value = 0;
        HAPI_Result result = HAPI_GetStatus(status_type, &status_value);

        return hapi_private_make_result_tuple_int(env, result, status_value);
    }

    return enif_make_badarg(env);
}


// HAPI_GetStatusStringBufLength equivalent.
ERL_NIF_TERM
hapi_get_status_string_buf_length_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_StatusType status_type;
    HAPI_StatusVerbosity status_verbosity;

    if(hapi_enum_status_type_erl_to_c(env, argv[0], &status_type) &&
        hapi_enum_status_verbosity_erl_to_c(env, argv[1], &status_verbosity))
    {
        int32_t buffer_size = 0;
        HAPI_Result result = HAPI_GetStatusStringBufLength(status_type, status_verbosity, &buffer_size);

        return hapi_private_make_result_tuple_int(env, result, buffer_size);
    }

    return enif_make_badarg(env);
}


// HAPI_GetStatusString equivalent.
ERL_NIF_TERM
hapi_get_status_string_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_StatusType status_type;
    HAPI_StatusVerbosity status_verbosity = HAPI_STATUSVERBOSITY_ERRORS;

    if(hapi_enum_status_type_erl_to_c(env, argv[0], &status_type))
    {
        int32_t buffer_size = 0;
        HAPI_Result result = HAPI_GetStatusStringBufLength(status_type, status_verbosity, &buffer_size);

        if(HAPI_RESULT_SUCCESS == result)
        {
            char* buffer = NULL;
            char stack_buffer[HAPI_STACK_STRING_SIZE_MAX];

            if(buffer_size < HAPI_STACK_STRING_SIZE_MAX)
            {
                memset(stack_buffer, 0, HAPI_STACK_STRING_SIZE_MAX);
                buffer = stack_buffer;
            }
            else
            {
                buffer = malloc(buffer_size);
                memset(buffer, 0, buffer_size);
            }

            result = HAPI_GetStatusString(status_type, stack_buffer);
            ERL_NIF_TERM result_atom = hapi_private_make_result_tuple_string(env, result, buffer);

            if(buffer_size >= HAPI_STACK_STRING_SIZE_MAX)
            {
                free(buffer);
            }

            return result_atom;
        }
        else
        {
            return hapi_enum_result_c_to_erl(env, result);
        }
    }

    return enif_make_badarg(env);
}

// HAPI_GetCookingTotalCount equivalent.
ERL_NIF_TERM
hapi_get_cooking_total_count_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t total_count = 0;
    HAPI_Result result = HAPI_GetCookingTotalCount(&total_count);

    return hapi_private_make_result_tuple_int(env, result, total_count);
}


// HAPI_GetCookingCurrentCount equivalent.
ERL_NIF_TERM
hapi_get_cooking_current_count_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t current_count = 0;
    HAPI_Result result = HAPI_GetCookingCurrentCount(&current_count);

    return hapi_private_make_result_tuple_int(env, result, current_count);
}

// HAPI_ConvertTransform equivalent.
ERL_NIF_TERM
hapi_convert_transform_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // HAPI_RSTOrder rst_order
    // HAPI_XYZOrder rot_order

    // HAPI_TransformEuler * transform_in_out

    // Needs implementation.
    assert(false);
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ConvertMatrixToQuat equivalent.
ERL_NIF_TERM
hapi_convert_matrix_to_quat_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    assert(false);
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ConvertMatrixToEuler equivalent.
ERL_NIF_TERM
hapi_convert_matrix_to_euler_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    assert(false);
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ConvertTransformQuatToMatrix equivalent.
ERL_NIF_TERM
hapi_convert_transform_quat_to_matrix_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    assert(false);
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ConvertTransformEulerToMatrix equivalent.
ERL_NIF_TERM
hapi_convert_transform_euler_to_matrix_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    assert(false);
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_PythonThreadInterpreterLock equivalent.
ERL_NIF_TERM
hapi_python_thread_interpreter_lock_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool lock_flag = true;

    if(hapi_private_check_bool(env, argv[0], &lock_flag))
    {
        return hapi_enum_result_c_to_erl(env, HAPI_PythonThreadInterpreterLock(lock_flag));
    }

    return enif_make_badarg(env);
}

// HAPI_GetStringBufLength equivalent.
ERL_NIF_TERM
hapi_get_string_buf_length_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_StringHandle string_handle;

    if(enif_get_int(env, argv[0], (int32_t*) &string_handle))
    {
        int32_t string_buf_length = 0;
        HAPI_Result result = HAPI_GetStringBufLength(string_handle, &string_buf_length);

        return hapi_private_make_result_tuple_int(env, result, string_buf_length);
    }

    return enif_make_badarg(env);
}

// HAPI_GetString equivalent.
ERL_NIF_TERM
hapi_get_string_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_StringHandle string_handle;
    int32_t string_length;

    if(enif_get_int(env, argv[0], (int32_t*) &string_handle) && enif_get_int(env, argv[1], &string_length))
    {
        char* buffer = NULL;
        char stack_buffer[HAPI_STACK_STRING_SIZE_MAX];

        if(string_length < HAPI_STACK_STRING_SIZE_MAX)
        {
            memset(stack_buffer, 0, HAPI_STACK_STRING_SIZE_MAX);
            buffer = stack_buffer;
        }
        else
        {
            buffer = malloc(string_length);
            memset(buffer, 0, string_length);
        }

        HAPI_Result result = HAPI_GetString(string_handle, buffer, string_length);
        ERL_NIF_TERM result_atom = hapi_private_make_result_tuple_string(env, result, buffer);

        if(string_length >= HAPI_STACK_STRING_SIZE_MAX)
        {
            free(buffer);
        }

        return result_atom;
    }

    return enif_make_badarg(env);
}

// HAPI_GetTime equivalent.
ERL_NIF_TERM
hapi_get_time_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    float time_value = 0.0f;

    HAPI_Result result = HAPI_GetTime(&time_value);

    return hapi_private_make_result_tuple_double(env, result, (double) time_value);
}

// HAPI_SetTime equivalent.
ERL_NIF_TERM
hapi_set_time_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double time_value = 0.0;

    if(enif_get_double(env, argv[0], &time_value))
    {
        return hapi_enum_result_c_to_erl(env, HAPI_SetTime(time_value));
    }

    return enif_make_badarg(env);
}

// HAPI_GetTimelineOptions equivalent.
ERL_NIF_TERM
hapi_get_timeline_options_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_TimelineOptions timeline_options;

    HAPI_Result result = HAPI_GetTimelineOptions(&timeline_options);
    ERL_NIF_TERM result_atom = hapi_enum_result_c_to_erl(env, result);

    if(HAPI_RESULT_SUCCESS == result)
    {
        ERL_NIF_TERM record_timeline_options = enif_make_tuple4(env,
            hapi_private_make_atom(env, "hapi_timeline_options"),
            enif_make_double(env, (double) timeline_options.fps),
            enif_make_double(env, (double) timeline_options.startTime),
            enif_make_double(env, (double) timeline_options.endTime));

        return enif_make_tuple(env, 2, result_atom, record_timeline_options);
    }

    return result_atom;
}

// HAPI_SetTimelineOptions equivalent.
ERL_NIF_TERM
hapi_set_timeline_options_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* tuple_timeline_options = NULL;
    bool timeline_options_record = false;

    double timeline_options_fps = 0.0;
    double timeline_options_start_time = 0.0;
    double timeline_options_end_time = 0.0;

    if(enif_get_tuple(env, argv[0], &tuple_size, &tuple_timeline_options) &&
        (4 == tuple_size) &&
        hapi_private_check_atom_value(env, tuple_timeline_options[0], "hapi_timeline_options", &timeline_options_record) &&
        timeline_options_record)
    {
        int32_t int_value;

        if(!enif_get_double(env, tuple_timeline_options[1], &timeline_options_fps))
        {
            if(!enif_get_int(env, tuple_timeline_options[1], &int_value))
            {
                return enif_make_badarg(env);
            }

            timeline_options_fps = (double) int_value;
        }

        if(!enif_get_double(env, tuple_timeline_options[2], &timeline_options_start_time))
        {
            if(!enif_get_int(env, tuple_timeline_options[2], &int_value))
            {
                return enif_make_badarg(env);
            }

            timeline_options_start_time = (double) int_value;
        }

        if(!enif_get_double(env, tuple_timeline_options[3], &timeline_options_end_time))
        {
            if(!enif_get_int(env, tuple_timeline_options[3], &int_value))
            {
                return enif_make_badarg(env);
            }

            timeline_options_end_time = (double) int_value;
        }

        HAPI_TimelineOptions timeline_options;
        timeline_options.fps = (float) timeline_options_fps;
        timeline_options.startTime = (float) timeline_options_start_time;
        timeline_options.endTime = (float) timeline_options_end_time;

        return hapi_enum_result_c_to_erl(env, HAPI_SetTimelineOptions(&timeline_options));
    }

    return enif_make_badarg(env);
}

// HAPI_IsAssetValid equivalent.
ERL_NIF_TERM
hapi_is_asset_valid_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_AssetId asset_id = -1;
    int32_t asset_validation_id = 0;

    if(enif_get_int(env, argv[0], (int32_t*) &asset_id) && enif_get_int(env, argv[1], &asset_validation_id))
    {
        int32_t is_valid = 0;
        HAPI_Result result = HAPI_IsAssetValid(asset_id, asset_validation_id, &is_valid);

        return hapi_private_make_result_tuple_bool(env, result, (0 != is_valid));
    }

    return enif_make_badarg(env);
}

// HAPI_LoadAssetLibraryFromFile equivalent.
ERL_NIF_TERM
hapi_load_asset_library_from_file_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool is_nil = false;
    bool allow_overwrite = false;

    HAPI_AssetLibraryId library_id = -1;
    HAPI_Result result = HAPI_RESULT_SUCCESS;

    if(!hapi_private_check_bool(env, argv[1], &allow_overwrite))
    {
        return enif_make_badarg(env);
    }

    if(hapi_private_check_nil(env, argv[0], &is_nil))
    {
        if(is_nil)
        {
            result = HAPI_LoadAssetLibraryFromFile("", allow_overwrite, &library_id);
            return hapi_private_make_result_tuple_int(env, result, (int32_t) library_id);
        }
        else
        {
            return enif_make_badarg(env);
        }
    }

    char* buffer = NULL;
    uint32_t buffer_length = 0;

    if(hapi_private_get_string(env, argv[0], &buffer, &buffer_length))
    {
        result = HAPI_LoadAssetLibraryFromFile(buffer, allow_overwrite, &library_id);

        if(buffer)
        {
            free(buffer);
        }

        return hapi_private_make_result_tuple_int(env, result, (int32_t) library_id);
    }

    return enif_make_badarg(env);
}

// HAPI_LoadAssetLibraryFromMemory equivalent.
ERL_NIF_TERM
hapi_load_asset_library_from_memory_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    assert(false);
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetAvailableAssetCount equivalent.
ERL_NIF_TERM
hapi_get_available_asset_count_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_AssetLibraryId asset_library_id = -1;

    if(enif_get_int(env, argv[0], (int32_t*) &asset_library_id))
    {
        int32_t asset_count = 0;
        HAPI_Result result = HAPI_GetAvailableAssetCount(asset_library_id, &asset_count);

        return hapi_private_make_result_tuple_int(env, result, asset_count);
    }

    return enif_make_badarg(env);
}

// HAPI_GetAvailableAssets equivalent.
ERL_NIF_TERM
hapi_get_available_assets_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_AssetLibraryId asset_library_id = -1;
    int32_t asset_count = 0;

    if(enif_get_int(env, argv[0], (int32_t*) &asset_library_id) && enif_get_int(env, argv[1], (int32_t*) &asset_count))
    {
        assert(asset_count >= 0);
        ERL_NIF_TERM list = enif_make_list(env, 0);

        if(asset_count > 0)
        {
            HAPI_StringHandle* handles = malloc(asset_count * sizeof(HAPI_StringHandle));
            HAPI_Result result = HAPI_GetAvailableAssets(asset_library_id, handles, asset_count);

            if(HAPI_RESULT_SUCCESS == result)
            {
                for(int32_t handle_idx = 0; handle_idx < asset_count; ++handle_idx)
                {
                    list = enif_make_list_cell(env, enif_make_int(env, handles[handle_idx]), list);
                }
            }

            free(handles);

            return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), list);
        }
    }

    return enif_make_badarg(env);
}

// Helper intstantiation function invoked by scheduler.
static
ERL_NIF_TERM
hapi_instantiate_asset_impl_helper(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char* asset_name = NULL;
    uint32_t asset_name_length = 0;
    bool cook_on_load = false;

    if(hapi_private_get_string(env, argv[0], &asset_name, &asset_name_length) &&
        hapi_private_check_bool(env, argv[1], &cook_on_load))
    {
        HAPI_AssetId asset_id = -1;
        HAPI_Result result = HAPI_InstantiateAsset(asset_name, cook_on_load, &asset_id);

        if(asset_name)
        {
            free(asset_name);
        }

        return hapi_private_make_result_tuple_int(env, result, asset_id);
    }

    return enif_make_badarg(env);
}

// HAPI_InstantiateAsset equivalent.
ERL_NIF_TERM
hapi_instantiate_asset_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_instantiate_asset_impl_helper", 0, hapi_instantiate_asset_impl_helper, argc, argv);
}

// HAPI_CreateCurve equivalent.
ERL_NIF_TERM
hapi_create_curve_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    assert(false);
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_CreateInputAsset equivalent.
ERL_NIF_TERM
hapi_create_input_asset_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    assert(false);
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_DestroyAsset equivalent.
ERL_NIF_TERM
hapi_destroy_asset_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_AssetId asset_id = -1;

    if(enif_get_int(env, argv[0], (int32_t*) &asset_id))
    {
        return hapi_enum_result_c_to_erl(env, HAPI_DestroyAsset(asset_id));
    }

    return enif_make_badarg(env);
}

// HAPI_GetAssetInfo equivalent.
ERL_NIF_TERM
hapi_get_asset_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_AssetId asset_id = -1;

    if(enif_get_int(env, argv[0], (int32_t*) &asset_id))
    {
        HAPI_AssetInfo asset_info;
        HAPI_Result result = HAPI_GetAssetInfo(asset_id, &asset_info);

        if(HAPI_RESULT_SUCCESS == result)
        {
            ERL_NIF_TERM asset_info_tuple = enif_make_tuple(env, 20,
                hapi_private_make_atom(env, "hapi_asset_info"),
                enif_make_int(env, asset_info.id),
                enif_make_int(env, asset_info.type),
                enif_make_int(env, asset_info.subType),
                enif_make_int(env, asset_info.validationId),
                enif_make_int(env, asset_info.nodeId),
                enif_make_int(env, asset_info.objectNodeId),
                hapi_private_make_atom_bool(env, asset_info.hasEverCooked),
                enif_make_int(env, asset_info.nameSH),
                enif_make_int(env, asset_info.labelSH),
                enif_make_int(env, asset_info.filePathSH),
                enif_make_int(env, asset_info.versionSH),
                enif_make_int(env, asset_info.fullOpNameSH),
                enif_make_int(env, asset_info.helpTextSH),

                enif_make_int(env, asset_info.objectCount),
                enif_make_int(env, asset_info.handleCount),
                enif_make_int(env, asset_info.transformInputCount),
                enif_make_int(env, asset_info.geoInputCount),

                hapi_private_make_atom_bool(env, asset_info.haveObjectsChanged),
                hapi_private_make_atom_bool(env, asset_info.haveMaterialsChanged));

            return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), asset_info_tuple);
        }

        return hapi_enum_result_c_to_erl(env, result);
    }

    return enif_make_badarg(env);
}

// HAPI_CookAsset equivalent.
ERL_NIF_TERM
hapi_cook_asset_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_Interrupt equivalent.
ERL_NIF_TERM
hapi_interrupt_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetAssetTransform equivalent.
ERL_NIF_TERM
hapi_get_asset_transform_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetAssetTransform equivalent.
ERL_NIF_TERM
hapi_set_asset_transform_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetInputName equivalent.
ERL_NIF_TERM
hapi_get_input_name_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_LoadHIPFile equivalent.
ERL_NIF_TERM
hapi_load_hip_file_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_CheckForNewAssets equivalent.
ERL_NIF_TERM
hapi_check_for_new_assets_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetNewAssetIds equivalent.
ERL_NIF_TERM
hapi_get_new_asset_ids_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SaveHIPFile equivalent.
ERL_NIF_TERM
hapi_save_hip_file_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetNodeInfo equivalent.
ERL_NIF_TERM
hapi_get_node_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetGlobalNodes equivalent.
ERL_NIF_TERM
hapi_get_global_nodes_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParameters equivalent.
ERL_NIF_TERM
hapi_get_parameters_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmInfo equivalent.
ERL_NIF_TERM
hapi_get_parm_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmIdFromName equivalent.
ERL_NIF_TERM
hapi_get_parm_id_from_name_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmInfoFromName equivalent.
ERL_NIF_TERM
hapi_get_parm_info_from_name_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmIntValue equivalent.
ERL_NIF_TERM
hapi_get_parm_int_value_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmIntValues equivalent.
ERL_NIF_TERM
hapi_get_parm_int_values_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmFloatValue equivalent.
ERL_NIF_TERM
hapi_get_parm_float_value_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmFloatValues equivalent.
ERL_NIF_TERM
hapi_get_parm_float_values_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmStringValue equivalent.
ERL_NIF_TERM
hapi_get_parm_string_value_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmStringValues equivalent.
ERL_NIF_TERM
hapi_get_parm_string_values_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetParmChoiceLists equivalent.
ERL_NIF_TERM
hapi_get_parm_choice_lists_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetParmIntValue equivalent.
ERL_NIF_TERM
hapi_set_parm_int_value_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetParmIntValues equivalent.
ERL_NIF_TERM
hapi_set_parm_int_values_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetParmFloatValue equivalent.
ERL_NIF_TERM
hapi_set_parm_float_value_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetParmFloatValues equivalent.
ERL_NIF_TERM
hapi_set_parm_float_values_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetParmStringValue equivalent.
ERL_NIF_TERM
hapi_set_parm_string_value_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_InsertMultiparmInstance equivalent.
ERL_NIF_TERM
hapi_insert_multiparm_instance_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_RemoveMultiparmInstance equivalent.
ERL_NIF_TERM
hapi_remove_multiparm_instance_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetHandleInfo equivalent.
ERL_NIF_TERM
hapi_get_handle_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetHandleBindingInfo equivalent.
ERL_NIF_TERM
hapi_get_handle_binding_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetPresetBufLength equivalent.
ERL_NIF_TERM
hapi_get_preset_buf_length_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetPreset equivalent.
ERL_NIF_TERM
hapi_get_preset_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetPreset equivalent.
ERL_NIF_TERM
hapi_set_preset_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetObjects equivalent.
ERL_NIF_TERM
hapi_get_objects_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetObjectTransforms equivalent.
ERL_NIF_TERM
hapi_get_object_transforms_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetInstanceTransforms equivalent.
ERL_NIF_TERM
hapi_get_instance_transforms_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetObjectTransform equivalent.
ERL_NIF_TERM
hapi_set_object_transform_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetGeoInfo equivalent.
ERL_NIF_TERM
hapi_get_geo_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetPartInfo equivalent.
ERL_NIF_TERM
hapi_get_part_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetFaceCounts equivalent.
ERL_NIF_TERM
hapi_get_face_counts_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetVertexList equivalent.
ERL_NIF_TERM
hapi_get_vertex_list_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetAttributeInfo equivalent.
ERL_NIF_TERM
hapi_get_attribute_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetAttributeNames equivalent.
ERL_NIF_TERM
hapi_get_attribute_names_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetAttributeIntData equivalent.
ERL_NIF_TERM
hapi_get_attribute_int_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetAttributeFloatData equivalent.
ERL_NIF_TERM
hapi_get_attribute_float_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetAttributeStringData equivalent.
ERL_NIF_TERM
hapi_get_attribute_string_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetGroupNames equivalent.
ERL_NIF_TERM
hapi_get_group_names_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetGroupMembership equivalent.
ERL_NIF_TERM
hapi_get_group_membership_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetGeoInfo equivalent.
ERL_NIF_TERM
hapi_set_geo_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetPartInfo equivalent.
ERL_NIF_TERM
hapi_set_part_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetFaceCounts equivalent.
ERL_NIF_TERM
hapi_set_face_counts_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetVertexList equivalent.
ERL_NIF_TERM
hapi_set_vertex_list_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_AddAttribute equivalent.
ERL_NIF_TERM
hapi_add_attribute_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetAttributeIntData equivalent.
ERL_NIF_TERM
hapi_set_attribute_int_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetAttributeFloatData equivalent.
ERL_NIF_TERM
hapi_set_attribute_float_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetAttributeStringData equivalent.
ERL_NIF_TERM
hapi_set_attribute_string_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_AddGroup equivalent.
ERL_NIF_TERM
hapi_add_group_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetGroupMembership equivalent.
ERL_NIF_TERM
hapi_set_group_membership_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_CommitGeo equivalent.
ERL_NIF_TERM
hapi_commit_geo_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_RevertGeo equivalent.
ERL_NIF_TERM
hapi_revert_geo_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ConnectAssetTransform equivalent.
ERL_NIF_TERM
hapi_connect_asset_transform_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_DisconnectAssetTransform equivalent.
ERL_NIF_TERM
hapi_disconnect_asset_transform_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ConnectAssetGeometry equivalent.
ERL_NIF_TERM
hapi_connect_asset_geometry_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_DisconnectAssetGeometry equivalent.
ERL_NIF_TERM
hapi_disconnect_asset_geometry_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetMaterialIdsOnFaces equivalent.
ERL_NIF_TERM
hapi_get_material_ids_on_faces_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetMaterialInfo equivalent.
ERL_NIF_TERM
hapi_get_material_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_RenderMaterialToImage equivalent.
ERL_NIF_TERM
hapi_render_material_to_image_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_RenderTextureToImage equivalent.
ERL_NIF_TERM
hapi_render_texture_to_image_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetSupportedImageFileFormatCount equivalent.
ERL_NIF_TERM
hapi_get_supported_image_file_format_count_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetSupportedImageFileFormats equivalent.
ERL_NIF_TERM
hapi_get_supported_image_file_formats_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetImageInfo equivalent.
ERL_NIF_TERM
hapi_get_image_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetImageInfo equivalent.
ERL_NIF_TERM
hapi_set_image_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetImagePlaneCount equivalent.
ERL_NIF_TERM
hapi_get_image_plane_count_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetImagePlanes equivalent.
ERL_NIF_TERM
hapi_get_image_planes_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ExtractImageToFile equivalent.
ERL_NIF_TERM
hapi_extract_image_to_file_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ExtractImageToMemory equivalent.
ERL_NIF_TERM
hapi_extract_image_to_memory_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetImageMemoryBuffer equivalent.
ERL_NIF_TERM
hapi_get_image_memory_buffer_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetAnimCurve equivalent.
ERL_NIF_TERM
hapi_set_anim_curve_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetTransformAnimCurve equivalent.
ERL_NIF_TERM
hapi_set_transform_anim_curve_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ResetSimulation equivalent.
ERL_NIF_TERM
hapi_reset_simulation_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetVolumeInfo equivalent.
ERL_NIF_TERM
hapi_get_volume_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetFirstVolumeTile equivalent.
ERL_NIF_TERM
hapi_get_first_volume_tile_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetNextVolumeTile equivalent.
ERL_NIF_TERM
hapi_get_next_volume_tile_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetVolumeTileFloatData equivalent.
ERL_NIF_TERM
hapi_get_volume_tile_float_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetVolumeTileIntData equivalent.
ERL_NIF_TERM
hapi_get_volume_tile_int_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetVolumeInfo equivalent.
ERL_NIF_TERM
hapi_set_volume_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetVolumeTileFloatData equivalent.
ERL_NIF_TERM
hapi_set_volume_tile_float_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetVolumeTileIntData equivalent.
ERL_NIF_TERM
hapi_set_volume_tile_int_data_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetCurveInfo equivalent.
ERL_NIF_TERM
hapi_get_curve_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetCurveCounts equivalent.
ERL_NIF_TERM
hapi_get_curve_counts_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetCurveOrders equivalent.
ERL_NIF_TERM
hapi_get_curve_orders_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetCurveKnots equivalent.
ERL_NIF_TERM
hapi_get_curve_knots_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetCurveInfo equivalent.
ERL_NIF_TERM
hapi_set_curve_info_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetCurveCounts equivalent.
ERL_NIF_TERM
hapi_set_curve_counts_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetCurveOrders equivalent.
ERL_NIF_TERM
hapi_set_curve_orders_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SetCurveKnots equivalent.
ERL_NIF_TERM
hapi_set_curve_knots_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SaveGeoToFile equivalent.
ERL_NIF_TERM
hapi_save_geo_to_file_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_LoadGeoFromFile equivalent.
ERL_NIF_TERM
hapi_load_geo_from_file_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_GetGeoSize equivalent.
ERL_NIF_TERM
hapi_get_geo_size_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_SaveGeoToMemory equivalent.
ERL_NIF_TERM
hapi_save_geo_to_memory_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_LoadGeoFromMemory equivalent.
ERL_NIF_TERM
hapi_load_geo_from_memory_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}
