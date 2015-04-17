#include "hapi_private_nif.h"
#include "hapi_enums_nif.h"
#include "hapi_functions_nif.h"

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

        return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_int(env, envint_value));
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

        return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_int(env, status_value));
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

        return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_int(env, buffer_size));
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

            if(buffer_size <= 0)
            {
                buffer_size = 1;
            }

            buffer = malloc(buffer_size);
            memset(buffer, 0, buffer_size);

            result = HAPI_GetStatusString(status_type, buffer);

            ERL_NIF_TERM result_atom = enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result),
                enif_make_string(env, buffer, ERL_NIF_LATIN1));

            free(buffer);
            return result_atom;
        }
    }

    return enif_make_badarg(env);
}


// HAPI_GetCookingTotalCount equivalent.
ERL_NIF_TERM
hapi_get_cooking_total_count_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t cooking_total_count = 0;

    HAPI_Result result = HAPI_GetCookingTotalCount(&cooking_total_count);

    return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_int(env, cooking_total_count));
}


// HAPI_GetCookingCurrentCount equivalent.
ERL_NIF_TERM
hapi_get_cooking_current_count_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t cooking_current_count = 0;

    HAPI_Result result = HAPI_GetCookingCurrentCount(&cooking_current_count);

    return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_int(env, cooking_current_count));

}


// HAPI_ConvertTransform equivalent.
ERL_NIF_TERM
hapi_convert_transform_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ConvertMatrixToQuat equivalent.
ERL_NIF_TERM
hapi_convert_matrix_to_quat_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}


// HAPI_ConvertMatrixToEuler equivalent.
ERL_NIF_TERM
hapi_convert_matrix_to_euler_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}


// HAPI_ConvertTransformQuatToMatrix equivalent.
ERL_NIF_TERM
hapi_convert_transform_quat_to_matrix_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}

// HAPI_ConvertTransformEulerToMatrix equivalent.
ERL_NIF_TERM
hapi_convert_transform_euler_to_matrix_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Needs implementation.
    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);
}
