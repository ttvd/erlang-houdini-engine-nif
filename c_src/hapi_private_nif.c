#include "hapi_private_nif.h"
#include "hapi_enums_nif.h"
#include "hapi_defines_nif.h"
#include "HAPI.h"

#include <stdbool.h>
#include <string.h>
#include <assert.h>


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


ERL_NIF_TERM
hapi_private_make_atom_bool(ErlNifEnv* env, bool value)
{
    if(value)
    {
        return hapi_private_make_atom(env, "true");
    }

    return hapi_private_make_atom(env, "false");
}


ERL_NIF_TERM
hapi_private_make_hash_tuple(ErlNifEnv* env, const char* atom_name)
{
    ERL_NIF_TERM atom = hapi_private_make_atom(env, atom_name);
    uint32_t atom_hash = XXH32(atom_name, strlen(atom_name), 0);

    return enif_make_tuple(env, 2, atom, enif_make_uint(env, atom_hash));
}


ERL_NIF_TERM
hapi_private_make_vector_float(ErlNifEnv* env, uint32_t size, const float* data)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for(int32_t idx = 0; idx < size; ++idx)
    {
        list = enif_make_list_cell(env, enif_make_int(env, (double) *(data + idx)), list);
    }

    return list;
}


ERL_NIF_TERM
hapi_private_make_result_tuple_int(ErlNifEnv* env, HAPI_Result result, int32_t value)
{
    if(HAPI_RESULT_SUCCESS == result)
    {
        return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_int(env, value));
    }

    return hapi_enum_result_c_to_erl(env, result);
}


ERL_NIF_TERM
hapi_private_make_result_tuple_double(ErlNifEnv* env, HAPI_Result result, double value)
{
    if(HAPI_RESULT_SUCCESS == result)
    {
        return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_double(env, value));
    }

    return hapi_enum_result_c_to_erl(env, result);
}


ERL_NIF_TERM
hapi_private_make_result_tuple_bool(ErlNifEnv* env, HAPI_Result result, bool value)
{
    if(HAPI_RESULT_SUCCESS == result)
    {
        return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), hapi_private_make_atom_bool(env, value));
    }

    return hapi_enum_result_c_to_erl(env, result);
}


ERL_NIF_TERM
hapi_private_make_result_tuple_string(ErlNifEnv* env, HAPI_Result result, const char* value)
{
    assert(value != NULL);

    if(HAPI_RESULT_SUCCESS == result)
    {
        return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_string(env, value, ERL_NIF_LATIN1));
    }

    return hapi_enum_result_c_to_erl(env, result);
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

    if(atom_len < HAPI_STACK_STRING_SIZE_MAX)
    {
        char atom_buffer[HAPI_STACK_STRING_SIZE_MAX];
        memset(atom_buffer, 0, HAPI_STACK_STRING_SIZE_MAX);

        if(!enif_get_atom(env, term, atom_buffer, atom_len + 1, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        *status = (bool)(!strcmp(atom_buffer, value));
    }
    else
    {
        atom_value = malloc(atom_len + 1);
        memset(atom_value, 0, atom_len + 1);

        if(!enif_get_atom(env, term, atom_value, atom_len + 1, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        *status = (bool)(!strcmp(atom_value, value));
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    return nif_success;
}


bool
hapi_private_check_nil(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status)
{
    if(enif_is_atom(env, term))
    {
        bool nil_status = false;
        if(hapi_private_check_atom_value(env, term, "nil", &nil_status))
        {
            *status = nil_status;
            return true;
        }
    }

    return false;
}


bool
hapi_private_check_bool(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status)
{
    bool nif_success = true;
    uint32_t atom_len = 0;
    char* atom_value = NULL;
    char atom_buffer[HAPI_STACK_STRING_SIZE_MAX];

    if(enif_is_atom(env, term))
    {
        if(!enif_get_atom_length(env, term, &atom_len, ERL_NIF_LATIN1))
        {
            return false;
        }

        if(atom_len + 1 < HAPI_STACK_STRING_SIZE_MAX)
        {
            memset(atom_buffer, 0, HAPI_STACK_STRING_SIZE_MAX);

            if(!enif_get_atom(env, term, atom_buffer, atom_len + 1, ERL_NIF_LATIN1))
            {
                return false;
            }

            atom_value = atom_buffer;
        }
        else
        {
            atom_value = malloc(atom_len + 1);
            memset(atom_value, 0, atom_len + 1);

            if(!enif_get_atom(env, term, atom_value, atom_len + 1, ERL_NIF_LATIN1))
            {
                nif_success = false;
                goto label_cleanup;
            }
        }

        if(!strcmp(atom_value, "true"))
        {
            *status = true;
        }
        else if(!strcmp(atom_value, "false"))
        {
            *status = false;
        }
        else
        {
            nif_success = false;
        }
    }

label_cleanup:

    if(atom_len + 1 >= HAPI_STACK_STRING_SIZE_MAX)
    {
        free(atom_value);
    }

    return nif_success;
}


bool
hapi_private_get_string(ErlNifEnv* env, const ERL_NIF_TERM term, char** string, uint32_t* string_length)
{
    uint32_t length = 0;
    char* buffer = NULL;

    if(!enif_get_list_length(env, term, &length))
    {
        return false;
    }

    if(length > 0)
    {
        buffer = malloc(length + 1);
        memset(buffer, 0, length + 1);

        if(enif_get_string(env, term, buffer, length + 1, ERL_NIF_LATIN1) < 1)
        {
            free(buffer);
            return false;
        }
    }

    *string_length = length;
    *string = buffer;
    return true;
}


bool
hapi_private_get_hapi_cook_options(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CookOptions* cook_options)
{
    int32_t tuple_size;
    const ERL_NIF_TERM* tuple_cook_options = NULL;
    bool cook_options_record = false;

    bool cook_options_split_geos_by_group = true;
    int32_t cook_options_max_vertices_per_primitive = -1;
    bool cook_options_refine_curve_to_linear = false;
    double cook_options_curve_refine_lod = 8.0;
    bool cook_options_clear_errors_and_warnings = false;
    bool cook_options_cook_template_geos = false;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_cook_options) ||
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
        return false;
    }

    // Set cook options.
    cook_options->splitGeosByGroup = cook_options_split_geos_by_group;
    cook_options->maxVerticesPerPrimitive = cook_options_max_vertices_per_primitive;
    cook_options->refineCurveToLinear = cook_options_refine_curve_to_linear;
    cook_options->curveRefineLOD = cook_options_curve_refine_lod;
    cook_options->clearErrorsAndWarnings = cook_options_clear_errors_and_warnings;
    cook_options->cookTemplatedGeos = cook_options_cook_template_geos;

    return true;
}


static
bool
hapi_private_get_id_helper(ErlNifEnv* env, const ERL_NIF_TERM term, int32_t* hapi_id_out)
{
    int32_t hapi_id = -1;

    if(enif_get_int(env, term, (int32_t*) &hapi_id))
    {
        *hapi_id_out = hapi_id;
        return true;
    }

    return false;
}

bool
hapi_private_get_hapi_asset_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetId* asset_id)
{
    return hapi_private_get_id_helper(env, term, (int32_t*) asset_id);
}


bool
hapi_private_get_hapi_asset_library_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetLibraryId* asset_library_id)
{
    return hapi_private_get_id_helper(env, term, (int32_t*) asset_library_id);
}


bool
hapi_private_get_hapi_node_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_NodeId* node_id)
{
    return hapi_private_get_id_helper(env, term, (int32_t*) node_id);
}


bool
hapi_private_get_vector(ErlNifEnv* env, const ERL_NIF_TERM term, uint32_t size, double* data)
{
    uint32_t list_size = 0;
    ERL_NIF_TERM head, tail;

    if(enif_get_list_length(env, term, &list_size) &&
        (list_size == size))
    {
        ERL_NIF_TERM list = term;
        int32_t index = 0;

        while(enif_get_list_cell(env, list, &head, &tail))
        {
            double param_double = 0.0;
            int param_int = 0;

            if(enif_get_double(env, head, &param_double))
            {
                *(data + index) = param_double;
            }
            else if(enif_get_int(env, head, &param_int))
            {
                *(data + index) = (double) param_int;
            }
            else
            {
                return false;
            }

            index++;
            list = tail;
        }

        return true;
    }

    return false;
}
