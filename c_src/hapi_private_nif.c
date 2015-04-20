/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL

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
hapi_private_make_hapi_parm_info(ErlNifEnv* env, HAPI_ParmId id, HAPI_ParmId parent_id, HAPI_ParmType type,
    HAPI_StringHandle type_info_sh, HAPI_Permissions permissions, int32_t size, int32_t choice_count,
    HAPI_StringHandle name_sh, HAPI_StringHandle label_sh, HAPI_StringHandle template_name_sh,
    HAPI_StringHandle help_sh, bool has_min, bool has_max, bool has_ui_min, bool has_ui_max,
    double min, double max, double ui_min, double ui_max,
    bool invisible, bool disabled, bool spare, bool join_next, bool label_none,
    int32_t int_values_index, int32_t float_values_index, int32_t string_values_index, int32_t choice_index,
    bool is_child_of_multiparm, int32_t instance_num, int32_t instance_length, int32_t instance_count,
    int32_t instance_start_offset, HAPI_RampType ramp_type)
{
    return enif_make_tuple(env, 35,
        hapi_private_make_atom(env, "hapi_parm_info"),
        enif_make_int(env, id),
        enif_make_int(env, parent_id),
        enif_make_int(env, type),
        enif_make_int(env, type_info_sh),
        enif_make_int(env, permissions),
        enif_make_int(env, size),
        enif_make_int(env, choice_count),
        enif_make_int(env, name_sh),
        enif_make_int(env, label_sh),
        enif_make_int(env, template_name_sh),
        enif_make_int(env, help_sh),
        enif_make_int(env, template_name_sh),
        enif_make_int(env, help_sh),
        hapi_private_make_atom_bool(env, has_min),
        hapi_private_make_atom_bool(env, has_max),
        hapi_private_make_atom_bool(env, has_ui_min),
        hapi_private_make_atom_bool(env, has_ui_max),
        enif_make_double(env, min),
        enif_make_double(env, max),
        enif_make_double(env, ui_min),
        enif_make_double(env, ui_max),
        hapi_private_make_atom_bool(env, invisible),
        hapi_private_make_atom_bool(env, disabled),
        hapi_private_make_atom_bool(env, spare),
        hapi_private_make_atom_bool(env, join_next),
        hapi_private_make_atom_bool(env, label_none),
        enif_make_int(env, int_values_index),
        enif_make_int(env, float_values_index),
        enif_make_int(env, string_values_index),
        enif_make_int(env, choice_index),
        hapi_private_make_atom_bool(env, is_child_of_multiparm),
        enif_make_int(env, instance_num),
        enif_make_int(env, instance_length),
        enif_make_int(env, instance_count),
        enif_make_int(env, instance_start_offset),
        enif_make_int(env, ramp_type));
}


ERL_NIF_TERM
hapi_private_make_hapi_parm_choice_info(ErlNifEnv* env, HAPI_ParmId parent_parm_id, HAPI_StringHandle label_sh,
    HAPI_StringHandle value_sh)
{
    return enif_make_tuple(env, 4,
        hapi_private_make_atom(env, "hapi_parm_choice_info"),
        enif_make_int(env, parent_parm_id),
        enif_make_int(env, label_sh),
        enif_make_int(env, value_sh));
}


ERL_NIF_TERM
hapi_private_make_hapi_handle_info(ErlNifEnv* env, HAPI_StringHandle name_sh, HAPI_StringHandle typeName_sh,
    int32_t bindings_count)
{
    return enif_make_tuple(env, 4,
        hapi_private_make_atom(env, "hapi_handle_info"),
        enif_make_int(env, name_sh),
        enif_make_int(env, typeName_sh),
        enif_make_int(env, bindings_count));
}


ERL_NIF_TERM
hapi_private_make_hapi_handle_binding_info(ErlNifEnv* env, HAPI_StringHandle handle_parm_name_sh,
    HAPI_StringHandle asset_parm_name_sh, HAPI_ParmId asset_parm_id)
{
    return enif_make_tuple(env, 4,
        hapi_private_make_atom(env, "hapi_handle_binding_info"),
        enif_make_int(env, handle_parm_name_sh),
        enif_make_int(env, asset_parm_name_sh),
        enif_make_int(env, asset_parm_id));
}


ERL_NIF_TERM
hapi_private_make_hapi_object_info(ErlNifEnv* env, HAPI_ObjectId id, HAPI_StringHandle name_sh,
    HAPI_StringHandle object_instance_path_sh, HAPI_Bool has_transform_changed, HAPI_Bool have_geos_changed,
    HAPI_Bool is_visible, HAPI_Bool is_instancer, int32_t geo_count, HAPI_NodeId node_id, HAPI_ObjectId object_to_instance_id)
{
    return enif_make_tuple(env, 11,
        hapi_private_make_atom(env, "hapi_object_info"),
        enif_make_int(env, id),
        enif_make_int(env, name_sh),
        enif_make_int(env, object_instance_path_sh),
        hapi_private_make_atom_bool(env, has_transform_changed),
        hapi_private_make_atom_bool(env, have_geos_changed),
        hapi_private_make_atom_bool(env, is_visible),
        hapi_private_make_atom_bool(env, is_instancer),
        enif_make_int(env, geo_count),
        enif_make_int(env, node_id),
        enif_make_int(env, object_to_instance_id));
}


ERL_NIF_TERM
hapi_private_make_hapi_transform(ErlNifEnv* env, const float* position, uint32_t position_size,
    const float* rotation_quaternion, uint32_t rotation_quaternion_size, const float* scale,
    uint32_t scale_size, HAPI_RSTOrder rst_order)
{
    return enif_make_tuple(env, 5,
        hapi_private_make_atom(env, "hapi_transform"),
        hapi_private_make_vector_float(env, position_size, position),
        hapi_private_make_vector_float(env, rotation_quaternion_size, rotation_quaternion),
        hapi_private_make_vector_float(env, scale_size, scale),
        hapi_enum_rst_order_c_to_erl(env, rst_order));
}


ERL_NIF_TERM
hapi_private_make_hapi_transform_euler(ErlNifEnv* env, const float* position, uint32_t position_size,
    const float* rotation_euler, uint32_t rotation_euler_size, const float* scale,
    uint32_t scale_size, HAPI_XYZOrder xyz_order, HAPI_RSTOrder rst_order)
{
    return enif_make_tuple(env, 6,
        hapi_private_make_atom(env, "hapi_transform_euler"),
        hapi_private_make_vector_float(env, position_size, position),
        hapi_private_make_vector_float(env, rotation_euler_size, rotation_euler),
        hapi_private_make_vector_float(env, scale_size, scale),
        hapi_enum_xyz_order_c_to_erl(env, xyz_order),
        hapi_enum_rst_order_c_to_erl(env, rst_order));
}


ERL_NIF_TERM
hapi_private_make_hapi_geo_info(ErlNifEnv* env, const HAPI_GeoInfo* geo_info)
{
    return enif_make_tuple(env, 13,
        hapi_private_make_atom(env, "hapi_geo_info"),
        enif_make_int(env, (int32_t) geo_info->id),
        enif_make_int(env, (int32_t) geo_info->type),
        enif_make_int(env, (int32_t) geo_info->nameSH),
        enif_make_int(env, (int32_t) geo_info->nodeId),
        hapi_private_make_atom_bool(env, (bool) geo_info->isEditable),
        hapi_private_make_atom_bool(env, (bool) geo_info->isTemplated),
        hapi_private_make_atom_bool(env, (bool) geo_info->isDisplayGeo),
        hapi_private_make_atom_bool(env, (bool) geo_info->hasGeoChanged),
        hapi_private_make_atom_bool(env, (bool) geo_info->hasMaterialChanged),
        enif_make_int(env, geo_info->pointGroupCount),
        enif_make_int(env, geo_info->primitiveGroupCount),
        enif_make_int(env, geo_info->partCount));
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
        !hapi_private_check_bool(env, tuple_cook_options[1], &cook_options_split_geos_by_group) ||
        !enif_get_int(env, tuple_cook_options[2], &cook_options_max_vertices_per_primitive) ||
        !hapi_private_check_bool(env, tuple_cook_options[3], &cook_options_refine_curve_to_linear) ||
        !enif_get_double(env, tuple_cook_options[4], &cook_options_curve_refine_lod) ||
        !hapi_private_check_bool(env, tuple_cook_options[5], &cook_options_clear_errors_and_warnings) ||
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


bool
hapi_private_get_hapi_handle_info(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_HandleInfo* handle_info)
{
    int32_t tuple_size;
    const ERL_NIF_TERM* tuple_handle_info = NULL;
    bool handle_info_record = false;

    HAPI_StringHandle handle_info_name_sh;
    HAPI_StringHandle handle_info_type_name_sh;
    int32_t bindings_count;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_handle_info) ||
        (tuple_size != 4) ||
        !hapi_private_check_atom_value(env, tuple_handle_info[0], "hapi_handle_info", &handle_info_record) ||
        !handle_info_record ||
        !enif_get_int(env, tuple_handle_info[1], &handle_info_name_sh) ||
        !enif_get_int(env, tuple_handle_info[2], &handle_info_type_name_sh) ||
        !enif_get_int(env, tuple_handle_info[3], &bindings_count))
    {
        return false;
    }

    handle_info->nameSH = handle_info_name_sh;
    handle_info->typeNameSH = handle_info_type_name_sh;
    handle_info->bindingsCount = bindings_count;

    return true;
}


bool
hapi_private_get_hapi_transform_euler(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TransformEuler* transform)
{
    int32_t tuple_size;
    const ERL_NIF_TERM* tuple_transform_euler = NULL;
    bool transform_euler_record = false;

    double transform_euler_position[HAPI_POSITION_VECTOR_SIZE];
    double transform_euler_rotation[HAPI_EULER_VECTOR_SIZE];
    double transform_euler_scale[HAPI_SCALE_VECTOR_SIZE];
    HAPI_XYZOrder transform_euler_rotation_order;
    HAPI_RSTOrder transform_euler_rst_order;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_transform_euler) ||
        (tuple_size != 6) ||
        !hapi_private_check_atom_value(env, tuple_transform_euler[0], "hapi_transform_euler", &transform_euler_record) ||
        !transform_euler_record ||
        !hapi_private_get_vector_double(env, tuple_transform_euler[1], HAPI_POSITION_VECTOR_SIZE, &transform_euler_position[0]) ||
        !hapi_private_get_vector_double(env, tuple_transform_euler[2], HAPI_EULER_VECTOR_SIZE, &transform_euler_rotation[0]) ||
        !hapi_private_get_vector_double(env, tuple_transform_euler[3], HAPI_SCALE_VECTOR_SIZE, &transform_euler_scale[0]) ||
        !hapi_enum_xyz_order_erl_to_c(env, tuple_transform_euler[4], &transform_euler_rotation_order) ||
        !hapi_enum_rst_order_erl_to_c(env, tuple_transform_euler[5], &transform_euler_rst_order))
    {
        return false;
    }

    for(int idx = 0; idx < HAPI_POSITION_VECTOR_SIZE; ++idx)
    {
        transform->position[idx] = (float) transform_euler_position[idx];
    }

    for(int idx = 0; idx < HAPI_EULER_VECTOR_SIZE; ++idx)
    {
        transform->rotationEuler[idx] = (float) transform_euler_rotation[idx];
    }

    for(int idx = 0; idx < HAPI_SCALE_VECTOR_SIZE; ++idx)
    {
        transform->scale[idx] = (float) transform_euler_scale[idx];
    }

    transform->rotationOrder = transform_euler_rotation_order;
    transform->rstOrder = transform_euler_rst_order;

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
hapi_private_get_hapi_parm_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ParmId* parm_id)
{
    return hapi_private_get_id_helper(env, term, (int32_t*) parm_id);
}


bool
hapi_private_get_hapi_object_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ObjectId* object_id)
{
    return hapi_private_get_id_helper(env, term, (int32_t*) object_id);
}


bool
hapi_private_get_hapi_geo_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_GeoId* geo_id)
{
    return hapi_private_get_id_helper(env, term, (int32_t*) geo_id);
}


bool
hapi_private_get_vector_double(ErlNifEnv* env, const ERL_NIF_TERM term, uint32_t size, double* data)
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


bool
hapi_private_get_vector_float(ErlNifEnv* env, const ERL_NIF_TERM term, uint32_t size, float* data)
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
                *(data + index) = (float) param_double;
            }
            else if(enif_get_int(env, head, &param_int))
            {
                *(data + index) = (float) param_int;
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


bool
hapi_private_get_vector_int(ErlNifEnv* env, const ERL_NIF_TERM term, uint32_t size, int32_t* data)
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
            int32_t param_int = 0.0;

            if(enif_get_int(env, head, &param_int))
            {
                *(data + index) = param_int;
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
