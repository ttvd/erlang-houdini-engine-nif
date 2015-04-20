/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL

#pragma once

#if !defined(HAPI_PRIVATE_NIF_H)
#define HAPI_PRIVATE_NIF_H

#include "erl_nif.h"
#include "xxhash.h"
#include "HAPI.h"


// Create an atom.
ERL_NIF_TERM hapi_private_make_atom(ErlNifEnv* env, const char* atom_name);

// Create a boolean atom.
ERL_NIF_TERM hapi_private_make_atom_bool(ErlNifEnv* env, bool value);

// Create an atom with hash.
ERL_NIF_TERM hapi_private_make_hash_tuple(ErlNifEnv* env, const char* atom_name);

// Create hapi_parm_info record.
ERL_NIF_TERM hapi_private_make_hapi_parm_info(ErlNifEnv* env, HAPI_ParmId id, HAPI_ParmId parent_id, HAPI_ParmType type,
    HAPI_StringHandle type_info_sh, HAPI_Permissions permissions, int32_t size, int32_t choice_count,
    HAPI_StringHandle name_sh, HAPI_StringHandle label_sh, HAPI_StringHandle template_name_sh, HAPI_StringHandle help_sh,
    bool has_min, bool has_max, bool has_ui_min, bool has_ui_max, double min, double max, double ui_min, double ui_max,
    bool invisible, bool disabled, bool spare, bool join_next, bool label_none,
    int32_t int_values_index, int32_t float_values_index, int32_t string_values_index, int32_t choice_index,
    bool is_child_of_multiparm, int32_t instance_num, int32_t instance_length, int32_t instance_count,
    int32_t instance_start_offset, HAPI_RampType ramp_type);

// Create hapi_parm_choice_info record.
ERL_NIF_TERM hapi_private_make_hapi_parm_choice_info(ErlNifEnv* env, HAPI_ParmId parent_parm_id, HAPI_StringHandle label_sh,
    HAPI_StringHandle value_sh);

// Create hapi_handle_info record.
ERL_NIF_TERM hapi_private_make_hapi_handle_info(ErlNifEnv* env, HAPI_StringHandle name_sh, HAPI_StringHandle typeName_sh,
    int32_t bindings_count);

// Create hapi_handle_binding_info record.
ERL_NIF_TERM hapi_private_make_hapi_handle_binding_info(ErlNifEnv* env, HAPI_StringHandle handle_parm_name_sh,
    HAPI_StringHandle asset_parm_name_sh, HAPI_ParmId asset_parm_id);

// Create hapi_object_info record.
ERL_NIF_TERM hapi_private_make_hapi_object_info(ErlNifEnv* env, const HAPI_ObjectInfo* object_info);

// Create hapi_transform record.
ERL_NIF_TERM hapi_private_make_hapi_transform(ErlNifEnv* env, const float* position, uint32_t position_size,
    const float* rotation_quaternion, uint32_t rotation_quaternion_size, const float* scale,
    uint32_t scale_size, HAPI_RSTOrder rst_order);

// Create hapi_transform_euler record.
ERL_NIF_TERM hapi_private_make_hapi_transform_euler(ErlNifEnv* env, const float* position, uint32_t position_size,
    const float* rotation_euler, uint32_t rotation_euler_size, const float* scale,
    uint32_t scale_size, HAPI_XYZOrder xyz_order, HAPI_RSTOrder rst_order);

// Create hapi_geo_info record.
ERL_NIF_TERM hapi_private_make_hapi_geo_info(ErlNifEnv* env, const HAPI_GeoInfo* geo_info);

// Create a vector of doubles (list) from float array of given size.
ERL_NIF_TERM hapi_private_make_vector_float(ErlNifEnv* env, uint32_t size, const float* data);

// Create a result / int tuple if hapi call was successful, otherwise return hapi result atom.
ERL_NIF_TERM hapi_private_make_result_tuple_int(ErlNifEnv* env, HAPI_Result result, int32_t value);

// Create a result / double tuple if hapi call was successful, otherwise return hapi result atom.
ERL_NIF_TERM hapi_private_make_result_tuple_double(ErlNifEnv* env, HAPI_Result result, double value);

// Create a result / string tuple if hapi call was successful, otherwise return hapi result atom.
ERL_NIF_TERM hapi_private_make_result_tuple_string(ErlNifEnv* env, HAPI_Result result, const char* value);

// Create a result / bool tuple if hapi call was successful, otherwise return hapi result atom.
ERL_NIF_TERM hapi_private_make_result_tuple_bool(ErlNifEnv* env, HAPI_Result result, bool value);

// Return true or false by pointer if given atom has a specified value.
bool hapi_private_check_atom_value(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status);

// Return true or false by pointer if atom is nil.
bool hapi_private_check_nil(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status);

// Return boolean value by pointer.
bool hapi_private_check_bool(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status);

// Return string and length by pointer, caller is responsible for clean up.
bool hapi_private_get_string(ErlNifEnv* env, const ERL_NIF_TERM term, char** string, uint32_t* string_length);

// Return HAPI_CookOptions by pointer.
bool hapi_private_get_hapi_cook_options(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CookOptions* cook_options);

// Return HAPI_HandleInfo by pointer.
bool hapi_private_get_hapi_handle_info(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_HandleInfo* handle_info);

// Return HAPI_TransformEuler by pointer.
bool hapi_private_get_hapi_transform_euler(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TransformEuler* transform);

// Return HAPI_AssetId by pointer.
bool hapi_private_get_hapi_asset_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetId* asset_id);

// Return HAPI_AssetLibraryId by pointer.
bool hapi_private_get_hapi_asset_library_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetLibraryId* asset_library_id);

// Return HAPI_NodeId by pointer.
bool hapi_private_get_hapi_node_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_NodeId* node_id);

// Return HAPI_ParmId by pointer.
bool hapi_private_get_hapi_parm_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ParmId* parm_id);

// Return HAPI_ObjectId by pointer.
bool hapi_private_get_hapi_object_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ObjectId* object_id);

// Return HAPI_GeoId by pointer.
bool hapi_private_get_hapi_geo_id(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_GeoId* geo_id);

// Return a double vector by pointer.
bool hapi_private_get_vector_double(ErlNifEnv* env, const ERL_NIF_TERM term, uint32_t size, double* data);

// Return a float vector by pointer.
bool hapi_private_get_vector_float(ErlNifEnv* env, const ERL_NIF_TERM term, uint32_t size, float* data);

// Return an int vector by pointer.
bool hapi_private_get_vector_int(ErlNifEnv* env, const ERL_NIF_TERM term, uint32_t size, int32_t* data);


#endif //!defined(HAPI_PRIVATE_NIF_H)
