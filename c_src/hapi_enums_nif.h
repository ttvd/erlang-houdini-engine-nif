#pragma once

#if !defined(HAPI_ENUMS_NIF_H)
#define HAPI_ENUMS_NIF_H

#include "erl_nif.h"
#include "HAPI.h"

#include <string.h>

// From hapi_enum_license_nif.c
bool hapi_enum_license_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_License* license);
ERL_NIF_TERM hapi_enum_license_c_to_erl(ErlNifEnv* env, HAPI_License license);

// From hapi_enum_status_type_nif.c
bool hapi_enum_status_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StatusType* status_type);
ERL_NIF_TERM hapi_enum_status_type_c_to_erl(ErlNifEnv* env, HAPI_StatusType status_type);

// From hapi_enum_status_verbosity_nif.c
bool hapi_enum_status_verbosity_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StatusVerbosity* status_verbosity);
ERL_NIF_TERM hapi_enum_status_verbosity_c_to_erl(ErlNifEnv* env, HAPI_StatusVerbosity status_verbosity);

// From hapi_enum_result_nif.c
bool hapi_enum_result_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Result* result);
ERL_NIF_TERM hapi_enum_result_c_to_erl(ErlNifEnv* env, HAPI_Result result);

// From hapi_enum_state_nif.c
bool hapi_enum_state_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_State* state);
ERL_NIF_TERM hapi_enum_state_c_to_erl(ErlNifEnv* env, HAPI_State state);

// From hapi_enum_permissions_nif.c
bool hapi_enum_permissions_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Permissions* permissions);
ERL_NIF_TERM hapi_enum_permissions_c_to_erl(ErlNifEnv* env, HAPI_Permissions permissions);

// From hapi_enum_ramp_type_nif.c
bool hapi_enum_ramp_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RampType* ramp_type);
ERL_NIF_TERM hapi_enum_ramp_type_c_to_erl(ErlNifEnv* env, HAPI_RampType ramp_type);

// From hapi_enum_parm_type_nif.c
bool hapi_enum_parm_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ParmType* parm_type);
ERL_NIF_TERM hapi_enum_parm_type_c_to_erl(ErlNifEnv* env, HAPI_ParmType parm_type);

// From hapi_enum_preset_type_nif.c
bool hapi_enum_preset_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_PresetType* preset_type);
ERL_NIF_TERM hapi_enum_preset_type_c_to_erl(ErlNifEnv* env, HAPI_PresetType preset_type);

// From hapi_enum_asset_type_nif.c
bool hapi_enum_asset_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetType* asset_type);
ERL_NIF_TERM hapi_enum_asset_type_c_to_erl(ErlNifEnv* env, HAPI_AssetType asset_type);

// From hapi_enum_asset_sub_type_nif.c
bool hapi_enum_asset_sub_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetSubType* asset_sub_type);
ERL_NIF_TERM hapi_enum_asset_sub_type_c_to_erl(ErlNifEnv* env, HAPI_AssetSubType asset_sub_type);

// From hapi_enum_group_type_nif.c
bool hapi_enum_group_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_GroupType* group_type);
ERL_NIF_TERM hapi_enum_group_type_c_to_erl(ErlNifEnv* env, HAPI_GroupType group_type);

// From hapi_enum_attribute_owner_nif.c
bool hapi_enum_attribute_owner_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AttributeOwner* attribute_owner);
ERL_NIF_TERM hapi_enum_attribute_owner_c_to_erl(ErlNifEnv* env, HAPI_AttributeOwner attribute_owner);

// From hapi_enum_curve_type_nif.c
bool hapi_enum_curve_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveType* curve_type);
ERL_NIF_TERM hapi_enum_curve_type_c_to_erl(ErlNifEnv* env, HAPI_CurveType curve_type);

// From hapi_enum_storage_type_nif.c
bool hapi_enum_storage_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StorageType* storage_type);
ERL_NIF_TERM hapi_enum_storage_type_c_to_erl(ErlNifEnv* env, HAPI_StorageType storage_type);

// From hapi_enum_geo_type_nif.c
bool hapi_enum_geo_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_GeoType* geo_type);
ERL_NIF_TERM hapi_enum_geo_type_c_to_erl(ErlNifEnv* env, HAPI_GeoType geo_type);

// From hapi_enum_input_type_nif.c
bool hapi_enum_input_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_InputType* input_type);
ERL_NIF_TERM hapi_enum_input_type_c_to_erl(ErlNifEnv* env, HAPI_InputType input_type);

// From hapi_enum_curve_orders_nif.c
bool hapi_enum_curve_orders_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveOrders* curve_orders);
ERL_NIF_TERM hapi_enum_curve_orders_c_to_erl(ErlNifEnv* env, HAPI_CurveOrders curve_orders);

// From hapi_enum_transform_component_nif.c
bool hapi_enum_transform_component_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TransformComponent* transform_component);
ERL_NIF_TERM hapi_enum_transform_component_c_to_erl(ErlNifEnv* env, HAPI_TransformComponent transform_component);

// From hapi_enum_rst_order_nif.c
bool hapi_enum_rst_order_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RSTOrder* rst_order);
ERL_NIF_TERM hapi_enum_rst_order_c_to_erl(ErlNifEnv* env, HAPI_RSTOrder rst_order);

// From hapi_enum_xyz_order_nif.c
bool hapi_enum_xyz_order_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_XYZOrder* xyz_order);
ERL_NIF_TERM hapi_enum_xyz_order_c_to_erl(ErlNifEnv* env, HAPI_XYZOrder xyz_order);

// From hapi_enum_shader_type_nif.c
bool hapi_enum_shader_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ShaderType* shader_type);
ERL_NIF_TERM hapi_enum_shader_type_c_to_erl(ErlNifEnv* env, HAPI_ShaderType shader_type);

// From hapi_enum_image_data_format_nif.c
bool hapi_enum_image_data_format_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImageDataFormat* image_data_format);
ERL_NIF_TERM hapi_enum_image_data_format_c_to_erl(ErlNifEnv* env, HAPI_ImageDataFormat image_data_format);

// From hapi_enum_image_packing_nif.c
bool hapi_enum_image_packing_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImagePacking* image_packing);
ERL_NIF_TERM hapi_enum_image_packing_c_to_erl(ErlNifEnv* env, HAPI_ImagePacking image_packing);

// From hapi_enum_env_int_type_nif.c
bool hapi_enum_env_int_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_EnvIntType* env_int_type);
ERL_NIF_TERM hapi_enum_env_int_type_c_to_erl(ErlNifEnv* env, HAPI_EnvIntType env_int_type);


#endif //!defined(HAPI_ENUMS_NIF_H)
