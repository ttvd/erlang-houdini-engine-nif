/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from hapi_exports_nif.c.template

#include "hapi_functions_nif.h"


ErlNifFunc
nif_funcs[] =
{
    {"timeline_options_create", 0, hapi_timeline_options_create},
    {"get_string_buf_length", 1, hapi_get_string_buf_length},
    {"parm_info_is_float", 1, hapi_parm_info_is_float},
    {"get_preset", 2, hapi_get_preset},
    {"attribute_info_init", 0, hapi_attribute_info_init},
    {"handle_info_init", 0, hapi_handle_info_init},
    {"part_info_create", 0, hapi_part_info_create},
    {"get_attribute_string_data", 7, hapi_get_attribute_string_data},
    {"get_cooking_current_count", 0, hapi_get_cooking_current_count},
    {"get_parm_float_value", 3, hapi_get_parm_float_value},
    {"geo_info_get_group_count_by_type", 1, hapi_geo_info_get_group_count_by_type},
    {"curve_info_create", 0, hapi_curve_info_create},
    {"cleanup", 0, hapi_cleanup},
    {"get_env_int", 1, hapi_get_env_int},
    {"get_status_string", 1, hapi_get_status_string},
    {"parm_info_is_non_value", 1, hapi_parm_info_is_non_value},
    {"parm_info_get_float_value_count", 1, hapi_parm_info_get_float_value_count},
    {"set_face_counts", 6, hapi_set_face_counts},
    {"node_info_create", 0, hapi_node_info_create},
    {"get_handle_info", 3, hapi_get_handle_info},
    {"get_node_info", 1, hapi_get_node_info},
    {"is_asset_valid", 2, hapi_is_asset_valid},
    {"get_instance_transforms", 6, hapi_get_instance_transforms},
    {"get_attribute_names", 6, hapi_get_attribute_names},
    {"parm_choice_info_create", 0, hapi_parm_choice_info_create},
    {"global_nodes_init", 0, hapi_global_nodes_init},
    {"volume_tile_info_create", 0, hapi_volume_tile_info_create},
    {"get_attribute_info", 6, hapi_get_attribute_info},
    {"load_asset_library_from_file", 2, hapi_load_asset_library_from_file},
    {"geo_input_info_init", 0, hapi_geo_input_info_init},
    {"set_asset_transform", 1, hapi_set_asset_transform},
    {"get_curve_counts", 6, hapi_get_curve_counts},
    {"set_parm_float_values", 4, hapi_set_parm_float_values},
    {"get_part_info", 4, hapi_get_part_info},
    {"geo_info_create", 0, hapi_geo_info_create},
    {"save_hipfile", 1, hapi_save_hipfile},
    {"material_info_create", 0, hapi_material_info_create},
    {"set_attribute_int_data", 8, hapi_set_attribute_int_data},
    {"part_info_get_element_count_by_attribute_owner", 1, hapi_part_info_get_element_count_by_attribute_owner},
    {"get_asset_transform", 3, hapi_get_asset_transform},
    {"insert_multiparm_instance", 3, hapi_insert_multiparm_instance},
    {"get_preset_buf_length", 3, hapi_get_preset_buf_length},
    {"extract_image_to_memory", 4, hapi_extract_image_to_memory},
    {"disconnect_asset_geometry", 2, hapi_disconnect_asset_geometry},
    {"image_file_format_create", 0, hapi_image_file_format_create},
    {"set_part_info", 4, hapi_set_part_info},
    {"set_vertex_list", 6, hapi_set_vertex_list},
    {"set_attribute_string_data", 8, hapi_set_attribute_string_data},
    {"get_object_transforms", 4, hapi_get_object_transforms},
    {"set_transform_anim_curve", 4, hapi_set_transform_anim_curve},
    {"get_geo_info", 3, hapi_get_geo_info},
    {"parm_info_create", 0, hapi_parm_info_create},
    {"image_info_init", 0, hapi_image_info_init},
    {"parm_info_is_string", 1, hapi_parm_info_is_string},
    {"get_attribute_int_data", 7, hapi_get_attribute_int_data},
    {"get_group_membership", 8, hapi_get_group_membership},
    {"get_time", 0, hapi_get_time},
    {"get_parm_info_from_name", 2, hapi_get_parm_info_from_name},
    {"get_face_counts", 6, hapi_get_face_counts},
    {"set_curve_counts", 7, hapi_set_curve_counts},
    {"attribute_info_create", 0, hapi_attribute_info_create},
    {"load_asset_library_from_memory", 3, hapi_load_asset_library_from_memory},
    {"get_new_asset_ids", 0, hapi_get_new_asset_ids},
    {"set_object_transform", 3, hapi_set_object_transform},
    {"handle_info_create", 0, hapi_handle_info_create},
    {"get_objects", 3, hapi_get_objects},
    {"set_volume_tile_float_data", 5, hapi_set_volume_tile_float_data},
    {"image_file_format_init", 0, hapi_image_file_format_init},
    {"get_asset_info", 1, hapi_get_asset_info},
    {"render_material_to_image", 3, hapi_render_material_to_image},
    {"part_info_get_attribute_count_by_owner", 1, hapi_part_info_get_attribute_count_by_owner},
    {"connect_asset_transform", 3, hapi_connect_asset_transform},
    {"volume_info_create", 0, hapi_volume_info_create},
    {"get_global_nodes", 0, hapi_get_global_nodes},
    {"get_handle_binding_info", 4, hapi_get_handle_binding_info},
    {"get_material_info", 2, hapi_get_material_info},
    {"get_volume_info", 4, hapi_get_volume_info},
    {"timeline_options_init", 0, hapi_timeline_options_init},
    {"global_nodes_create", 0, hapi_global_nodes_create},
    {"get_image_planes", 3, hapi_get_image_planes},
    {"handle_binding_info_create", 0, hapi_handle_binding_info_create},
    {"set_parm_string_value", 4, hapi_set_parm_string_value},
    {"set_parm_int_value", 4, hapi_set_parm_int_value},
    {"get_cooking_total_count", 0, hapi_get_cooking_total_count},
    {"render_texture_to_image", 3, hapi_render_texture_to_image},
    {"get_string", 2, hapi_get_string},
    {"get_image_plane_count", 2, hapi_get_image_plane_count},
    {"convert_matrix_to_euler", 2, hapi_convert_matrix_to_euler},
    {"get_parm_choice_lists", 3, hapi_get_parm_choice_lists},
    {"part_info_get_element_count_by_group_type", 1, hapi_part_info_get_element_count_by_group_type},
    {"remove_multiparm_instance", 3, hapi_remove_multiparm_instance},
    {"get_vertex_list", 6, hapi_get_vertex_list},
    {"get_available_asset_count", 1, hapi_get_available_asset_count},
    {"get_next_volume_tile", 4, hapi_get_next_volume_tile},
    {"set_image_info", 3, hapi_set_image_info},
    {"node_info_init", 0, hapi_node_info_init},
    {"parm_info_get_string_value_count", 1, hapi_parm_info_get_string_value_count},
    {"destroy_asset", 1, hapi_destroy_asset},
    {"get_input_name", 3, hapi_get_input_name},
    {"get_parm_int_values", 3, hapi_get_parm_int_values},
    {"set_parm_int_values", 4, hapi_set_parm_int_values},
    {"cook_options_init", 0, hapi_cook_options_init},
    {"get_parm_string_value", 4, hapi_get_parm_string_value},
    {"set_anim_curve", 5, hapi_set_anim_curve},
    {"get_parm_info", 2, hapi_get_parm_info},
    {"set_curve_knots", 7, hapi_set_curve_knots},
    {"reset_simulation", 1, hapi_reset_simulation},
    {"set_preset", 5, hapi_set_preset},
    {"keyframe_init", 0, hapi_keyframe_init},
    {"interrupt", 0, hapi_interrupt},
    {"get_image_memory_buffer", 3, hapi_get_image_memory_buffer},
    {"convert_matrix_to_quat", 1, hapi_convert_matrix_to_quat},
    {"get_parameters", 3, hapi_get_parameters},
    {"convert_transform", 2, hapi_convert_transform},
    {"create_curve", 0, hapi_create_curve},
    {"parm_info_is_int", 1, hapi_parm_info_is_int},
    {"convert_transform_quat_to_matrix", 1, hapi_convert_transform_quat_to_matrix},
    {"parm_info_get_int_value_count", 1, hapi_parm_info_get_int_value_count},
    {"volume_tile_info_init", 0, hapi_volume_tile_info_init},
    {"save_geo_to_memory", 4, hapi_save_geo_to_memory},
    {"instantiate_asset", 2, hapi_instantiate_asset},
    {"get_supported_image_file_format_count", 0, hapi_get_supported_image_file_format_count},
    {"set_volume_info", 4, hapi_set_volume_info},
    {"save_geo_to_file", 4, hapi_save_geo_to_file},
    {"get_curve_orders", 6, hapi_get_curve_orders},
    {"asset_info_create", 0, hapi_asset_info_create},
    {"get_curve_info", 4, hapi_get_curve_info},
    {"is_initialized", 0, hapi_is_initialized},
    {"disconnect_asset_transform", 2, hapi_disconnect_asset_transform},
    {"set_curve_info", 5, hapi_set_curve_info},
    {"initialize", 5, hapi_initialize},
    {"part_info_init", 0, hapi_part_info_init},
    {"get_parm_float_values", 3, hapi_get_parm_float_values},
    {"handle_binding_info_init", 0, hapi_handle_binding_info_init},
    {"get_volume_tile_int_data", 4, hapi_get_volume_tile_int_data},
    {"object_info_init", 0, hapi_object_info_init},
    {"get_timeline_options", 0, hapi_get_timeline_options},
    {"commit_geo", 3, hapi_commit_geo},
    {"parm_choice_info_init", 0, hapi_parm_choice_info_init},
    {"image_info_create", 0, hapi_image_info_create},
    {"cook_asset", 2, hapi_cook_asset},
    {"load_hipfile", 2, hapi_load_hipfile},
    {"geo_info_init", 0, hapi_geo_info_init},
    {"volume_info_init", 0, hapi_volume_info_init},
    {"get_parm_int_value", 3, hapi_get_parm_int_value},
    {"add_attribute", 5, hapi_add_attribute},
    {"add_group", 5, hapi_add_group},
    {"convert_transform_euler_to_matrix", 1, hapi_convert_transform_euler_to_matrix},
    {"set_attribute_float_data", 8, hapi_set_attribute_float_data},
    {"get_parm_id_from_name", 2, hapi_get_parm_id_from_name},
    {"set_group_membership", 7, hapi_set_group_membership},
    {"set_volume_tile_int_data", 5, hapi_set_volume_tile_int_data},
    {"get_status", 1, hapi_get_status},
    {"get_material_ids_on_faces", 6, hapi_get_material_ids_on_faces},
    {"get_geo_size", 4, hapi_get_geo_size},
    {"set_curve_orders", 7, hapi_set_curve_orders},
    {"get_status_string_buf_length", 2, hapi_get_status_string_buf_length},
    {"get_curve_knots", 6, hapi_get_curve_knots},
    {"get_first_volume_tile", 4, hapi_get_first_volume_tile},
    {"get_group_names", 5, hapi_get_group_names},
    {"load_geo_from_file", 4, hapi_load_geo_from_file},
    {"revert_geo", 3, hapi_revert_geo},
    {"asset_info_init", 0, hapi_asset_info_init},
    {"parm_info_is_path", 1, hapi_parm_info_is_path},
    {"create_input_asset", 1, hapi_create_input_asset},
    {"set_parm_float_value", 4, hapi_set_parm_float_value},
    {"load_geo_from_memory", 5, hapi_load_geo_from_memory},
    {"parm_info_is_file_path", 1, hapi_parm_info_is_file_path},
    {"keyframe_create", 0, hapi_keyframe_create},
    {"set_geo_info", 3, hapi_set_geo_info},
    {"geo_input_info_create", 0, hapi_geo_input_info_create},
    {"material_info_init", 0, hapi_material_info_init},
    {"parm_info_init", 0, hapi_parm_info_init},
    {"get_available_assets", 2, hapi_get_available_assets},
    {"get_material_on_part", 4, hapi_get_material_on_part},
    {"curve_info_init", 0, hapi_curve_info_init},
    {"parm_info_is_node_path", 1, hapi_parm_info_is_node_path},
    {"cook_options_create", 0, hapi_cook_options_create},
    {"extract_image_to_file", 6, hapi_extract_image_to_file},
    {"set_time", 1, hapi_set_time},
    {"get_image_info", 2, hapi_get_image_info},
    {"get_parm_string_values", 4, hapi_get_parm_string_values},
    {"get_material_on_group", 4, hapi_get_material_on_group},
    {"python_thread_interpreter_lock", 1, hapi_python_thread_interpreter_lock},
    {"set_timeline_options", 1, hapi_set_timeline_options},
    {"get_volume_tile_float_data", 4, hapi_get_volume_tile_float_data},
    {"get_attribute_float_data", 7, hapi_get_attribute_float_data},
    {"connect_asset_geometry", 4, hapi_connect_asset_geometry},
    {"object_info_create", 0, hapi_object_info_create},
    {"get_supported_image_file_formats", 1, hapi_get_supported_image_file_formats},
    {"check_for_new_assets", 0, hapi_check_for_new_assets}
};


ERL_NIF_INIT(hapi, nif_funcs, NULL, NULL, NULL, NULL)