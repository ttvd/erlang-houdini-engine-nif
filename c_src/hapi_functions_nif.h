/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_functions_nif.h.template

#pragma once

#if !defined(HAPI_FUNCTIONS_NIF_H)
#define HAPI_FUNCTIONS_NIF_H

#include "erl_nif.h"
#include "HAPI.h"


extern ERL_NIF_TERM hapi_timeline_options_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_string_buf_length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_is_float(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_preset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_attribute_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_handle_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_part_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_attribute_string_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_cooking_current_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_float_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_geo_info_get_group_count_by_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_curve_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_cleanup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_env_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_status_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_is_non_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_get_float_value_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_face_counts(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_node_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_handle_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_node_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_is_asset_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_instance_transforms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_attribute_names(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_choice_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_global_nodes_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_volume_tile_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_attribute_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_load_asset_library_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_geo_input_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_asset_transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_curve_counts(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_parm_float_values(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_part_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_geo_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_save_hipfile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_material_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_attribute_int_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_part_info_get_element_count_by_attribute_owner(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_asset_transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_insert_multiparm_instance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_preset_buf_length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_extract_image_to_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_disconnect_asset_geometry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_image_file_format_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_part_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_vertex_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_attribute_string_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_object_transforms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_transform_anim_curve(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_geo_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_image_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_is_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_attribute_int_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_group_membership(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_info_from_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_face_counts(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_curve_counts(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_attribute_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_load_asset_library_from_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_new_asset_ids(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_object_transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_handle_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_objects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_volume_tile_float_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_image_file_format_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_asset_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_render_material_to_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_part_info_get_attribute_count_by_owner(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_connect_asset_transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_volume_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_global_nodes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_handle_binding_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_material_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_volume_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_timeline_options_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_global_nodes_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_image_planes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_handle_binding_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_parm_string_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_parm_int_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_cooking_total_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_render_texture_to_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_image_plane_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_convert_matrix_to_euler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_choice_lists(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_part_info_get_element_count_by_group_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_remove_multiparm_instance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_vertex_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_available_asset_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_next_volume_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_image_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_node_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_get_string_value_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_destroy_asset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_input_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_int_values(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_parm_int_values(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_cook_options_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_string_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_anim_curve(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_curve_knots(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_reset_simulation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_preset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_keyframe_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_interrupt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_image_memory_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_convert_matrix_to_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parameters(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_convert_transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_create_curve(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_is_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_convert_transform_quat_to_matrix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_get_int_value_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_volume_tile_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_save_geo_to_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_instantiate_asset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_supported_image_file_format_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_volume_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_save_geo_to_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_curve_orders(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_asset_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_curve_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_is_initialized(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_disconnect_asset_transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_curve_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_part_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_float_values(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_handle_binding_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_volume_tile_int_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_object_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_timeline_options(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_commit_geo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_choice_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_image_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_cook_asset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_load_hipfile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_geo_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_volume_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_int_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_add_attribute(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_add_group(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_convert_transform_euler_to_matrix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_attribute_float_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_id_from_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_group_membership(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_volume_tile_int_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_status(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_material_ids_on_faces(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_geo_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_curve_orders(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_status_string_buf_length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_curve_knots(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_first_volume_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_group_names(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_load_geo_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_revert_geo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_asset_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_is_path(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_create_input_asset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_parm_float_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_load_geo_from_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_is_file_path(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_keyframe_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_geo_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_geo_input_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_material_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_available_assets(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_material_on_part(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_curve_info_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_parm_info_is_node_path(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_cook_options_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_extract_image_to_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_image_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_parm_string_values(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_material_on_group(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_python_thread_interpreter_lock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_set_timeline_options(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_volume_tile_float_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_attribute_float_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_connect_asset_geometry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_object_info_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_get_supported_image_file_formats(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM hapi_check_for_new_assets(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


#endif //!defined(HAPI_FUNCTIONS_NIF_H)
