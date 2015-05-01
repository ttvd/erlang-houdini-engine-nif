%%% @author Mykola Konyk <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% @license MS-RL
%%% This file has been auto-generated from hapi.erl.template

-module(hapi).
-version(1.9).

-on_load(init/0).
-define(nif_stub, nif_stub_error(?LINE)).

% Generated records.
-include("hapi_records.hrl").

% Generated enum types and translation functions.
%-include("hapi_enums.hrl").

% Generated exports.
-export([
    hapi_timeline_options_create/0,
    hapi_get_string_buf_length/1,
    hapi_parm_info_is_float/1,
    hapi_get_preset/2,
    hapi_attribute_info_init/0,
    hapi_handle_info_init/0,
    hapi_part_info_create/0,
    hapi_get_attribute_string_data/7,
    hapi_get_cooking_current_count/0,
    hapi_get_parm_float_value/3,
    hapi_geo_info_get_group_count_by_type/1,
    hapi_curve_info_create/0,
    hapi_cleanup/0,
    hapi_get_env_int/1,
    hapi_get_status_string/1,
    hapi_parm_info_is_non_value/1,
    hapi_parm_info_get_float_value_count/1,
    hapi_set_face_counts/6,
    hapi_node_info_create/0,
    hapi_get_handle_info/3,
    hapi_get_node_info/1,
    hapi_is_asset_valid/2,
    hapi_get_instance_transforms/6,
    hapi_get_attribute_names/6,
    hapi_parm_choice_info_create/0,
    hapi_global_nodes_init/0,
    hapi_volume_tile_info_create/0,
    hapi_get_attribute_info/6,
    hapi_load_asset_library_from_file/2,
    hapi_geo_input_info_init/0,
    hapi_set_asset_transform/1,
    hapi_get_curve_counts/6,
    hapi_set_parm_float_values/4,
    hapi_get_part_info/4,
    hapi_geo_info_create/0,
    hapi_save_hipfile/1,
    hapi_material_info_create/0,
    hapi_set_attribute_int_data/8,
    hapi_part_info_get_element_count_by_attribute_owner/1,
    hapi_get_asset_transform/3,
    hapi_insert_multiparm_instance/3,
    hapi_get_preset_buf_length/3,
    hapi_extract_image_to_memory/4,
    hapi_disconnect_asset_geometry/2,
    hapi_image_file_format_create/0,
    hapi_set_part_info/4,
    hapi_set_vertex_list/6,
    hapi_set_attribute_string_data/8,
    hapi_get_object_transforms/4,
    hapi_set_transform_anim_curve/4,
    hapi_get_geo_info/3,
    hapi_parm_info_create/0,
    hapi_image_info_init/0,
    hapi_parm_info_is_string/1,
    hapi_get_attribute_int_data/7,
    hapi_get_group_membership/8,
    hapi_get_time/0,
    hapi_get_parm_info_from_name/2,
    hapi_get_face_counts/6,
    hapi_set_curve_counts/7,
    hapi_attribute_info_create/0,
    hapi_load_asset_library_from_memory/3,
    hapi_get_new_asset_ids/0,
    hapi_set_object_transform/3,
    hapi_handle_info_create/0,
    hapi_get_objects/3,
    hapi_set_volume_tile_float_data/5,
    hapi_image_file_format_init/0,
    hapi_get_asset_info/1,
    hapi_render_material_to_image/3,
    hapi_part_info_get_attribute_count_by_owner/1,
    hapi_connect_asset_transform/3,
    hapi_volume_info_create/0,
    hapi_get_global_nodes/0,
    hapi_get_handle_binding_info/4,
    hapi_get_material_info/2,
    hapi_get_volume_info/4,
    hapi_timeline_options_init/0,
    hapi_global_nodes_create/0,
    hapi_get_image_planes/3,
    hapi_handle_binding_info_create/0,
    hapi_set_parm_string_value/4,
    hapi_set_parm_int_value/4,
    hapi_get_cooking_total_count/0,
    hapi_render_texture_to_image/3,
    hapi_get_string/2,
    hapi_get_image_plane_count/2,
    hapi_convert_matrix_to_euler/2,
    hapi_get_parm_choice_lists/3,
    hapi_part_info_get_element_count_by_group_type/1,
    hapi_remove_multiparm_instance/3,
    hapi_get_vertex_list/6,
    hapi_get_available_asset_count/1,
    hapi_get_next_volume_tile/4,
    hapi_set_image_info/3,
    hapi_node_info_init/0,
    hapi_parm_info_get_string_value_count/1,
    hapi_destroy_asset/1,
    hapi_get_input_name/3,
    hapi_get_parm_int_values/3,
    hapi_set_parm_int_values/4,
    hapi_cook_options_init/0,
    hapi_get_parm_string_value/4,
    hapi_set_anim_curve/5,
    hapi_get_parm_info/2,
    hapi_set_curve_knots/7,
    hapi_reset_simulation/1,
    hapi_set_preset/5,
    hapi_keyframe_init/0,
    hapi_interrupt/0,
    hapi_get_image_memory_buffer/3,
    hapi_convert_matrix_to_quat/1,
    hapi_get_parameters/3,
    hapi_convert_transform/2,
    hapi_create_curve/0,
    hapi_parm_info_is_int/1,
    hapi_convert_transform_quat_to_matrix/1,
    hapi_parm_info_get_int_value_count/1,
    hapi_volume_tile_info_init/0,
    hapi_save_geo_to_memory/4,
    hapi_instantiate_asset/2,
    hapi_get_supported_image_file_format_count/0,
    hapi_set_volume_info/4,
    hapi_save_geo_to_file/4,
    hapi_get_curve_orders/6,
    hapi_asset_info_create/0,
    hapi_get_curve_info/4,
    hapi_is_initialized/0,
    hapi_disconnect_asset_transform/2,
    hapi_set_curve_info/5,
    hapi_initialize/5,
    hapi_part_info_init/0,
    hapi_get_parm_float_values/3,
    hapi_handle_binding_info_init/0,
    hapi_get_volume_tile_int_data/4,
    hapi_object_info_init/0,
    hapi_get_timeline_options/0,
    hapi_commit_geo/3,
    hapi_parm_choice_info_init/0,
    hapi_image_info_create/0,
    hapi_cook_asset/2,
    hapi_load_hipfile/2,
    hapi_geo_info_init/0,
    hapi_volume_info_init/0,
    hapi_get_parm_int_value/3,
    hapi_add_attribute/5,
    hapi_add_group/5,
    hapi_convert_transform_euler_to_matrix/1,
    hapi_set_attribute_float_data/8,
    hapi_get_parm_id_from_name/2,
    hapi_set_group_membership/7,
    hapi_set_volume_tile_int_data/5,
    hapi_get_status/1,
    hapi_get_material_ids_on_faces/6,
    hapi_get_geo_size/4,
    hapi_set_curve_orders/7,
    hapi_get_status_string_buf_length/2,
    hapi_get_curve_knots/6,
    hapi_get_first_volume_tile/4,
    hapi_get_group_names/5,
    hapi_load_geo_from_file/4,
    hapi_revert_geo/3,
    hapi_asset_info_init/0,
    hapi_parm_info_is_path/1,
    hapi_create_input_asset/1,
    hapi_set_parm_float_value/4,
    hapi_load_geo_from_memory/5,
    hapi_parm_info_is_file_path/1,
    hapi_keyframe_create/0,
    hapi_set_geo_info/3,
    hapi_geo_input_info_create/0,
    hapi_material_info_init/0,
    hapi_parm_info_init/0,
    hapi_get_available_assets/2,
    hapi_get_material_on_part/4,
    hapi_curve_info_init/0,
    hapi_parm_info_is_node_path/1,
    hapi_cook_options_create/0,
    hapi_extract_image_to_file/6,
    hapi_set_time/1,
    hapi_get_image_info/2,
    hapi_get_parm_string_values/4,
    hapi_get_material_on_group/4,
    hapi_python_thread_interpreter_lock/1,
    hapi_set_timeline_options/1,
    hapi_get_volume_tile_float_data/4,
    hapi_get_attribute_float_data/7,
    hapi_connect_asset_geometry/4,
    hapi_object_info_create/0,
    hapi_get_supported_image_file_formats/1,
    hapi_check_for_new_assets/0
    ]).

%
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

% Module init.
-spec init() -> ok | {error, any()}.
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
        end,
    erlang:load_nif(filename:join(PrivDir, hapi_nif), 0).

% Corresponds to HAPI_TimelineOptions_Create function.
%-spec hapi_timeline_options_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_timeline_options_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetStringBufLength function.
%-spec hapi_get_string_buf_length(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_string_buf_length(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsFloat function.
%-spec hapi_parm_info_is_float(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_is_float(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetPreset function.
%-spec hapi_get_preset(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_preset(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_AttributeInfo_Init function.
%-spec hapi_attribute_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_attribute_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_HandleInfo_Init function.
%-spec hapi_handle_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_handle_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_Create function.
%-spec hapi_part_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_part_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeStringData function.
%-spec hapi_get_attribute_string_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_attribute_string_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetCookingCurrentCount function.
%-spec hapi_get_cooking_current_count(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_cooking_current_count(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmFloatValue function.
%-spec hapi_get_parm_float_value(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_float_value(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInfo_GetGroupCountByType function.
%-spec hapi_geo_info_get_group_count_by_type(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_geo_info_get_group_count_by_type(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CurveInfo_Create function.
%-spec hapi_curve_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_curve_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_Cleanup function.
%-spec hapi_cleanup(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_cleanup(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetEnvInt function.
%-spec hapi_get_env_int(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_env_int(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetStatusString function.
%-spec hapi_get_status_string(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_status_string(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsNonValue function.
%-spec hapi_parm_info_is_non_value(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_is_non_value(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_GetFloatValueCount function.
%-spec hapi_parm_info_get_float_value_count(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_get_float_value_count(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetFaceCounts function.
%-spec hapi_set_face_counts(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_face_counts(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_NodeInfo_Create function.
%-spec hapi_node_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_node_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetHandleInfo function.
%-spec hapi_get_handle_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_handle_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetNodeInfo function.
%-spec hapi_get_node_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_node_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_IsAssetValid function.
%-spec hapi_is_asset_valid(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_is_asset_valid(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetInstanceTransforms function.
%-spec hapi_get_instance_transforms(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_instance_transforms(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeNames function.
%-spec hapi_get_attribute_names(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_attribute_names(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmChoiceInfo_Create function.
%-spec hapi_parm_choice_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_choice_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GlobalNodes_Init function.
%-spec hapi_global_nodes_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_global_nodes_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_VolumeTileInfo_Create function.
%-spec hapi_volume_tile_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_volume_tile_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeInfo function.
%-spec hapi_get_attribute_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_attribute_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_LoadAssetLibraryFromFile function.
%-spec hapi_load_asset_library_from_file(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_load_asset_library_from_file(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInputInfo_Init function.
%-spec hapi_geo_input_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_geo_input_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetAssetTransform function.
%-spec hapi_set_asset_transform(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_asset_transform(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetCurveCounts function.
%-spec hapi_get_curve_counts(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_curve_counts(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmFloatValues function.
%-spec hapi_set_parm_float_values(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_parm_float_values(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetPartInfo function.
%-spec hapi_get_part_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_part_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInfo_Create function.
%-spec hapi_geo_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_geo_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SaveHIPFile function.
%-spec hapi_save_hipfile(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_save_hipfile(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_MaterialInfo_Create function.
%-spec hapi_material_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_material_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetAttributeIntData function.
%-spec hapi_set_attribute_int_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_attribute_int_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_GetElementCountByAttributeOwner function.
%-spec hapi_part_info_get_element_count_by_attribute_owner(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_part_info_get_element_count_by_attribute_owner(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAssetTransform function.
%-spec hapi_get_asset_transform(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_asset_transform(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_InsertMultiparmInstance function.
%-spec hapi_insert_multiparm_instance(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_insert_multiparm_instance(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetPresetBufLength function.
%-spec hapi_get_preset_buf_length(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_preset_buf_length(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ExtractImageToMemory function.
%-spec hapi_extract_image_to_memory(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_extract_image_to_memory(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_DisconnectAssetGeometry function.
%-spec hapi_disconnect_asset_geometry(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_disconnect_asset_geometry(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ImageFileFormat_Create function.
%-spec hapi_image_file_format_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_image_file_format_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetPartInfo function.
%-spec hapi_set_part_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_part_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetVertexList function.
%-spec hapi_set_vertex_list(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_vertex_list(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetAttributeStringData function.
%-spec hapi_set_attribute_string_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_attribute_string_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetObjectTransforms function.
%-spec hapi_get_object_transforms(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_object_transforms(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetTransformAnimCurve function.
%-spec hapi_set_transform_anim_curve(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_transform_anim_curve(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetGeoInfo function.
%-spec hapi_get_geo_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_geo_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_Create function.
%-spec hapi_parm_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ImageInfo_Init function.
%-spec hapi_image_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_image_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsString function.
%-spec hapi_parm_info_is_string(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_is_string(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeIntData function.
%-spec hapi_get_attribute_int_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_attribute_int_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetGroupMembership function.
%-spec hapi_get_group_membership(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_group_membership(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetTime function.
%-spec hapi_get_time(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_time(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmInfoFromName function.
%-spec hapi_get_parm_info_from_name(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_info_from_name(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetFaceCounts function.
%-spec hapi_get_face_counts(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_face_counts(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetCurveCounts function.
%-spec hapi_set_curve_counts(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_curve_counts(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_AttributeInfo_Create function.
%-spec hapi_attribute_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_attribute_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_LoadAssetLibraryFromMemory function.
%-spec hapi_load_asset_library_from_memory(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_load_asset_library_from_memory(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetNewAssetIds function.
%-spec hapi_get_new_asset_ids(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_new_asset_ids(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetObjectTransform function.
%-spec hapi_set_object_transform(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_object_transform(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_HandleInfo_Create function.
%-spec hapi_handle_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_handle_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetObjects function.
%-spec hapi_get_objects(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_objects(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetVolumeTileFloatData function.
%-spec hapi_set_volume_tile_float_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_volume_tile_float_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ImageFileFormat_Init function.
%-spec hapi_image_file_format_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_image_file_format_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAssetInfo function.
%-spec hapi_get_asset_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_asset_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_RenderMaterialToImage function.
%-spec hapi_render_material_to_image(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_render_material_to_image(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_GetAttributeCountByOwner function.
%-spec hapi_part_info_get_attribute_count_by_owner(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_part_info_get_attribute_count_by_owner(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ConnectAssetTransform function.
%-spec hapi_connect_asset_transform(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_connect_asset_transform(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_VolumeInfo_Create function.
%-spec hapi_volume_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_volume_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetGlobalNodes function.
%-spec hapi_get_global_nodes(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_global_nodes(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetHandleBindingInfo function.
%-spec hapi_get_handle_binding_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_handle_binding_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetMaterialInfo function.
%-spec hapi_get_material_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_material_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetVolumeInfo function.
%-spec hapi_get_volume_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_volume_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_TimelineOptions_Init function.
%-spec hapi_timeline_options_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_timeline_options_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GlobalNodes_Create function.
%-spec hapi_global_nodes_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_global_nodes_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetImagePlanes function.
%-spec hapi_get_image_planes(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_image_planes(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_HandleBindingInfo_Create function.
%-spec hapi_handle_binding_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_handle_binding_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmStringValue function.
%-spec hapi_set_parm_string_value(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_parm_string_value(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmIntValue function.
%-spec hapi_set_parm_int_value(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_parm_int_value(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetCookingTotalCount function.
%-spec hapi_get_cooking_total_count(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_cooking_total_count(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_RenderTextureToImage function.
%-spec hapi_render_texture_to_image(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_render_texture_to_image(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetString function.
%-spec hapi_get_string(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_string(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetImagePlaneCount function.
%-spec hapi_get_image_plane_count(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_image_plane_count(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertMatrixToEuler function.
%-spec hapi_convert_matrix_to_euler(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_convert_matrix_to_euler(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmChoiceLists function.
%-spec hapi_get_parm_choice_lists(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_choice_lists(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_GetElementCountByGroupType function.
%-spec hapi_part_info_get_element_count_by_group_type(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_part_info_get_element_count_by_group_type(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_RemoveMultiparmInstance function.
%-spec hapi_remove_multiparm_instance(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_remove_multiparm_instance(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetVertexList function.
%-spec hapi_get_vertex_list(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_vertex_list(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAvailableAssetCount function.
%-spec hapi_get_available_asset_count(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_available_asset_count(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetNextVolumeTile function.
%-spec hapi_get_next_volume_tile(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_next_volume_tile(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetImageInfo function.
%-spec hapi_set_image_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_image_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_NodeInfo_Init function.
%-spec hapi_node_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_node_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_GetStringValueCount function.
%-spec hapi_parm_info_get_string_value_count(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_get_string_value_count(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_DestroyAsset function.
%-spec hapi_destroy_asset(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_destroy_asset(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetInputName function.
%-spec hapi_get_input_name(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_input_name(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmIntValues function.
%-spec hapi_get_parm_int_values(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_int_values(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmIntValues function.
%-spec hapi_set_parm_int_values(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_parm_int_values(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CookOptions_Init function.
%-spec hapi_cook_options_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_cook_options_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmStringValue function.
%-spec hapi_get_parm_string_value(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_string_value(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetAnimCurve function.
%-spec hapi_set_anim_curve(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_anim_curve(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmInfo function.
%-spec hapi_get_parm_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetCurveKnots function.
%-spec hapi_set_curve_knots(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_curve_knots(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ResetSimulation function.
%-spec hapi_reset_simulation(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_reset_simulation(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetPreset function.
%-spec hapi_set_preset(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_preset(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_Keyframe_Init function.
%-spec hapi_keyframe_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_keyframe_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_Interrupt function.
%-spec hapi_interrupt(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_interrupt(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetImageMemoryBuffer function.
%-spec hapi_get_image_memory_buffer(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_image_memory_buffer(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertMatrixToQuat function.
%-spec hapi_convert_matrix_to_quat(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_convert_matrix_to_quat(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParameters function.
%-spec hapi_get_parameters(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parameters(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertTransform function.
%-spec hapi_convert_transform(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_convert_transform(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CreateCurve function.
%-spec hapi_create_curve(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_create_curve(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsInt function.
%-spec hapi_parm_info_is_int(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_is_int(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertTransformQuatToMatrix function.
%-spec hapi_convert_transform_quat_to_matrix(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_convert_transform_quat_to_matrix(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_GetIntValueCount function.
%-spec hapi_parm_info_get_int_value_count(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_get_int_value_count(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_VolumeTileInfo_Init function.
%-spec hapi_volume_tile_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_volume_tile_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SaveGeoToMemory function.
%-spec hapi_save_geo_to_memory(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_save_geo_to_memory(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_InstantiateAsset function.
%-spec hapi_instantiate_asset(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_instantiate_asset(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetSupportedImageFileFormatCount function.
%-spec hapi_get_supported_image_file_format_count(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_supported_image_file_format_count(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetVolumeInfo function.
%-spec hapi_set_volume_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_volume_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SaveGeoToFile function.
%-spec hapi_save_geo_to_file(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_save_geo_to_file(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetCurveOrders function.
%-spec hapi_get_curve_orders(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_curve_orders(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_AssetInfo_Create function.
%-spec hapi_asset_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_asset_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetCurveInfo function.
%-spec hapi_get_curve_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_curve_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_IsInitialized function.
%-spec hapi_is_initialized(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_is_initialized(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_DisconnectAssetTransform function.
%-spec hapi_disconnect_asset_transform(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_disconnect_asset_transform(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetCurveInfo function.
%-spec hapi_set_curve_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_curve_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_Initialize function.
%-spec hapi_initialize(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_initialize(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_Init function.
%-spec hapi_part_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_part_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmFloatValues function.
%-spec hapi_get_parm_float_values(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_float_values(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_HandleBindingInfo_Init function.
%-spec hapi_handle_binding_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_handle_binding_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetVolumeTileIntData function.
%-spec hapi_get_volume_tile_int_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_volume_tile_int_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ObjectInfo_Init function.
%-spec hapi_object_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_object_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetTimelineOptions function.
%-spec hapi_get_timeline_options(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_timeline_options(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CommitGeo function.
%-spec hapi_commit_geo(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_commit_geo(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmChoiceInfo_Init function.
%-spec hapi_parm_choice_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_choice_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ImageInfo_Create function.
%-spec hapi_image_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_image_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CookAsset function.
%-spec hapi_cook_asset(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_cook_asset(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_LoadHIPFile function.
%-spec hapi_load_hipfile(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_load_hipfile(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInfo_Init function.
%-spec hapi_geo_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_geo_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_VolumeInfo_Init function.
%-spec hapi_volume_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_volume_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmIntValue function.
%-spec hapi_get_parm_int_value(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_int_value(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_AddAttribute function.
%-spec hapi_add_attribute(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_add_attribute(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_AddGroup function.
%-spec hapi_add_group(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_add_group(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertTransformEulerToMatrix function.
%-spec hapi_convert_transform_euler_to_matrix(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_convert_transform_euler_to_matrix(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetAttributeFloatData function.
%-spec hapi_set_attribute_float_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_attribute_float_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmIdFromName function.
%-spec hapi_get_parm_id_from_name(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_id_from_name(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetGroupMembership function.
%-spec hapi_set_group_membership(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_group_membership(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetVolumeTileIntData function.
%-spec hapi_set_volume_tile_int_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_volume_tile_int_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetStatus function.
%-spec hapi_get_status(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_status(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetMaterialIdsOnFaces function.
%-spec hapi_get_material_ids_on_faces(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_material_ids_on_faces(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetGeoSize function.
%-spec hapi_get_geo_size(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_geo_size(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetCurveOrders function.
%-spec hapi_set_curve_orders(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_curve_orders(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetStatusStringBufLength function.
%-spec hapi_get_status_string_buf_length(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_status_string_buf_length(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetCurveKnots function.
%-spec hapi_get_curve_knots(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_curve_knots(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetFirstVolumeTile function.
%-spec hapi_get_first_volume_tile(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_first_volume_tile(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetGroupNames function.
%-spec hapi_get_group_names(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_group_names(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_LoadGeoFromFile function.
%-spec hapi_load_geo_from_file(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_load_geo_from_file(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_RevertGeo function.
%-spec hapi_revert_geo(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_revert_geo(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_AssetInfo_Init function.
%-spec hapi_asset_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_asset_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsPath function.
%-spec hapi_parm_info_is_path(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_is_path(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CreateInputAsset function.
%-spec hapi_create_input_asset(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_create_input_asset(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmFloatValue function.
%-spec hapi_set_parm_float_value(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_parm_float_value(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_LoadGeoFromMemory function.
%-spec hapi_load_geo_from_memory(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_load_geo_from_memory(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsFilePath function.
%-spec hapi_parm_info_is_file_path(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_is_file_path(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_Keyframe_Create function.
%-spec hapi_keyframe_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_keyframe_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetGeoInfo function.
%-spec hapi_set_geo_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_geo_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInputInfo_Create function.
%-spec hapi_geo_input_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_geo_input_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_MaterialInfo_Init function.
%-spec hapi_material_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_material_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_Init function.
%-spec hapi_parm_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAvailableAssets function.
%-spec hapi_get_available_assets(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_available_assets(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetMaterialOnPart function.
%-spec hapi_get_material_on_part(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_material_on_part(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CurveInfo_Init function.
%-spec hapi_curve_info_init(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_curve_info_init(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsNodePath function.
%-spec hapi_parm_info_is_node_path(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_parm_info_is_node_path(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CookOptions_Create function.
%-spec hapi_cook_options_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_cook_options_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ExtractImageToFile function.
%-spec hapi_extract_image_to_file(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_extract_image_to_file(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetTime function.
%-spec hapi_set_time(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_time(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetImageInfo function.
%-spec hapi_get_image_info(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_image_info(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmStringValues function.
%-spec hapi_get_parm_string_values(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_parm_string_values(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetMaterialOnGroup function.
%-spec hapi_get_material_on_group(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_material_on_group(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_PythonThreadInterpreterLock function.
%-spec hapi_python_thread_interpreter_lock(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_python_thread_interpreter_lock(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_SetTimelineOptions function.
%-spec hapi_set_timeline_options(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_set_timeline_options(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetVolumeTileFloatData function.
%-spec hapi_get_volume_tile_float_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_volume_tile_float_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeFloatData function.
%-spec hapi_get_attribute_float_data(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_attribute_float_data(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ConnectAssetGeometry function.
%-spec hapi_connect_asset_geometry(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_connect_asset_geometry(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_ObjectInfo_Create function.
%-spec hapi_object_info_create(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_object_info_create(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_GetSupportedImageFileFormats function.
%-spec hapi_get_supported_image_file_formats(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_get_supported_image_file_formats(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.


% Corresponds to HAPI_CheckForNewAssets function.
%-spec hapi_check_for_new_assets(%{HAPI_FUNCTION_PARAMS}%) -> {%{HAPI_FUNCTION_RETURN}%}.
hapi_check_for_new_assets(%{HAPI_FUNCTION_PARAMS}%) ->
    ?nif_stub.

