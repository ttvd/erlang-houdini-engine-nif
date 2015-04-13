%%% @author Mykola Konyk  <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% This file has been auto-generated.

-module(hapi).
-version(1.9).

-on_load(init/0).
-define(nif_stub, nif_stub_error(?LINE)).

% Includes generated from HAPI_Common.h
-include("hapi_cook_options.hrl").

% Imports are generated from HAPI_Common.h
-import(hapi_result, [hapi_result_to_int/1, int_to_hapi_result/1]).

% Exports are generated from HAPI.h
-export([
    hapi_is_initialized/0,
    hapi_initialize/5,
    hapi_cleanup/0,
    hapi_get_env_int/2,
    hapi_get_status/2,
    hapi_get_status_string_buf_length/3,
    hapi_get_status_string/2,
    hapi_get_cooking_total_count/1,
    hapi_get_cooking_current_count/1,
    hapi_convert_transform/3,
    hapi_convert_matrix_to_quat/3,
    hapi_convert_matrix_to_euler/4,
    hapi_convert_transform_quat_to_matrix/2,
    hapi_convert_transform_euler_to_matrix/2,
    hapi_python_thread_interpreter_lock/1,
    hapi_get_string_buf_length/2,
    hapi_get_string/3,
    hapi_get_time/1,
    hapi_set_time/1,
    hapi_get_timeline_options/1,
    hapi_set_timeline_options/1,
    hapi_is_asset_valid/3,
    hapi_load_asset_library_from_file/3,
    hapi_load_asset_library_from_memory/4,
    hapi_get_available_asset_count/2,
    hapi_get_available_assets/3,
    hapi_instantiate_asset/3,
    hapi_create_curve/1,
    hapi_create_input_asset/2,
    hapi_destroy_asset/1,
    hapi_get_asset_info/2,
    hapi_cook_asset/2,
    hapi_interrupt/0,
    hapi_get_asset_transform/4,
    hapi_set_asset_transform/2,
    hapi_get_input_name/4,
    hapi_load_h_i_p_file/2,
    hapi_check_for_new_assets/1,
    hapi_get_new_asset_ids/1,
    hapi_save_h_i_p_file/1,
    hapi_get_node_info/2,
    hapi_get_global_nodes/1,
    hapi_get_parameters/4,
    hapi_get_parm_info/3,
    hapi_get_parm_id_from_name/3,
    hapi_get_parm_info_from_name/3,
    hapi_get_parm_int_value/4,
    hapi_get_parm_int_values/4,
    hapi_get_parm_float_value/4,
    hapi_get_parm_float_values/4,
    hapi_get_parm_string_value/5,
    hapi_get_parm_string_values/5,
    hapi_get_parm_choice_lists/4,
    hapi_set_parm_int_value/4,
    hapi_set_parm_int_values/4,
    hapi_set_parm_float_value/4,
    hapi_set_parm_float_values/4,
    hapi_set_parm_string_value/4,
    hapi_insert_multiparm_instance/3,
    hapi_remove_multiparm_instance/3,
    hapi_get_handle_info/4,
    hapi_get_handle_binding_info/5,
    hapi_get_preset_buf_length/4,
    hapi_get_preset/3,
    hapi_set_preset/5,
    hapi_get_objects/4,
    hapi_get_object_transforms/5,
    hapi_get_instance_transforms/7,
    hapi_set_object_transform/3,
    hapi_get_geo_info/4,
    hapi_get_part_info/5,
    hapi_get_face_counts/7,
    hapi_get_vertex_list/7,
    hapi_get_attribute_info/7,
    hapi_get_attribute_names/7,
    hapi_get_attribute_int_data/9,
    hapi_get_attribute_float_data/9,
    hapi_get_attribute_string_data/9,
    hapi_get_group_names/6,
    hapi_get_group_membership/9,
    hapi_set_geo_info/4,
    hapi_set_part_info/4,
    hapi_set_face_counts/6,
    hapi_set_vertex_list/6,
    hapi_add_attribute/5,
    hapi_set_attribute_int_data/8,
    hapi_set_attribute_float_data/8,
    hapi_set_attribute_string_data/8,
    hapi_add_group/5,
    hapi_set_group_membership/8,
    hapi_commit_geo/3,
    hapi_revert_geo/3,
    hapi_connect_asset_transform/3,
    hapi_disconnect_asset_transform/2,
    hapi_connect_asset_geometry/4,
    hapi_disconnect_asset_geometry/2,
    hapi_get_material_ids_on_faces/8,
    hapi_get_material_info/3,
    hapi_render_material_to_image/3,
    hapi_render_texture_to_image/3,
    hapi_get_supported_image_file_format_count/1,
    hapi_get_supported_image_file_formats/2,
    hapi_get_image_info/3,
    hapi_set_image_info/3,
    hapi_get_image_plane_count/3,
    hapi_get_image_planes/4,
    hapi_extract_image_to_file/7,
    hapi_extract_image_to_memory/5,
    hapi_get_image_memory_buffer/4,
    hapi_set_anim_curve/5,
    hapi_set_transform_anim_curve/4,
    hapi_reset_simulation/1,
    hapi_get_volume_info/5,
    hapi_get_first_volume_tile/5,
    hapi_get_next_volume_tile/5,
    hapi_get_volume_tile_float_data/6,
    hapi_get_volume_tile_int_data/6,
    hapi_set_volume_info/4,
    hapi_set_volume_tile_float_data/5,
    hapi_set_volume_tile_int_data/5,
    hapi_get_curve_info/5,
    hapi_get_curve_counts/7,
    hapi_get_curve_orders/7,
    hapi_get_curve_knots/7,
    hapi_set_curve_info/5,
    hapi_set_curve_counts/7,
    hapi_set_curve_orders/7,
    hapi_set_curve_knots/7,
    hapi_save_geo_to_file/4,
    hapi_load_geo_from_file/4,
    hapi_get_geo_size/5,
    hapi_save_geo_to_memory/5,
    hapi_load_geo_from_memory/6
    ]).

%
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

%
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

%
hapi_is_initialized() ->
    ?nif_stub.

%
hapi_initialize(_OtlSearchPath, _DsoSearchPath, _CookOptions, _UseCookingThread, _CookingThreadStackSize) ->
    ?nif_stub.

%
hapi_cleanup() ->
    ?nif_stub.

%
hapi_get_env_int(_IntType, _Value) ->
    ?nif_stub.

%
hapi_get_status(_StatusType, _Status) ->
    ?nif_stub.

%
hapi_get_status_string_buf_length(_StatusType, _Verbosity, _BufferSize) ->
    ?nif_stub.

%
hapi_get_status_string(_StatusType, _Buffer) ->
    ?nif_stub.

%
hapi_get_cooking_total_count(_Count) ->
    ?nif_stub.

%
hapi_get_cooking_current_count(_Count) ->
    ?nif_stub.

%
hapi_convert_transform(_TransformInOut, _RstOrder, _RotOrder) ->
    ?nif_stub.

%
hapi_convert_matrix_to_quat(_Mat, _RstOrder, _TransformOut) ->
    ?nif_stub.

%
hapi_convert_matrix_to_euler(_Mat, _RstOrder, _RotOrder, _TransformOut) ->
    ?nif_stub.

%
hapi_convert_transform_quat_to_matrix(_Transform, _Matrix) ->
    ?nif_stub.

%
hapi_convert_transform_euler_to_matrix(_Transform, _Matrix) ->
    ?nif_stub.

%
hapi_python_thread_interpreter_lock(_Locked) ->
    ?nif_stub.

%
hapi_get_string_buf_length(_StringHandle, _BufferLength) ->
    ?nif_stub.

%
hapi_get_string(_StringHandle, _StringValue, _BufferLength) ->
    ?nif_stub.

%
hapi_get_time(_Time) ->
    ?nif_stub.

%
hapi_set_time(_Time) ->
    ?nif_stub.

%
hapi_get_timeline_options(_TimelineOptions) ->
    ?nif_stub.

%
hapi_set_timeline_options(_TimelineOptions) ->
    ?nif_stub.

%
hapi_is_asset_valid(_AssetId, _AssetValidationId, _Answer) ->
    ?nif_stub.

%
hapi_load_asset_library_from_file(_FilePath, _AllowOverwrite, _LibraryId) ->
    ?nif_stub.

%
hapi_load_asset_library_from_memory(_LibraryBuffer, _LibraryBufferSize, _AllowOverwrite, _LibraryId) ->
    ?nif_stub.

%
hapi_get_available_asset_count(_LibraryId, _AssetCount) ->
    ?nif_stub.

%
hapi_get_available_assets(_LibraryId, _AssetNames, _AssetCount) ->
    ?nif_stub.

%
hapi_instantiate_asset(_AssetName, _CookOnLoad, _AssetId) ->
    ?nif_stub.

%
hapi_create_curve(_AssetId) ->
    ?nif_stub.

%
hapi_create_input_asset(_AssetId, _Name) ->
    ?nif_stub.

%
hapi_destroy_asset(_AssetId) ->
    ?nif_stub.

%
hapi_get_asset_info(_AssetId, _AssetInfo) ->
    ?nif_stub.

%
hapi_cook_asset(_AssetId, _CookOptions) ->
    ?nif_stub.

%
hapi_interrupt() ->
    ?nif_stub.

%
hapi_get_asset_transform(_AssetId, _RstOrder, _RotOrder, _Transform) ->
    ?nif_stub.

%
hapi_set_asset_transform(_AssetId, _Transform) ->
    ?nif_stub.

%
hapi_get_input_name(_AssetId, _InputIdx, _InputType, _Name) ->
    ?nif_stub.

%
hapi_load_h_i_p_file(_FileName, _CookOnLoad) ->
    ?nif_stub.

%
hapi_check_for_new_assets(_NewAssetCount) ->
    ?nif_stub.

%
hapi_get_new_asset_ids(_AssetIds) ->
    ?nif_stub.

%
hapi_save_h_i_p_file(_FilePath) ->
    ?nif_stub.

%
hapi_get_node_info(_NodeId, _NodeInfo) ->
    ?nif_stub.

%
hapi_get_global_nodes(_GlobalNodes) ->
    ?nif_stub.

%
hapi_get_parameters(_NodeId, _ParmInfos, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_parm_info(_NodeId, _ParmId, _ParmInfo) ->
    ?nif_stub.

%
hapi_get_parm_id_from_name(_NodeId, _ParmName, _ParmId) ->
    ?nif_stub.

%
hapi_get_parm_info_from_name(_NodeId, _ParmName, _ParmInfo) ->
    ?nif_stub.

%
hapi_get_parm_int_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
hapi_get_parm_int_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_parm_float_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
hapi_get_parm_float_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_parm_string_value(_NodeId, _ParmName, _Index, _Evaluate, _Value) ->
    ?nif_stub.

%
hapi_get_parm_string_values(_NodeId, _Evaluate, _Values, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_parm_choice_lists(_NodeId, _ParmChoices, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_parm_int_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
hapi_set_parm_int_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_parm_float_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
hapi_set_parm_float_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_parm_string_value(_NodeId, _Value, _ParmId, _Index) ->
    ?nif_stub.

%
hapi_insert_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.

%
hapi_remove_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.

%
hapi_get_handle_info(_AssetId, _HandleInfos, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_handle_binding_info(_AssetId, _HandleIndex, _HandleInfos, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_preset_buf_length(_NodeId, _PresetType, _PresetName, _BufferLength) ->
    ?nif_stub.

%
hapi_get_preset(_NodeId, _Buffer, _BufferLength) ->
    ?nif_stub.

%
hapi_set_preset(_NodeId, _PresetType, _PresetName, _Buffer, _BufferLength) ->
    ?nif_stub.

%
hapi_get_objects(_AssetId, _ObjectInfos, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_object_transforms(_AssetId, _RstOrder, _Transforms, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_instance_transforms(_AssetId, _ObjectId, _GeoId, _RstOrder, _Transforms, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_object_transform(_AssetId, _ObjectId, _Transform) ->
    ?nif_stub.

%
hapi_get_geo_info(_AssetId, _ObjectId, _GeoId, _GeoInfo) ->
    ?nif_stub.

%
hapi_get_part_info(_AssetId, _ObjectId, _GeoId, _PartId, _PartInfo) ->
    ?nif_stub.

%
hapi_get_face_counts(_AssetId, _ObjectId, _GeoId, _PartId, _FaceCounts, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_vertex_list(_AssetId, _ObjectId, _GeoId, _PartId, _VertexList, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_attribute_info(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Owner, _AttrInfo) ->
    ?nif_stub.

%
hapi_get_attribute_names(_AssetId, _ObjectId, _GeoId, _PartId, _Owner, _AttributeNames, _Count) ->
    ?nif_stub.

%
hapi_get_attribute_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_attribute_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_attribute_string_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_group_names(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupNames, _GroupCount) ->
    ?nif_stub.

%
hapi_get_group_membership(_AssetId, _ObjectId, _GeoId, _PartId, _GroupType, _GroupName, _Membership, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_geo_info(_AssetId, _ObjectId, _GeoId, _GeoInfo) ->
    ?nif_stub.

%
hapi_set_part_info(_AssetId, _ObjectId, _GeoId, _PartInfo) ->
    ?nif_stub.

%
hapi_set_face_counts(_AssetId, _ObjectId, _GeoId, _FaceCounts, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_vertex_list(_AssetId, _ObjectId, _GeoId, _VertexList, _Start, _Length) ->
    ?nif_stub.

%
hapi_add_attribute(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo) ->
    ?nif_stub.

%
hapi_set_attribute_int_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_attribute_float_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_attribute_string_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
hapi_add_group(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName) ->
    ?nif_stub.

%
hapi_set_group_membership(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName, _Membership, _Start, _Length) ->
    ?nif_stub.

%
hapi_commit_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.

%
hapi_revert_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.

%
hapi_connect_asset_transform(_AssetIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.

%
hapi_disconnect_asset_transform(_AssetId, _InputIdx) ->
    ?nif_stub.

%
hapi_connect_asset_geometry(_AssetIdFrom, _ObjectIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.

%
hapi_disconnect_asset_geometry(_AssetId, _InputIdx) ->
    ?nif_stub.

%
hapi_get_material_ids_on_faces(_AssetId, _ObjectId, _GeoId, _PartId, _AreAllTheSame, _MaterialIds, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_material_info(_AssetId, _MaterialId, _MaterialInfo) ->
    ?nif_stub.

%
hapi_render_material_to_image(_AssetId, _MaterialId, _ShaderType) ->
    ?nif_stub.

%
hapi_render_texture_to_image(_AssetId, _MaterialId, _ParmId) ->
    ?nif_stub.

%
hapi_get_supported_image_file_format_count(_FileFormatCount) ->
    ?nif_stub.

%
hapi_get_supported_image_file_formats(_Formats, _FileFormatCount) ->
    ?nif_stub.

%
hapi_get_image_info(_AssetId, _MaterialId, _ImageInfo) ->
    ?nif_stub.

%
hapi_set_image_info(_AssetId, _MaterialId, _ImageInfo) ->
    ?nif_stub.

%
hapi_get_image_plane_count(_AssetId, _MaterialId, _ImagePlaneCount) ->
    ?nif_stub.

%
hapi_get_image_planes(_AssetId, _MaterialId, _ImagePlanes, _ImagePlaneCount) ->
    ?nif_stub.

%
hapi_extract_image_to_file(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _DestinationFolderPath, _DestinationFileName, _DestinationFilePath) ->
    ?nif_stub.

%
hapi_extract_image_to_memory(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _BufferSize) ->
    ?nif_stub.

%
hapi_get_image_memory_buffer(_AssetId, _MaterialId, _Buffer, _BufferSize) ->
    ?nif_stub.

%
hapi_set_anim_curve(_NodeId, _ParmId, _ParmIndex, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.

%
hapi_set_transform_anim_curve(_NodeId, _TransComp, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.

%
hapi_reset_simulation(_AssetId) ->
    ?nif_stub.

%
hapi_get_volume_info(_AssetId, _ObjectId, _GeoId, _PartId, _VolumeInfo) ->
    ?nif_stub.

%
hapi_get_first_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId, _Tile) ->
    ?nif_stub.

%
hapi_get_next_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId, _Next) ->
    ?nif_stub.

%
hapi_get_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Tile, _Values) ->
    ?nif_stub.

%
hapi_get_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Tile, _Values) ->
    ?nif_stub.

%
hapi_set_volume_info(_AssetId, _ObjectId, _GeoId, _VolumeInfo) ->
    ?nif_stub.

%
hapi_set_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.

%
hapi_set_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.

%
hapi_get_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) ->
    ?nif_stub.

%
hapi_get_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) ->
    ?nif_stub.

%
hapi_get_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) ->
    ?nif_stub.

%
hapi_set_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) ->
    ?nif_stub.

%
hapi_set_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) ->
    ?nif_stub.

%
hapi_save_geo_to_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.

%
hapi_load_geo_from_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.

%
hapi_get_geo_size(_AssetId, _ObjectId, _GeoId, _Format, _Size) ->
    ?nif_stub.

%
hapi_save_geo_to_memory(_AssetId, _ObjectId, _GeoId, _Buffer, _Size) ->
    ?nif_stub.

%
hapi_load_geo_from_memory(_AssetId, _ObjectId, _GeoId, _Format, _Buffer, _Size) ->
    ?nif_stub.
