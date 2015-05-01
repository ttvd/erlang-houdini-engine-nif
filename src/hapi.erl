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
%-spec hapi_timeline_options_create() -> {hapi_timeline_options()}.
hapi_timeline_options_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetStringBufLength function.
%-spec hapi_get_string_buf_length(_StringHandle) -> {atom(), integer()}.
hapi_get_string_buf_length(_StringHandle) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsFloat function.
%-spec hapi_parm_info_is_float(_In) -> {boolean()}.
hapi_parm_info_is_float(_In) ->
    ?nif_stub.


% Corresponds to HAPI_GetPreset function.
%-spec hapi_get_preset(_NodeId, _BufferLength) -> {atom(), byte()}.
hapi_get_preset(_NodeId, _BufferLength) ->
    ?nif_stub.


% Corresponds to HAPI_AttributeInfo_Init function.
%-spec hapi_attribute_info_init() -> {atom(), hapi_attribute_info()}.
hapi_attribute_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_HandleInfo_Init function.
%-spec hapi_handle_info_init() -> {atom(), hapi_handle_info()}.
hapi_handle_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_Create function.
%-spec hapi_part_info_create() -> {hapi_part_info()}.
hapi_part_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeStringData function.
%-spec hapi_get_attribute_string_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {atom(), hapi_attribute_info(), integer()}.
hapi_get_attribute_string_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetCookingCurrentCount function.
%-spec hapi_get_cooking_current_count() -> {atom(), integer()}.
hapi_get_cooking_current_count() ->
    ?nif_stub.


% Corresponds to HAPI_GetParmFloatValue function.
%-spec hapi_get_parm_float_value(_NodeId, _ParmName, _Index) -> {atom(), float()}.
hapi_get_parm_float_value(_NodeId, _ParmName, _Index) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInfo_GetGroupCountByType function.
%-spec hapi_geo_info_get_group_count_by_type(_Type) -> {integer(), hapi_geo_info()}.
hapi_geo_info_get_group_count_by_type(_Type) ->
    ?nif_stub.


% Corresponds to HAPI_CurveInfo_Create function.
%-spec hapi_curve_info_create() -> {hapi_curve_info()}.
hapi_curve_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_Cleanup function.
%-spec hapi_cleanup() -> {atom()}.
hapi_cleanup() ->
    ?nif_stub.


% Corresponds to HAPI_GetEnvInt function.
%-spec hapi_get_env_int(_IntType) -> {atom(), integer()}.
hapi_get_env_int(_IntType) ->
    ?nif_stub.


% Corresponds to HAPI_GetStatusString function.
%-spec hapi_get_status_string(_StatusType) -> {atom(), byte()}.
hapi_get_status_string(_StatusType) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsNonValue function.
%-spec hapi_parm_info_is_non_value(_In) -> {boolean()}.
hapi_parm_info_is_non_value(_In) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_GetFloatValueCount function.
%-spec hapi_parm_info_get_float_value_count(_In) -> {integer()}.
hapi_parm_info_get_float_value_count(_In) ->
    ?nif_stub.


% Corresponds to HAPI_SetFaceCounts function.
%-spec hapi_set_face_counts(_AssetId, _ObjectId, _GeoId, _FaceCounts, _Start, _Length) -> {atom()}.
hapi_set_face_counts(_AssetId, _ObjectId, _GeoId, _FaceCounts, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_NodeInfo_Create function.
%-spec hapi_node_info_create() -> {hapi_node_info()}.
hapi_node_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetHandleInfo function.
%-spec hapi_get_handle_info(_AssetId, _Start, _Length) -> {atom(), list()}.
hapi_get_handle_info(_AssetId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetNodeInfo function.
%-spec hapi_get_node_info(_NodeId) -> {atom(), hapi_node_info()}.
hapi_get_node_info(_NodeId) ->
    ?nif_stub.


% Corresponds to HAPI_IsAssetValid function.
%-spec hapi_is_asset_valid(_AssetId, _AssetValidationId) -> {atom(), integer()}.
hapi_is_asset_valid(_AssetId, _AssetValidationId) ->
    ?nif_stub.


% Corresponds to HAPI_GetInstanceTransforms function.
%-spec hapi_get_instance_transforms(_AssetId, _ObjectId, _GeoId, _RstOrder, _Start, _Length) -> {atom(), list()}.
hapi_get_instance_transforms(_AssetId, _ObjectId, _GeoId, _RstOrder, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeNames function.
%-spec hapi_get_attribute_names(_AssetId, _ObjectId, _GeoId, _PartId, _Owner, _Count) -> {atom(), list()}.
hapi_get_attribute_names(_AssetId, _ObjectId, _GeoId, _PartId, _Owner, _Count) ->
    ?nif_stub.


% Corresponds to HAPI_ParmChoiceInfo_Create function.
%-spec hapi_parm_choice_info_create() -> {hapi_parm_choice_info()}.
hapi_parm_choice_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_GlobalNodes_Init function.
%-spec hapi_global_nodes_init() -> {atom(), list()}.
hapi_global_nodes_init() ->
    ?nif_stub.


% Corresponds to HAPI_VolumeTileInfo_Create function.
%-spec hapi_volume_tile_info_create() -> {hapi_volume_tile_info()}.
hapi_volume_tile_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeInfo function.
%-spec hapi_get_attribute_info(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Owner) -> {atom(), hapi_attribute_info()}.
hapi_get_attribute_info(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Owner) ->
    ?nif_stub.


% Corresponds to HAPI_LoadAssetLibraryFromFile function.
%-spec hapi_load_asset_library_from_file(_FilePath, _AllowOverwrite) -> {atom(), integer()}.
hapi_load_asset_library_from_file(_FilePath, _AllowOverwrite) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInputInfo_Init function.
%-spec hapi_geo_input_info_init() -> {atom(), hapi_geo_input_info()}.
hapi_geo_input_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_SetAssetTransform function.
%-spec hapi_set_asset_transform(_AssetId) -> {atom(), hapi_transform_euler()}.
hapi_set_asset_transform(_AssetId) ->
    ?nif_stub.


% Corresponds to HAPI_GetCurveCounts function.
%-spec hapi_get_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), list()}.
hapi_get_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmFloatValues function.
%-spec hapi_set_parm_float_values(_NodeId, _Values, _Start, _Length) -> {atom()}.
hapi_set_parm_float_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetPartInfo function.
%-spec hapi_get_part_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_part_info()}.
hapi_get_part_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInfo_Create function.
%-spec hapi_geo_info_create() -> {hapi_geo_info()}.
hapi_geo_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_SaveHIPFile function.
%-spec hapi_save_hipfile(_FilePath) -> {atom()}.
hapi_save_hipfile(_FilePath) ->
    ?nif_stub.


% Corresponds to HAPI_MaterialInfo_Create function.
%-spec hapi_material_info_create() -> {hapi_material_info()}.
hapi_material_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_SetAttributeIntData function.
%-spec hapi_set_attribute_int_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) -> {atom()}.
hapi_set_attribute_int_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_GetElementCountByAttributeOwner function.
%-spec hapi_part_info_get_element_count_by_attribute_owner(_Owner) -> {integer(), hapi_part_info()}.
hapi_part_info_get_element_count_by_attribute_owner(_Owner) ->
    ?nif_stub.


% Corresponds to HAPI_GetAssetTransform function.
%-spec hapi_get_asset_transform(_AssetId, _RstOrder, _RotOrder) -> {atom(), hapi_transform_euler()}.
hapi_get_asset_transform(_AssetId, _RstOrder, _RotOrder) ->
    ?nif_stub.


% Corresponds to HAPI_InsertMultiparmInstance function.
%-spec hapi_insert_multiparm_instance(_NodeId, _ParmId, _InstancePosition) -> {atom()}.
hapi_insert_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.


% Corresponds to HAPI_GetPresetBufLength function.
%-spec hapi_get_preset_buf_length(_NodeId, _PresetType, _PresetName) -> {atom(), integer()}.
hapi_get_preset_buf_length(_NodeId, _PresetType, _PresetName) ->
    ?nif_stub.


% Corresponds to HAPI_ExtractImageToMemory function.
%-spec hapi_extract_image_to_memory(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes) -> {atom(), integer()}.
hapi_extract_image_to_memory(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes) ->
    ?nif_stub.


% Corresponds to HAPI_DisconnectAssetGeometry function.
%-spec hapi_disconnect_asset_geometry(_AssetId, _InputIdx) -> {atom()}.
hapi_disconnect_asset_geometry(_AssetId, _InputIdx) ->
    ?nif_stub.


% Corresponds to HAPI_ImageFileFormat_Create function.
%-spec hapi_image_file_format_create() -> {hapi_image_file_format()}.
hapi_image_file_format_create() ->
    ?nif_stub.


% Corresponds to HAPI_SetPartInfo function.
%-spec hapi_set_part_info(_AssetId, _ObjectId, _GeoId, _PartInfo) -> {atom()}.
hapi_set_part_info(_AssetId, _ObjectId, _GeoId, _PartInfo) ->
    ?nif_stub.


% Corresponds to HAPI_SetVertexList function.
%-spec hapi_set_vertex_list(_AssetId, _ObjectId, _GeoId, _VertexList, _Start, _Length) -> {atom()}.
hapi_set_vertex_list(_AssetId, _ObjectId, _GeoId, _VertexList, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_SetAttributeStringData function.
%-spec hapi_set_attribute_string_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) -> {atom()}.
hapi_set_attribute_string_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetObjectTransforms function.
%-spec hapi_get_object_transforms(_AssetId, _RstOrder, _Start, _Length) -> {atom(), list()}.
hapi_get_object_transforms(_AssetId, _RstOrder, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_SetTransformAnimCurve function.
%-spec hapi_set_transform_anim_curve(_NodeId, _TransComp, _CurveKeyframes, _KeyframeCount) -> {atom()}.
hapi_set_transform_anim_curve(_NodeId, _TransComp, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.


% Corresponds to HAPI_GetGeoInfo function.
%-spec hapi_get_geo_info(_AssetId, _ObjectId, _GeoId) -> {atom(), hapi_geo_info()}.
hapi_get_geo_info(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_Create function.
%-spec hapi_parm_info_create() -> {hapi_parm_info()}.
hapi_parm_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_ImageInfo_Init function.
%-spec hapi_image_info_init() -> {atom(), hapi_image_info()}.
hapi_image_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsString function.
%-spec hapi_parm_info_is_string(_In) -> {boolean()}.
hapi_parm_info_is_string(_In) ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeIntData function.
%-spec hapi_get_attribute_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {atom(), hapi_attribute_info(), integer()}.
hapi_get_attribute_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetGroupMembership function.
%-spec hapi_get_group_membership(_AssetId, _ObjectId, _GeoId, _PartId, _GroupType, _GroupName, _Start, _Length) -> {atom(), integer()}.
hapi_get_group_membership(_AssetId, _ObjectId, _GeoId, _PartId, _GroupType, _GroupName, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetTime function.
%-spec hapi_get_time() -> {atom(), float()}.
hapi_get_time() ->
    ?nif_stub.


% Corresponds to HAPI_GetParmInfoFromName function.
%-spec hapi_get_parm_info_from_name(_NodeId, _ParmName) -> {atom(), hapi_parm_info()}.
hapi_get_parm_info_from_name(_NodeId, _ParmName) ->
    ?nif_stub.


% Corresponds to HAPI_GetFaceCounts function.
%-spec hapi_get_face_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), list()}.
hapi_get_face_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_SetCurveCounts function.
%-spec hapi_set_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) -> {atom()}.
hapi_set_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_AttributeInfo_Create function.
%-spec hapi_attribute_info_create() -> {hapi_attribute_info()}.
hapi_attribute_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_LoadAssetLibraryFromMemory function.
%-spec hapi_load_asset_library_from_memory(_LibraryBuffer, _LibraryBufferSize, _AllowOverwrite) -> {atom(), integer()}.
hapi_load_asset_library_from_memory(_LibraryBuffer, _LibraryBufferSize, _AllowOverwrite) ->
    ?nif_stub.


% Corresponds to HAPI_GetNewAssetIds function.
%-spec hapi_get_new_asset_ids() -> {atom(), list()}.
hapi_get_new_asset_ids() ->
    ?nif_stub.


% Corresponds to HAPI_SetObjectTransform function.
%-spec hapi_set_object_transform(_AssetId, _ObjectId, _Transform) -> {atom()}.
hapi_set_object_transform(_AssetId, _ObjectId, _Transform) ->
    ?nif_stub.


% Corresponds to HAPI_HandleInfo_Create function.
%-spec hapi_handle_info_create() -> {hapi_handle_info()}.
hapi_handle_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetObjects function.
%-spec hapi_get_objects(_AssetId, _Start, _Length) -> {atom(), list()}.
hapi_get_objects(_AssetId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_SetVolumeTileFloatData function.
%-spec hapi_set_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) -> {atom()}.
hapi_set_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.


% Corresponds to HAPI_ImageFileFormat_Init function.
%-spec hapi_image_file_format_init() -> {atom(), hapi_image_file_format()}.
hapi_image_file_format_init() ->
    ?nif_stub.


% Corresponds to HAPI_GetAssetInfo function.
%-spec hapi_get_asset_info(_AssetId) -> {atom(), hapi_asset_info()}.
hapi_get_asset_info(_AssetId) ->
    ?nif_stub.


% Corresponds to HAPI_RenderMaterialToImage function.
%-spec hapi_render_material_to_image(_AssetId, _MaterialId, _ShaderType) -> {atom()}.
hapi_render_material_to_image(_AssetId, _MaterialId, _ShaderType) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_GetAttributeCountByOwner function.
%-spec hapi_part_info_get_attribute_count_by_owner(_Owner) -> {integer(), hapi_part_info()}.
hapi_part_info_get_attribute_count_by_owner(_Owner) ->
    ?nif_stub.


% Corresponds to HAPI_ConnectAssetTransform function.
%-spec hapi_connect_asset_transform(_AssetIdFrom, _AssetIdTo, _InputIdx) -> {atom()}.
hapi_connect_asset_transform(_AssetIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.


% Corresponds to HAPI_VolumeInfo_Create function.
%-spec hapi_volume_info_create() -> {hapi_volume_info()}.
hapi_volume_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetGlobalNodes function.
%-spec hapi_get_global_nodes() -> {atom(), list()}.
hapi_get_global_nodes() ->
    ?nif_stub.


% Corresponds to HAPI_GetHandleBindingInfo function.
%-spec hapi_get_handle_binding_info(_AssetId, _HandleIndex, _Start, _Length) -> {atom(), list()}.
hapi_get_handle_binding_info(_AssetId, _HandleIndex, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetMaterialInfo function.
%-spec hapi_get_material_info(_AssetId, _MaterialId) -> {atom(), hapi_material_info()}.
hapi_get_material_info(_AssetId, _MaterialId) ->
    ?nif_stub.


% Corresponds to HAPI_GetVolumeInfo function.
%-spec hapi_get_volume_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_info()}.
hapi_get_volume_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to HAPI_TimelineOptions_Init function.
%-spec hapi_timeline_options_init() -> {atom(), list()}.
hapi_timeline_options_init() ->
    ?nif_stub.


% Corresponds to HAPI_GlobalNodes_Create function.
%-spec hapi_global_nodes_create() -> {hapi_global_nodes()}.
hapi_global_nodes_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetImagePlanes function.
%-spec hapi_get_image_planes(_AssetId, _MaterialId, _ImagePlaneCount) -> {atom(), list()}.
hapi_get_image_planes(_AssetId, _MaterialId, _ImagePlaneCount) ->
    ?nif_stub.


% Corresponds to HAPI_HandleBindingInfo_Create function.
%-spec hapi_handle_binding_info_create() -> {hapi_handle_binding_info()}.
hapi_handle_binding_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_SetParmStringValue function.
%-spec hapi_set_parm_string_value(_NodeId, _Value, _ParmId, _Index) -> {atom()}.
hapi_set_parm_string_value(_NodeId, _Value, _ParmId, _Index) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmIntValue function.
%-spec hapi_set_parm_int_value(_NodeId, _ParmName, _Index, _Value) -> {atom()}.
hapi_set_parm_int_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.


% Corresponds to HAPI_GetCookingTotalCount function.
%-spec hapi_get_cooking_total_count() -> {atom(), integer()}.
hapi_get_cooking_total_count() ->
    ?nif_stub.


% Corresponds to HAPI_RenderTextureToImage function.
%-spec hapi_render_texture_to_image(_AssetId, _MaterialId, _ParmId) -> {atom()}.
hapi_render_texture_to_image(_AssetId, _MaterialId, _ParmId) ->
    ?nif_stub.


% Corresponds to HAPI_GetString function.
%-spec hapi_get_string(_StringHandle, _BufferLength) -> {atom(), byte()}.
hapi_get_string(_StringHandle, _BufferLength) ->
    ?nif_stub.


% Corresponds to HAPI_GetImagePlaneCount function.
%-spec hapi_get_image_plane_count(_AssetId, _MaterialId) -> {atom(), integer()}.
hapi_get_image_plane_count(_AssetId, _MaterialId) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertMatrixToEuler function.
%-spec hapi_convert_matrix_to_euler(_RstOrder, _RotOrder) -> {atom(), float(), hapi_transform_euler()}.
hapi_convert_matrix_to_euler(_RstOrder, _RotOrder) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmChoiceLists function.
%-spec hapi_get_parm_choice_lists(_NodeId, _Start, _Length) -> {atom(), list()}.
hapi_get_parm_choice_lists(_NodeId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_GetElementCountByGroupType function.
%-spec hapi_part_info_get_element_count_by_group_type(_Type) -> {integer(), hapi_part_info()}.
hapi_part_info_get_element_count_by_group_type(_Type) ->
    ?nif_stub.


% Corresponds to HAPI_RemoveMultiparmInstance function.
%-spec hapi_remove_multiparm_instance(_NodeId, _ParmId, _InstancePosition) -> {atom()}.
hapi_remove_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.


% Corresponds to HAPI_GetVertexList function.
%-spec hapi_get_vertex_list(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), integer()}.
hapi_get_vertex_list(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetAvailableAssetCount function.
%-spec hapi_get_available_asset_count(_LibraryId) -> {atom(), integer()}.
hapi_get_available_asset_count(_LibraryId) ->
    ?nif_stub.


% Corresponds to HAPI_GetNextVolumeTile function.
%-spec hapi_get_next_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_tile_info()}.
hapi_get_next_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to HAPI_SetImageInfo function.
%-spec hapi_set_image_info(_AssetId, _MaterialId, _ImageInfo) -> {atom()}.
hapi_set_image_info(_AssetId, _MaterialId, _ImageInfo) ->
    ?nif_stub.


% Corresponds to HAPI_NodeInfo_Init function.
%-spec hapi_node_info_init() -> {atom(), hapi_node_info()}.
hapi_node_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_GetStringValueCount function.
%-spec hapi_parm_info_get_string_value_count(_In) -> {integer()}.
hapi_parm_info_get_string_value_count(_In) ->
    ?nif_stub.


% Corresponds to HAPI_DestroyAsset function.
%-spec hapi_destroy_asset(_AssetId) -> {atom()}.
hapi_destroy_asset(_AssetId) ->
    ?nif_stub.


% Corresponds to HAPI_GetInputName function.
%-spec hapi_get_input_name(_AssetId, _InputIdx, _InputType) -> {atom(), integer()}.
hapi_get_input_name(_AssetId, _InputIdx, _InputType) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmIntValues function.
%-spec hapi_get_parm_int_values(_NodeId, _Start, _Length) -> {atom(), list()}.
hapi_get_parm_int_values(_NodeId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmIntValues function.
%-spec hapi_set_parm_int_values(_NodeId, _Values, _Start, _Length) -> {atom()}.
hapi_set_parm_int_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_CookOptions_Init function.
%-spec hapi_cook_options_init() -> {atom(), list()}.
hapi_cook_options_init() ->
    ?nif_stub.


% Corresponds to HAPI_GetParmStringValue function.
%-spec hapi_get_parm_string_value(_NodeId, _ParmName, _Index, _Evaluate) -> {atom(), integer()}.
hapi_get_parm_string_value(_NodeId, _ParmName, _Index, _Evaluate) ->
    ?nif_stub.


% Corresponds to HAPI_SetAnimCurve function.
%-spec hapi_set_anim_curve(_NodeId, _ParmId, _ParmIndex, _CurveKeyframes, _KeyframeCount) -> {atom()}.
hapi_set_anim_curve(_NodeId, _ParmId, _ParmIndex, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmInfo function.
%-spec hapi_get_parm_info(_NodeId, _ParmId) -> {atom(), hapi_parm_info()}.
hapi_get_parm_info(_NodeId, _ParmId) ->
    ?nif_stub.


% Corresponds to HAPI_SetCurveKnots function.
%-spec hapi_set_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) -> {atom()}.
hapi_set_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_ResetSimulation function.
%-spec hapi_reset_simulation(_AssetId) -> {atom()}.
hapi_reset_simulation(_AssetId) ->
    ?nif_stub.


% Corresponds to HAPI_SetPreset function.
%-spec hapi_set_preset(_NodeId, _PresetType, _PresetName, _Buffer, _BufferLength) -> {atom()}.
hapi_set_preset(_NodeId, _PresetType, _PresetName, _Buffer, _BufferLength) ->
    ?nif_stub.


% Corresponds to HAPI_Keyframe_Init function.
%-spec hapi_keyframe_init() -> {atom(), hapi_keyframe()}.
hapi_keyframe_init() ->
    ?nif_stub.


% Corresponds to HAPI_Interrupt function.
%-spec hapi_interrupt() -> {atom()}.
hapi_interrupt() ->
    ?nif_stub.


% Corresponds to HAPI_GetImageMemoryBuffer function.
%-spec hapi_get_image_memory_buffer(_AssetId, _MaterialId, _BufferSize) -> {atom(), byte()}.
hapi_get_image_memory_buffer(_AssetId, _MaterialId, _BufferSize) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertMatrixToQuat function.
%-spec hapi_convert_matrix_to_quat(_RstOrder) -> {atom(), float(), hapi_transform()}.
hapi_convert_matrix_to_quat(_RstOrder) ->
    ?nif_stub.


% Corresponds to HAPI_GetParameters function.
%-spec hapi_get_parameters(_NodeId, _Start, _Length) -> {atom(), list()}.
hapi_get_parameters(_NodeId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertTransform function.
%-spec hapi_convert_transform(_RstOrder, _RotOrder) -> {atom(), hapi_transform_euler()}.
hapi_convert_transform(_RstOrder, _RotOrder) ->
    ?nif_stub.


% Corresponds to HAPI_CreateCurve function.
%-spec hapi_create_curve() -> {atom(), integer()}.
hapi_create_curve() ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsInt function.
%-spec hapi_parm_info_is_int(_In) -> {boolean()}.
hapi_parm_info_is_int(_In) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertTransformQuatToMatrix function.
%-spec hapi_convert_transform_quat_to_matrix(_Transform) -> {atom(), float()}.
hapi_convert_transform_quat_to_matrix(_Transform) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_GetIntValueCount function.
%-spec hapi_parm_info_get_int_value_count(_In) -> {integer()}.
hapi_parm_info_get_int_value_count(_In) ->
    ?nif_stub.


% Corresponds to HAPI_VolumeTileInfo_Init function.
%-spec hapi_volume_tile_info_init() -> {atom(), hapi_volume_tile_info()}.
hapi_volume_tile_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_SaveGeoToMemory function.
%-spec hapi_save_geo_to_memory(_AssetId, _ObjectId, _GeoId, _Size) -> {atom(), byte()}.
hapi_save_geo_to_memory(_AssetId, _ObjectId, _GeoId, _Size) ->
    ?nif_stub.


% Corresponds to HAPI_InstantiateAsset function.
%-spec hapi_instantiate_asset(_AssetName, _CookOnLoad) -> {atom(), integer()}.
hapi_instantiate_asset(_AssetName, _CookOnLoad) ->
    ?nif_stub.


% Corresponds to HAPI_GetSupportedImageFileFormatCount function.
%-spec hapi_get_supported_image_file_format_count() -> {atom(), integer()}.
hapi_get_supported_image_file_format_count() ->
    ?nif_stub.


% Corresponds to HAPI_SetVolumeInfo function.
%-spec hapi_set_volume_info(_AssetId, _ObjectId, _GeoId, _VolumeInfo) -> {atom()}.
hapi_set_volume_info(_AssetId, _ObjectId, _GeoId, _VolumeInfo) ->
    ?nif_stub.


% Corresponds to HAPI_SaveGeoToFile function.
%-spec hapi_save_geo_to_file(_AssetId, _ObjectId, _GeoId, _FileName) -> {atom()}.
hapi_save_geo_to_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.


% Corresponds to HAPI_GetCurveOrders function.
%-spec hapi_get_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), list()}.
hapi_get_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_AssetInfo_Create function.
%-spec hapi_asset_info_create() -> {hapi_asset_info()}.
hapi_asset_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetCurveInfo function.
%-spec hapi_get_curve_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_curve_info()}.
hapi_get_curve_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to HAPI_IsInitialized function.
%-spec hapi_is_initialized() -> {atom()}.
hapi_is_initialized() ->
    ?nif_stub.


% Corresponds to HAPI_DisconnectAssetTransform function.
%-spec hapi_disconnect_asset_transform(_AssetId, _InputIdx) -> {atom()}.
hapi_disconnect_asset_transform(_AssetId, _InputIdx) ->
    ?nif_stub.


% Corresponds to HAPI_SetCurveInfo function.
%-spec hapi_set_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) -> {atom()}.
hapi_set_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) ->
    ?nif_stub.


% Corresponds to HAPI_Initialize function.
%-spec hapi_initialize(_OtlSearchPath, _DsoSearchPath, _CookOptions, _UseCookingThread, _CookingThreadStackSize) -> {atom()}.
hapi_initialize(_OtlSearchPath, _DsoSearchPath, _CookOptions, _UseCookingThread, _CookingThreadStackSize) ->
    ?nif_stub.


% Corresponds to HAPI_PartInfo_Init function.
%-spec hapi_part_info_init() -> {atom(), hapi_part_info()}.
hapi_part_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_GetParmFloatValues function.
%-spec hapi_get_parm_float_values(_NodeId, _Start, _Length) -> {atom(), list()}.
hapi_get_parm_float_values(_NodeId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_HandleBindingInfo_Init function.
%-spec hapi_handle_binding_info_init() -> {atom(), hapi_handle_binding_info()}.
hapi_handle_binding_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_GetVolumeTileIntData function.
%-spec hapi_get_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_tile_info(), list()}.
hapi_get_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to HAPI_ObjectInfo_Init function.
%-spec hapi_object_info_init() -> {atom(), hapi_object_info()}.
hapi_object_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_GetTimelineOptions function.
%-spec hapi_get_timeline_options() -> {atom(), list()}.
hapi_get_timeline_options() ->
    ?nif_stub.


% Corresponds to HAPI_CommitGeo function.
%-spec hapi_commit_geo(_AssetId, _ObjectId, _GeoId) -> {atom()}.
hapi_commit_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.


% Corresponds to HAPI_ParmChoiceInfo_Init function.
%-spec hapi_parm_choice_info_init() -> {atom(), hapi_parm_choice_info()}.
hapi_parm_choice_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_ImageInfo_Create function.
%-spec hapi_image_info_create() -> {hapi_image_info()}.
hapi_image_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_CookAsset function.
%-spec hapi_cook_asset(_AssetId, _CookOptions) -> {atom()}.
hapi_cook_asset(_AssetId, _CookOptions) ->
    ?nif_stub.


% Corresponds to HAPI_LoadHIPFile function.
%-spec hapi_load_hipfile(_FileName, _CookOnLoad) -> {atom()}.
hapi_load_hipfile(_FileName, _CookOnLoad) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInfo_Init function.
%-spec hapi_geo_info_init() -> {atom(), hapi_geo_info()}.
hapi_geo_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_VolumeInfo_Init function.
%-spec hapi_volume_info_init() -> {atom(), hapi_volume_info()}.
hapi_volume_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_GetParmIntValue function.
%-spec hapi_get_parm_int_value(_NodeId, _ParmName, _Index) -> {atom(), integer()}.
hapi_get_parm_int_value(_NodeId, _ParmName, _Index) ->
    ?nif_stub.


% Corresponds to HAPI_AddAttribute function.
%-spec hapi_add_attribute(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo) -> {atom()}.
hapi_add_attribute(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo) ->
    ?nif_stub.


% Corresponds to HAPI_AddGroup function.
%-spec hapi_add_group(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName) -> {atom()}.
hapi_add_group(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName) ->
    ?nif_stub.


% Corresponds to HAPI_ConvertTransformEulerToMatrix function.
%-spec hapi_convert_transform_euler_to_matrix(_Transform) -> {atom(), float()}.
hapi_convert_transform_euler_to_matrix(_Transform) ->
    ?nif_stub.


% Corresponds to HAPI_SetAttributeFloatData function.
%-spec hapi_set_attribute_float_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) -> {atom()}.
hapi_set_attribute_float_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmIdFromName function.
%-spec hapi_get_parm_id_from_name(_NodeId, _ParmName) -> {atom(), integer()}.
hapi_get_parm_id_from_name(_NodeId, _ParmName) ->
    ?nif_stub.


% Corresponds to HAPI_SetGroupMembership function.
%-spec hapi_set_group_membership(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName, _Start, _Length) -> {atom(), integer()}.
hapi_set_group_membership(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_SetVolumeTileIntData function.
%-spec hapi_set_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) -> {atom()}.
hapi_set_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.


% Corresponds to HAPI_GetStatus function.
%-spec hapi_get_status(_StatusType) -> {atom(), list()}.
hapi_get_status(_StatusType) ->
    ?nif_stub.


% Corresponds to HAPI_GetMaterialIdsOnFaces function.
%-spec hapi_get_material_ids_on_faces(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), boolean(), list()}.
hapi_get_material_ids_on_faces(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetGeoSize function.
%-spec hapi_get_geo_size(_AssetId, _ObjectId, _GeoId, _Format) -> {atom(), integer()}.
hapi_get_geo_size(_AssetId, _ObjectId, _GeoId, _Format) ->
    ?nif_stub.


% Corresponds to HAPI_SetCurveOrders function.
%-spec hapi_set_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) -> {atom()}.
hapi_set_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetStatusStringBufLength function.
%-spec hapi_get_status_string_buf_length(_StatusType, _Verbosity) -> {atom(), integer()}.
hapi_get_status_string_buf_length(_StatusType, _Verbosity) ->
    ?nif_stub.


% Corresponds to HAPI_GetCurveKnots function.
%-spec hapi_get_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), list()}.
hapi_get_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetFirstVolumeTile function.
%-spec hapi_get_first_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_tile_info()}.
hapi_get_first_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to HAPI_GetGroupNames function.
%-spec hapi_get_group_names(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupCount) -> {atom(), list()}.
hapi_get_group_names(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupCount) ->
    ?nif_stub.


% Corresponds to HAPI_LoadGeoFromFile function.
%-spec hapi_load_geo_from_file(_AssetId, _ObjectId, _GeoId, _FileName) -> {atom()}.
hapi_load_geo_from_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.


% Corresponds to HAPI_RevertGeo function.
%-spec hapi_revert_geo(_AssetId, _ObjectId, _GeoId) -> {atom()}.
hapi_revert_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.


% Corresponds to HAPI_AssetInfo_Init function.
%-spec hapi_asset_info_init() -> {atom(), hapi_asset_info()}.
hapi_asset_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsPath function.
%-spec hapi_parm_info_is_path(_In) -> {boolean()}.
hapi_parm_info_is_path(_In) ->
    ?nif_stub.


% Corresponds to HAPI_CreateInputAsset function.
%-spec hapi_create_input_asset(_Name) -> {atom(), integer()}.
hapi_create_input_asset(_Name) ->
    ?nif_stub.


% Corresponds to HAPI_SetParmFloatValue function.
%-spec hapi_set_parm_float_value(_NodeId, _ParmName, _Index, _Value) -> {atom()}.
hapi_set_parm_float_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.


% Corresponds to HAPI_LoadGeoFromMemory function.
%-spec hapi_load_geo_from_memory(_AssetId, _ObjectId, _GeoId, _Format, _Size) -> {atom(), byte()}.
hapi_load_geo_from_memory(_AssetId, _ObjectId, _GeoId, _Format, _Size) ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsFilePath function.
%-spec hapi_parm_info_is_file_path(_In) -> {boolean()}.
hapi_parm_info_is_file_path(_In) ->
    ?nif_stub.


% Corresponds to HAPI_Keyframe_Create function.
%-spec hapi_keyframe_create() -> {hapi_keyframe()}.
hapi_keyframe_create() ->
    ?nif_stub.


% Corresponds to HAPI_SetGeoInfo function.
%-spec hapi_set_geo_info(_AssetId, _ObjectId, _GeoId) -> {atom(), hapi_geo_info()}.
hapi_set_geo_info(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.


% Corresponds to HAPI_GeoInputInfo_Create function.
%-spec hapi_geo_input_info_create() -> {hapi_geo_input_info()}.
hapi_geo_input_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_MaterialInfo_Init function.
%-spec hapi_material_info_init() -> {atom(), hapi_material_info()}.
hapi_material_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_Init function.
%-spec hapi_parm_info_init() -> {atom(), hapi_parm_info()}.
hapi_parm_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_GetAvailableAssets function.
%-spec hapi_get_available_assets(_LibraryId, _AssetCount) -> {atom(), list()}.
hapi_get_available_assets(_LibraryId, _AssetCount) ->
    ?nif_stub.


% Corresponds to HAPI_GetMaterialOnPart function.
%-spec hapi_get_material_on_part(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_material_info()}.
hapi_get_material_on_part(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to HAPI_CurveInfo_Init function.
%-spec hapi_curve_info_init() -> {atom(), hapi_curve_info()}.
hapi_curve_info_init() ->
    ?nif_stub.


% Corresponds to HAPI_ParmInfo_IsNodePath function.
%-spec hapi_parm_info_is_node_path(_In) -> {boolean()}.
hapi_parm_info_is_node_path(_In) ->
    ?nif_stub.


% Corresponds to HAPI_CookOptions_Create function.
%-spec hapi_cook_options_create() -> {hapi_cook_options()}.
hapi_cook_options_create() ->
    ?nif_stub.


% Corresponds to HAPI_ExtractImageToFile function.
%-spec hapi_extract_image_to_file(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _DestinationFolderPath, _DestinationFileName) -> {atom(), integer()}.
hapi_extract_image_to_file(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _DestinationFolderPath, _DestinationFileName) ->
    ?nif_stub.


% Corresponds to HAPI_SetTime function.
%-spec hapi_set_time(_Time) -> {atom()}.
hapi_set_time(_Time) ->
    ?nif_stub.


% Corresponds to HAPI_GetImageInfo function.
%-spec hapi_get_image_info(_AssetId, _MaterialId) -> {atom(), hapi_image_info()}.
hapi_get_image_info(_AssetId, _MaterialId) ->
    ?nif_stub.


% Corresponds to HAPI_GetParmStringValues function.
%-spec hapi_get_parm_string_values(_NodeId, _Evaluate, _Start, _Length) -> {atom(), list()}.
hapi_get_parm_string_values(_NodeId, _Evaluate, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_GetMaterialOnGroup function.
%-spec hapi_get_material_on_group(_AssetId, _ObjectId, _GeoId, _GroupName) -> {atom(), hapi_material_info()}.
hapi_get_material_on_group(_AssetId, _ObjectId, _GeoId, _GroupName) ->
    ?nif_stub.


% Corresponds to HAPI_PythonThreadInterpreterLock function.
%-spec hapi_python_thread_interpreter_lock(_Locked) -> {atom()}.
hapi_python_thread_interpreter_lock(_Locked) ->
    ?nif_stub.


% Corresponds to HAPI_SetTimelineOptions function.
%-spec hapi_set_timeline_options(_TimelineOptions) -> {atom()}.
hapi_set_timeline_options(_TimelineOptions) ->
    ?nif_stub.


% Corresponds to HAPI_GetVolumeTileFloatData function.
%-spec hapi_get_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_tile_info(), list()}.
hapi_get_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to HAPI_GetAttributeFloatData function.
%-spec hapi_get_attribute_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {atom(), hapi_attribute_info(), float()}.
hapi_get_attribute_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HAPI_ConnectAssetGeometry function.
%-spec hapi_connect_asset_geometry(_AssetIdFrom, _ObjectIdFrom, _AssetIdTo, _InputIdx) -> {atom()}.
hapi_connect_asset_geometry(_AssetIdFrom, _ObjectIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.


% Corresponds to HAPI_ObjectInfo_Create function.
%-spec hapi_object_info_create() -> {hapi_object_info()}.
hapi_object_info_create() ->
    ?nif_stub.


% Corresponds to HAPI_GetSupportedImageFileFormats function.
%-spec hapi_get_supported_image_file_formats(_FileFormatCount) -> {atom(), list()}.
hapi_get_supported_image_file_formats(_FileFormatCount) ->
    ?nif_stub.


% Corresponds to HAPI_CheckForNewAssets function.
%-spec hapi_check_for_new_assets() -> {atom(), integer()}.
hapi_check_for_new_assets() ->
    ?nif_stub.

