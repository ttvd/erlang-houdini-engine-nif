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
    is_initialized/0,
    initialize/5,
    cleanup/0,
    get_env_int/2,
    get_status/2,
    get_status_string_buf_length/3,
    get_status_string/2,
    get_cooking_total_count/1,
    get_cooking_current_count/1,
    convert_transform/3,
    convert_matrix_to_quat/3,
    convert_matrix_to_euler/4,
    convert_transform_quat_to_matrix/2,
    convert_transform_euler_to_matrix/2,
    python_thread_interpreter_lock/1,
    get_string_buf_length/2,
    get_string/3,
    get_time/1,
    set_time/1,
    get_timeline_options/1,
    set_timeline_options/1,
    is_asset_valid/3,
    load_asset_library_from_file/3,
    load_asset_library_from_memory/4,
    get_available_asset_count/2,
    get_available_assets/3,
    instantiate_asset/3,
    create_curve/1,
    create_input_asset/2,
    destroy_asset/1,
    get_asset_info/2,
    cook_asset/2,
    interrupt/0,
    get_asset_transform/4,
    set_asset_transform/2,
    get_input_name/4,
    load_h_i_p_file/2,
    check_for_new_assets/1,
    get_new_asset_ids/1,
    save_h_i_p_file/1,
    get_node_info/2,
    get_global_nodes/1,
    get_parameters/4,
    get_parm_info/3,
    get_parm_id_from_name/3,
    get_parm_info_from_name/3,
    get_parm_int_value/4,
    get_parm_int_values/4,
    get_parm_float_value/4,
    get_parm_float_values/4,
    get_parm_string_value/5,
    get_parm_string_values/5,
    get_parm_choice_lists/4,
    set_parm_int_value/4,
    set_parm_int_values/4,
    set_parm_float_value/4,
    set_parm_float_values/4,
    set_parm_string_value/4,
    insert_multiparm_instance/3,
    remove_multiparm_instance/3,
    get_handle_info/4,
    get_handle_binding_info/5,
    get_preset_buf_length/4,
    get_preset/3,
    set_preset/5,
    get_objects/4,
    get_object_transforms/5,
    get_instance_transforms/7,
    set_object_transform/3,
    get_geo_info/4,
    get_part_info/5,
    get_face_counts/7,
    get_vertex_list/7,
    get_attribute_info/7,
    get_attribute_names/7,
    get_attribute_int_data/9,
    get_attribute_float_data/9,
    get_attribute_string_data/9,
    get_group_names/6,
    get_group_membership/9,
    set_geo_info/4,
    set_part_info/4,
    set_face_counts/6,
    set_vertex_list/6,
    add_attribute/5,
    set_attribute_int_data/8,
    set_attribute_float_data/8,
    set_attribute_string_data/8,
    add_group/5,
    set_group_membership/8,
    commit_geo/3,
    revert_geo/3,
    connect_asset_transform/3,
    disconnect_asset_transform/2,
    connect_asset_geometry/4,
    disconnect_asset_geometry/2,
    get_material_ids_on_faces/8,
    get_material_info/3,
    render_material_to_image/3,
    render_texture_to_image/3,
    get_supported_image_file_format_count/1,
    get_supported_image_file_formats/2,
    get_image_info/3,
    set_image_info/3,
    get_image_plane_count/3,
    get_image_planes/4,
    extract_image_to_file/7,
    extract_image_to_memory/5,
    get_image_memory_buffer/4,
    set_anim_curve/5,
    set_transform_anim_curve/4,
    reset_simulation/1,
    get_volume_info/5,
    get_first_volume_tile/5,
    get_next_volume_tile/5,
    get_volume_tile_float_data/6,
    get_volume_tile_int_data/6,
    set_volume_info/4,
    set_volume_tile_float_data/5,
    set_volume_tile_int_data/5,
    get_curve_info/5,
    get_curve_counts/7,
    get_curve_orders/7,
    get_curve_knots/7,
    set_curve_info/5,
    set_curve_counts/7,
    set_curve_orders/7,
    set_curve_knots/7,
    save_geo_to_file/4,
    load_geo_from_file/4,
    get_geo_size/5,
    save_geo_to_memory/5,
    load_geo_from_memory/6
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
is_initialized() ->
    ?nif_stub.

%
initialize(_OtlSearchPath, _DsoSearchPath, _CookOptions, _UseCookingThread, _CookingThreadStackSize) ->
    ?nif_stub.

%
cleanup() ->
    ?nif_stub.

%
get_env_int(_IntType, _Value) ->
    ?nif_stub.

%
get_status(_StatusType, _Status) ->
    ?nif_stub.

%
get_status_string_buf_length(_StatusType, _Verbosity, _BufferSize) ->
    ?nif_stub.

%
get_status_string(_StatusType, _Buffer) ->
    ?nif_stub.

%
get_cooking_total_count(_Count) ->
    ?nif_stub.

%
get_cooking_current_count(_Count) ->
    ?nif_stub.

%
convert_transform(_TransformInOut, _RstOrder, _RotOrder) ->
    ?nif_stub.

%
convert_matrix_to_quat(_Mat, _RstOrder, _TransformOut) ->
    ?nif_stub.

%
convert_matrix_to_euler(_Mat, _RstOrder, _RotOrder, _TransformOut) ->
    ?nif_stub.

%
convert_transform_quat_to_matrix(_Transform, _Matrix) ->
    ?nif_stub.

%
convert_transform_euler_to_matrix(_Transform, _Matrix) ->
    ?nif_stub.

%
python_thread_interpreter_lock(_Locked) ->
    ?nif_stub.

%
get_string_buf_length(_StringHandle, _BufferLength) ->
    ?nif_stub.

%
get_string(_StringHandle, _StringValue, _BufferLength) ->
    ?nif_stub.

%
get_time(_Time) ->
    ?nif_stub.

%
set_time(_Time) ->
    ?nif_stub.

%
get_timeline_options(_TimelineOptions) ->
    ?nif_stub.

%
set_timeline_options(_TimelineOptions) ->
    ?nif_stub.

%
is_asset_valid(_AssetId, _AssetValidationId, _Answer) ->
    ?nif_stub.

%
load_asset_library_from_file(_FilePath, _AllowOverwrite, _LibraryId) ->
    ?nif_stub.

%
load_asset_library_from_memory(_LibraryBuffer, _LibraryBufferSize, _AllowOverwrite, _LibraryId) ->
    ?nif_stub.

%
get_available_asset_count(_LibraryId, _AssetCount) ->
    ?nif_stub.

%
get_available_assets(_LibraryId, _AssetNames, _AssetCount) ->
    ?nif_stub.

%
instantiate_asset(_AssetName, _CookOnLoad, _AssetId) ->
    ?nif_stub.

%
create_curve(_AssetId) ->
    ?nif_stub.

%
create_input_asset(_AssetId, _Name) ->
    ?nif_stub.

%
destroy_asset(_AssetId) ->
    ?nif_stub.

%
get_asset_info(_AssetId, _AssetInfo) ->
    ?nif_stub.

%
cook_asset(_AssetId, _CookOptions) ->
    ?nif_stub.

%
interrupt() ->
    ?nif_stub.

%
get_asset_transform(_AssetId, _RstOrder, _RotOrder, _Transform) ->
    ?nif_stub.

%
set_asset_transform(_AssetId, _Transform) ->
    ?nif_stub.

%
get_input_name(_AssetId, _InputIdx, _InputType, _Name) ->
    ?nif_stub.

%
load_h_i_p_file(_FileName, _CookOnLoad) ->
    ?nif_stub.

%
check_for_new_assets(_NewAssetCount) ->
    ?nif_stub.

%
get_new_asset_ids(_AssetIds) ->
    ?nif_stub.

%
save_h_i_p_file(_FilePath) ->
    ?nif_stub.

%
get_node_info(_NodeId, _NodeInfo) ->
    ?nif_stub.

%
get_global_nodes(_GlobalNodes) ->
    ?nif_stub.

%
get_parameters(_NodeId, _ParmInfos, _Start, _Length) ->
    ?nif_stub.

%
get_parm_info(_NodeId, _ParmId, _ParmInfo) ->
    ?nif_stub.

%
get_parm_id_from_name(_NodeId, _ParmName, _ParmId) ->
    ?nif_stub.

%
get_parm_info_from_name(_NodeId, _ParmName, _ParmInfo) ->
    ?nif_stub.

%
get_parm_int_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
get_parm_int_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
get_parm_float_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
get_parm_float_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
get_parm_string_value(_NodeId, _ParmName, _Index, _Evaluate, _Value) ->
    ?nif_stub.

%
get_parm_string_values(_NodeId, _Evaluate, _Values, _Start, _Length) ->
    ?nif_stub.

%
get_parm_choice_lists(_NodeId, _ParmChoices, _Start, _Length) ->
    ?nif_stub.

%
set_parm_int_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
set_parm_int_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
set_parm_float_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
set_parm_float_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
set_parm_string_value(_NodeId, _Value, _ParmId, _Index) ->
    ?nif_stub.

%
insert_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.

%
remove_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.

%
get_handle_info(_AssetId, _HandleInfos, _Start, _Length) ->
    ?nif_stub.

%
get_handle_binding_info(_AssetId, _HandleIndex, _HandleInfos, _Start, _Length) ->
    ?nif_stub.

%
get_preset_buf_length(_NodeId, _PresetType, _PresetName, _BufferLength) ->
    ?nif_stub.

%
get_preset(_NodeId, _Buffer, _BufferLength) ->
    ?nif_stub.

%
set_preset(_NodeId, _PresetType, _PresetName, _Buffer, _BufferLength) ->
    ?nif_stub.

%
get_objects(_AssetId, _ObjectInfos, _Start, _Length) ->
    ?nif_stub.

%
get_object_transforms(_AssetId, _RstOrder, _Transforms, _Start, _Length) ->
    ?nif_stub.

%
get_instance_transforms(_AssetId, _ObjectId, _GeoId, _RstOrder, _Transforms, _Start, _Length) ->
    ?nif_stub.

%
set_object_transform(_AssetId, _ObjectId, _Transform) ->
    ?nif_stub.

%
get_geo_info(_AssetId, _ObjectId, _GeoId, _GeoInfo) ->
    ?nif_stub.

%
get_part_info(_AssetId, _ObjectId, _GeoId, _PartId, _PartInfo) ->
    ?nif_stub.

%
get_face_counts(_AssetId, _ObjectId, _GeoId, _PartId, _FaceCounts, _Start, _Length) ->
    ?nif_stub.

%
get_vertex_list(_AssetId, _ObjectId, _GeoId, _PartId, _VertexList, _Start, _Length) ->
    ?nif_stub.

%
get_attribute_info(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Owner, _AttrInfo) ->
    ?nif_stub.

%
get_attribute_names(_AssetId, _ObjectId, _GeoId, _PartId, _Owner, _AttributeNames, _Count) ->
    ?nif_stub.

%
get_attribute_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
get_attribute_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
get_attribute_string_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
get_group_names(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupNames, _GroupCount) ->
    ?nif_stub.

%
get_group_membership(_AssetId, _ObjectId, _GeoId, _PartId, _GroupType, _GroupName, _Membership, _Start, _Length) ->
    ?nif_stub.

%
set_geo_info(_AssetId, _ObjectId, _GeoId, _GeoInfo) ->
    ?nif_stub.

%
set_part_info(_AssetId, _ObjectId, _GeoId, _PartInfo) ->
    ?nif_stub.

%
set_face_counts(_AssetId, _ObjectId, _GeoId, _FaceCounts, _Start, _Length) ->
    ?nif_stub.

%
set_vertex_list(_AssetId, _ObjectId, _GeoId, _VertexList, _Start, _Length) ->
    ?nif_stub.

%
add_attribute(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo) ->
    ?nif_stub.

%
set_attribute_int_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
set_attribute_float_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
set_attribute_string_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
add_group(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName) ->
    ?nif_stub.

%
set_group_membership(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName, _Membership, _Start, _Length) ->
    ?nif_stub.

%
commit_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.

%
revert_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.

%
connect_asset_transform(_AssetIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.

%
disconnect_asset_transform(_AssetId, _InputIdx) ->
    ?nif_stub.

%
connect_asset_geometry(_AssetIdFrom, _ObjectIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.

%
disconnect_asset_geometry(_AssetId, _InputIdx) ->
    ?nif_stub.

%
get_material_ids_on_faces(_AssetId, _ObjectId, _GeoId, _PartId, _AreAllTheSame, _MaterialIds, _Start, _Length) ->
    ?nif_stub.

%
get_material_info(_AssetId, _MaterialId, _MaterialInfo) ->
    ?nif_stub.

%
render_material_to_image(_AssetId, _MaterialId, _ShaderType) ->
    ?nif_stub.

%
render_texture_to_image(_AssetId, _MaterialId, _ParmId) ->
    ?nif_stub.

%
get_supported_image_file_format_count(_FileFormatCount) ->
    ?nif_stub.

%
get_supported_image_file_formats(_Formats, _FileFormatCount) ->
    ?nif_stub.

%
get_image_info(_AssetId, _MaterialId, _ImageInfo) ->
    ?nif_stub.

%
set_image_info(_AssetId, _MaterialId, _ImageInfo) ->
    ?nif_stub.

%
get_image_plane_count(_AssetId, _MaterialId, _ImagePlaneCount) ->
    ?nif_stub.

%
get_image_planes(_AssetId, _MaterialId, _ImagePlanes, _ImagePlaneCount) ->
    ?nif_stub.

%
extract_image_to_file(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _DestinationFolderPath, _DestinationFileName, _DestinationFilePath) ->
    ?nif_stub.

%
extract_image_to_memory(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _BufferSize) ->
    ?nif_stub.

%
get_image_memory_buffer(_AssetId, _MaterialId, _Buffer, _BufferSize) ->
    ?nif_stub.

%
set_anim_curve(_NodeId, _ParmId, _ParmIndex, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.

%
set_transform_anim_curve(_NodeId, _TransComp, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.

%
reset_simulation(_AssetId) ->
    ?nif_stub.

%
get_volume_info(_AssetId, _ObjectId, _GeoId, _PartId, _VolumeInfo) ->
    ?nif_stub.

%
get_first_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId, _Tile) ->
    ?nif_stub.

%
get_next_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId, _Next) ->
    ?nif_stub.

%
get_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Tile, _Values) ->
    ?nif_stub.

%
get_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Tile, _Values) ->
    ?nif_stub.

%
set_volume_info(_AssetId, _ObjectId, _GeoId, _VolumeInfo) ->
    ?nif_stub.

%
set_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.

%
set_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.

%
get_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) ->
    ?nif_stub.

%
get_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) ->
    ?nif_stub.

%
get_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) ->
    ?nif_stub.

%
get_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) ->
    ?nif_stub.

%
set_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) ->
    ?nif_stub.

%
set_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) ->
    ?nif_stub.

%
set_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) ->
    ?nif_stub.

%
set_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) ->
    ?nif_stub.

%
save_geo_to_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.

%
load_geo_from_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.

%
get_geo_size(_AssetId, _ObjectId, _GeoId, _Format, _Size) ->
    ?nif_stub.

%
save_geo_to_memory(_AssetId, _ObjectId, _GeoId, _Buffer, _Size) ->
    ?nif_stub.

%
load_geo_from_memory(_AssetId, _ObjectId, _GeoId, _Format, _Buffer, _Size) ->
    ?nif_stub.
