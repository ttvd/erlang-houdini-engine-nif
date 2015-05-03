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
    timeline_options_create/0,
    get_string_buf_length/1,
    parm_info_is_float/1,
    get_preset/2,
    attribute_info_init/0,
    handle_info_init/0,
    part_info_create/0,
    get_attribute_string_data/7,
    get_cooking_current_count/0,
    get_parm_float_value/3,
    geo_info_get_group_count_by_type/1,
    curve_info_create/0,
    cleanup/0,
    get_env_int/1,
    get_status_string/2,
    parm_info_is_non_value/1,
    parm_info_get_float_value_count/1,
    set_face_counts/6,
    node_info_create/0,
    get_handle_info/3,
    get_node_info/1,
    is_asset_valid/2,
    get_instance_transforms/6,
    get_attribute_names/6,
    parm_choice_info_create/0,
    global_nodes_init/0,
    volume_tile_info_create/0,
    get_attribute_info/6,
    load_asset_library_from_file/2,
    geo_input_info_init/0,
    set_asset_transform/1,
    get_curve_counts/6,
    set_parm_float_values/4,
    get_part_info/4,
    geo_info_create/0,
    save_hipfile/1,
    material_info_create/0,
    set_attribute_int_data/8,
    part_info_get_element_count_by_attribute_owner/1,
    get_asset_transform/3,
    insert_multiparm_instance/3,
    get_preset_buf_length/3,
    extract_image_to_memory/4,
    disconnect_asset_geometry/2,
    image_file_format_create/0,
    set_part_info/4,
    set_vertex_list/6,
    set_attribute_string_data/8,
    get_object_transforms/4,
    set_transform_anim_curve/4,
    get_geo_info/3,
    parm_info_create/0,
    image_info_init/0,
    parm_info_is_string/1,
    get_attribute_int_data/7,
    get_group_membership/8,
    get_time/0,
    get_parm_info_from_name/2,
    get_face_counts/6,
    set_curve_counts/7,
    attribute_info_create/0,
    load_asset_library_from_memory/3,
    get_new_asset_ids/0,
    set_object_transform/3,
    handle_info_create/0,
    get_objects/3,
    set_volume_tile_float_data/5,
    image_file_format_init/0,
    get_asset_info/1,
    render_material_to_image/3,
    part_info_get_attribute_count_by_owner/1,
    connect_asset_transform/3,
    volume_info_create/0,
    get_global_nodes/0,
    get_handle_binding_info/4,
    get_material_info/2,
    get_volume_info/4,
    timeline_options_init/0,
    global_nodes_create/0,
    get_image_planes/3,
    handle_binding_info_create/0,
    set_parm_string_value/4,
    set_parm_int_value/4,
    get_cooking_total_count/0,
    render_texture_to_image/3,
    get_string/2,
    get_image_plane_count/2,
    convert_matrix_to_euler/3,
    get_parm_choice_lists/3,
    part_info_get_element_count_by_group_type/1,
    remove_multiparm_instance/3,
    get_vertex_list/6,
    get_available_asset_count/1,
    get_next_volume_tile/4,
    set_image_info/3,
    node_info_init/0,
    parm_info_get_string_value_count/1,
    destroy_asset/1,
    get_input_name/3,
    get_parm_int_values/3,
    set_parm_int_values/4,
    cook_options_init/0,
    get_parm_string_value/4,
    set_anim_curve/5,
    get_parm_info/2,
    set_curve_knots/7,
    reset_simulation/1,
    set_preset/5,
    keyframe_init/0,
    interrupt/0,
    get_image_memory_buffer/3,
    convert_matrix_to_quat/2,
    get_parameters/3,
    convert_transform/2,
    create_curve/0,
    parm_info_is_int/1,
    convert_transform_quat_to_matrix/1,
    parm_info_get_int_value_count/1,
    volume_tile_info_init/0,
    save_geo_to_memory/4,
    instantiate_asset/2,
    get_supported_image_file_format_count/0,
    set_volume_info/4,
    save_geo_to_file/4,
    get_curve_orders/6,
    asset_info_create/0,
    get_curve_info/4,
    is_initialized/0,
    disconnect_asset_transform/2,
    set_curve_info/5,
    initialize/5,
    part_info_init/0,
    get_parm_float_values/3,
    handle_binding_info_init/0,
    get_volume_tile_int_data/4,
    object_info_init/0,
    get_timeline_options/0,
    commit_geo/3,
    parm_choice_info_init/0,
    image_info_create/0,
    cook_asset/2,
    load_hipfile/2,
    geo_info_init/0,
    volume_info_init/0,
    get_parm_int_value/3,
    add_attribute/5,
    add_group/5,
    convert_transform_euler_to_matrix/1,
    set_attribute_float_data/8,
    get_parm_id_from_name/2,
    set_group_membership/7,
    set_volume_tile_int_data/5,
    get_status/1,
    get_material_ids_on_faces/6,
    get_geo_size/4,
    set_curve_orders/7,
    get_status_string_buf_length/2,
    get_curve_knots/6,
    get_first_volume_tile/4,
    get_group_names/5,
    load_geo_from_file/4,
    revert_geo/3,
    asset_info_init/0,
    parm_info_is_path/1,
    create_input_asset/1,
    set_parm_float_value/4,
    load_geo_from_memory/5,
    parm_info_is_file_path/1,
    keyframe_create/0,
    set_geo_info/3,
    geo_input_info_create/0,
    material_info_init/0,
    parm_info_init/0,
    get_available_assets/2,
    get_material_on_part/4,
    curve_info_init/0,
    parm_info_is_node_path/1,
    cook_options_create/0,
    extract_image_to_file/6,
    set_time/1,
    get_image_info/2,
    get_parm_string_values/4,
    get_material_on_group/4,
    python_thread_interpreter_lock/1,
    set_timeline_options/1,
    get_volume_tile_float_data/4,
    get_attribute_float_data/7,
    connect_asset_geometry/4,
    object_info_create/0,
    get_supported_image_file_formats/1,
    check_for_new_assets/0
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

% Corresponds to TimelineOptions_Create function.
%-spec timeline_options_create() -> {hapi_timeline_options()}.
timeline_options_create() ->
    ?nif_stub.


% Corresponds to GetStringBufLength function.
%-spec get_string_buf_length(_StringHandle) -> {atom(), integer()}.
get_string_buf_length(_StringHandle) ->
    ?nif_stub.


% Corresponds to ParmInfo_IsFloat function.
%-spec parm_info_is_float(_In) -> {boolean()}.
parm_info_is_float(_In) ->
    ?nif_stub.


% Corresponds to GetPreset function.
%-spec get_preset(_NodeId, _BufferLength) -> {atom(), byte()}.
get_preset(_NodeId, _BufferLength) ->
    ?nif_stub.


% Corresponds to AttributeInfo_Init function.
%-spec attribute_info_init() -> {atom(), hapi_attribute_info()}.
attribute_info_init() ->
    ?nif_stub.


% Corresponds to HandleInfo_Init function.
%-spec handle_info_init() -> {atom(), hapi_handle_info()}.
handle_info_init() ->
    ?nif_stub.


% Corresponds to PartInfo_Create function.
%-spec part_info_create() -> {hapi_part_info()}.
part_info_create() ->
    ?nif_stub.


% Corresponds to GetAttributeStringData function.
%-spec get_attribute_string_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {atom(), hapi_attribute_info(), integer()}.
get_attribute_string_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetCookingCurrentCount function.
%-spec get_cooking_current_count() -> {atom(), integer()}.
get_cooking_current_count() ->
    ?nif_stub.


% Corresponds to GetParmFloatValue function.
%-spec get_parm_float_value(_NodeId, _ParmName, _Index) -> {atom(), float()}.
get_parm_float_value(_NodeId, _ParmName, _Index) ->
    ?nif_stub.


% Corresponds to GeoInfo_GetGroupCountByType function.
%-spec geo_info_get_group_count_by_type(_Type) -> {integer(), hapi_geo_info()}.
geo_info_get_group_count_by_type(_Type) ->
    ?nif_stub.


% Corresponds to CurveInfo_Create function.
%-spec curve_info_create() -> {hapi_curve_info()}.
curve_info_create() ->
    ?nif_stub.


% Corresponds to Cleanup function.
%-spec cleanup() -> {atom()}.
cleanup() ->
    ?nif_stub.


% Corresponds to GetEnvInt function.
%-spec get_env_int(_IntType) -> {atom(), integer()}.
get_env_int(_IntType) ->
    ?nif_stub.


% Corresponds to GetStatusString function.
%-spec get_status_string(_StatusType, _BufferLength) -> {atom(), byte()}.
get_status_string(_StatusType, _BufferLength) ->
    ?nif_stub.


% Corresponds to ParmInfo_IsNonValue function.
%-spec parm_info_is_non_value(_In) -> {boolean()}.
parm_info_is_non_value(_In) ->
    ?nif_stub.


% Corresponds to ParmInfo_GetFloatValueCount function.
%-spec parm_info_get_float_value_count(_In) -> {integer()}.
parm_info_get_float_value_count(_In) ->
    ?nif_stub.


% Corresponds to SetFaceCounts function.
%-spec set_face_counts(_AssetId, _ObjectId, _GeoId, _FaceCounts, _Start, _Length) -> {atom()}.
set_face_counts(_AssetId, _ObjectId, _GeoId, _FaceCounts, _Start, _Length) ->
    ?nif_stub.


% Corresponds to NodeInfo_Create function.
%-spec node_info_create() -> {hapi_node_info()}.
node_info_create() ->
    ?nif_stub.


% Corresponds to GetHandleInfo function.
%-spec get_handle_info(_AssetId, _Start, _Length) -> {atom(), list()}.
get_handle_info(_AssetId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetNodeInfo function.
%-spec get_node_info(_NodeId) -> {atom(), hapi_node_info()}.
get_node_info(_NodeId) ->
    ?nif_stub.


% Corresponds to IsAssetValid function.
%-spec is_asset_valid(_AssetId, _AssetValidationId) -> {atom(), integer()}.
is_asset_valid(_AssetId, _AssetValidationId) ->
    ?nif_stub.


% Corresponds to GetInstanceTransforms function.
%-spec get_instance_transforms(_AssetId, _ObjectId, _GeoId, _RstOrder, _Start, _Length) -> {atom(), list()}.
get_instance_transforms(_AssetId, _ObjectId, _GeoId, _RstOrder, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetAttributeNames function.
%-spec get_attribute_names(_AssetId, _ObjectId, _GeoId, _PartId, _Owner, _Count) -> {atom(), list()}.
get_attribute_names(_AssetId, _ObjectId, _GeoId, _PartId, _Owner, _Count) ->
    ?nif_stub.


% Corresponds to ParmChoiceInfo_Create function.
%-spec parm_choice_info_create() -> {hapi_parm_choice_info()}.
parm_choice_info_create() ->
    ?nif_stub.


% Corresponds to GlobalNodes_Init function.
%-spec global_nodes_init() -> {atom(), hapi_global_nodes()}.
global_nodes_init() ->
    ?nif_stub.


% Corresponds to VolumeTileInfo_Create function.
%-spec volume_tile_info_create() -> {hapi_volume_tile_info()}.
volume_tile_info_create() ->
    ?nif_stub.


% Corresponds to GetAttributeInfo function.
%-spec get_attribute_info(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Owner) -> {atom(), hapi_attribute_info()}.
get_attribute_info(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Owner) ->
    ?nif_stub.


% Corresponds to LoadAssetLibraryFromFile function.
%-spec load_asset_library_from_file(_FilePath, _AllowOverwrite) -> {atom(), integer()}.
load_asset_library_from_file(_FilePath, _AllowOverwrite) ->
    ?nif_stub.


% Corresponds to GeoInputInfo_Init function.
%-spec geo_input_info_init() -> {atom(), hapi_geo_input_info()}.
geo_input_info_init() ->
    ?nif_stub.


% Corresponds to SetAssetTransform function.
%-spec set_asset_transform(_AssetId) -> {atom(), hapi_transform_euler()}.
set_asset_transform(_AssetId) ->
    ?nif_stub.


% Corresponds to GetCurveCounts function.
%-spec get_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), list()}.
get_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to SetParmFloatValues function.
%-spec set_parm_float_values(_NodeId, _Values, _Start, _Length) -> {atom()}.
set_parm_float_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetPartInfo function.
%-spec get_part_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_part_info()}.
get_part_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to GeoInfo_Create function.
%-spec geo_info_create() -> {hapi_geo_info()}.
geo_info_create() ->
    ?nif_stub.


% Corresponds to SaveHIPFile function.
%-spec save_hipfile(_FilePath) -> {atom()}.
save_hipfile(_FilePath) ->
    ?nif_stub.


% Corresponds to MaterialInfo_Create function.
%-spec material_info_create() -> {hapi_material_info()}.
material_info_create() ->
    ?nif_stub.


% Corresponds to SetAttributeIntData function.
%-spec set_attribute_int_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) -> {atom()}.
set_attribute_int_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.


% Corresponds to PartInfo_GetElementCountByAttributeOwner function.
%-spec part_info_get_element_count_by_attribute_owner(_Owner) -> {integer(), hapi_part_info()}.
part_info_get_element_count_by_attribute_owner(_Owner) ->
    ?nif_stub.


% Corresponds to GetAssetTransform function.
%-spec get_asset_transform(_AssetId, _RstOrder, _RotOrder) -> {atom(), hapi_transform_euler()}.
get_asset_transform(_AssetId, _RstOrder, _RotOrder) ->
    ?nif_stub.


% Corresponds to InsertMultiparmInstance function.
%-spec insert_multiparm_instance(_NodeId, _ParmId, _InstancePosition) -> {atom()}.
insert_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.


% Corresponds to GetPresetBufLength function.
%-spec get_preset_buf_length(_NodeId, _PresetType, _PresetName) -> {atom(), integer()}.
get_preset_buf_length(_NodeId, _PresetType, _PresetName) ->
    ?nif_stub.


% Corresponds to ExtractImageToMemory function.
%-spec extract_image_to_memory(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes) -> {atom(), integer()}.
extract_image_to_memory(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes) ->
    ?nif_stub.


% Corresponds to DisconnectAssetGeometry function.
%-spec disconnect_asset_geometry(_AssetId, _InputIdx) -> {atom()}.
disconnect_asset_geometry(_AssetId, _InputIdx) ->
    ?nif_stub.


% Corresponds to ImageFileFormat_Create function.
%-spec image_file_format_create() -> {hapi_image_file_format()}.
image_file_format_create() ->
    ?nif_stub.


% Corresponds to SetPartInfo function.
%-spec set_part_info(_AssetId, _ObjectId, _GeoId, _PartInfo) -> {atom()}.
set_part_info(_AssetId, _ObjectId, _GeoId, _PartInfo) ->
    ?nif_stub.


% Corresponds to SetVertexList function.
%-spec set_vertex_list(_AssetId, _ObjectId, _GeoId, _VertexList, _Start, _Length) -> {atom()}.
set_vertex_list(_AssetId, _ObjectId, _GeoId, _VertexList, _Start, _Length) ->
    ?nif_stub.


% Corresponds to SetAttributeStringData function.
%-spec set_attribute_string_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) -> {atom()}.
set_attribute_string_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetObjectTransforms function.
%-spec get_object_transforms(_AssetId, _RstOrder, _Start, _Length) -> {atom(), list()}.
get_object_transforms(_AssetId, _RstOrder, _Start, _Length) ->
    ?nif_stub.


% Corresponds to SetTransformAnimCurve function.
%-spec set_transform_anim_curve(_NodeId, _TransComp, _CurveKeyframes, _KeyframeCount) -> {atom()}.
set_transform_anim_curve(_NodeId, _TransComp, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.


% Corresponds to GetGeoInfo function.
%-spec get_geo_info(_AssetId, _ObjectId, _GeoId) -> {atom(), hapi_geo_info()}.
get_geo_info(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.


% Corresponds to ParmInfo_Create function.
%-spec parm_info_create() -> {hapi_parm_info()}.
parm_info_create() ->
    ?nif_stub.


% Corresponds to ImageInfo_Init function.
%-spec image_info_init() -> {atom(), hapi_image_info()}.
image_info_init() ->
    ?nif_stub.


% Corresponds to ParmInfo_IsString function.
%-spec parm_info_is_string(_In) -> {boolean()}.
parm_info_is_string(_In) ->
    ?nif_stub.


% Corresponds to GetAttributeIntData function.
%-spec get_attribute_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {atom(), hapi_attribute_info(), integer()}.
get_attribute_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetGroupMembership function.
%-spec get_group_membership(_AssetId, _ObjectId, _GeoId, _PartId, _GroupType, _GroupName, _Start, _Length) -> {atom(), integer()}.
get_group_membership(_AssetId, _ObjectId, _GeoId, _PartId, _GroupType, _GroupName, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetTime function.
%-spec get_time() -> {atom(), float()}.
get_time() ->
    ?nif_stub.


% Corresponds to GetParmInfoFromName function.
%-spec get_parm_info_from_name(_NodeId, _ParmName) -> {atom(), hapi_parm_info()}.
get_parm_info_from_name(_NodeId, _ParmName) ->
    ?nif_stub.


% Corresponds to GetFaceCounts function.
%-spec get_face_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), list()}.
get_face_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to SetCurveCounts function.
%-spec set_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) -> {atom()}.
set_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) ->
    ?nif_stub.


% Corresponds to AttributeInfo_Create function.
%-spec attribute_info_create() -> {hapi_attribute_info()}.
attribute_info_create() ->
    ?nif_stub.


% Corresponds to LoadAssetLibraryFromMemory function.
%-spec load_asset_library_from_memory(_LibraryBuffer, _LibraryBufferSize, _AllowOverwrite) -> {atom(), integer()}.
load_asset_library_from_memory(_LibraryBuffer, _LibraryBufferSize, _AllowOverwrite) ->
    ?nif_stub.


% Corresponds to GetNewAssetIds function.
%-spec get_new_asset_ids() -> {atom(), list()}.
get_new_asset_ids() ->
    ?nif_stub.


% Corresponds to SetObjectTransform function.
%-spec set_object_transform(_AssetId, _ObjectId, _Transform) -> {atom()}.
set_object_transform(_AssetId, _ObjectId, _Transform) ->
    ?nif_stub.


% Corresponds to HandleInfo_Create function.
%-spec handle_info_create() -> {hapi_handle_info()}.
handle_info_create() ->
    ?nif_stub.


% Corresponds to GetObjects function.
%-spec get_objects(_AssetId, _Start, _Length) -> {atom(), list()}.
get_objects(_AssetId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to SetVolumeTileFloatData function.
%-spec set_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) -> {atom()}.
set_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.


% Corresponds to ImageFileFormat_Init function.
%-spec image_file_format_init() -> {atom(), hapi_image_file_format()}.
image_file_format_init() ->
    ?nif_stub.


% Corresponds to GetAssetInfo function.
%-spec get_asset_info(_AssetId) -> {atom(), hapi_asset_info()}.
get_asset_info(_AssetId) ->
    ?nif_stub.


% Corresponds to RenderMaterialToImage function.
%-spec render_material_to_image(_AssetId, _MaterialId, _ShaderType) -> {atom()}.
render_material_to_image(_AssetId, _MaterialId, _ShaderType) ->
    ?nif_stub.


% Corresponds to PartInfo_GetAttributeCountByOwner function.
%-spec part_info_get_attribute_count_by_owner(_Owner) -> {integer(), hapi_part_info()}.
part_info_get_attribute_count_by_owner(_Owner) ->
    ?nif_stub.


% Corresponds to ConnectAssetTransform function.
%-spec connect_asset_transform(_AssetIdFrom, _AssetIdTo, _InputIdx) -> {atom()}.
connect_asset_transform(_AssetIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.


% Corresponds to VolumeInfo_Create function.
%-spec volume_info_create() -> {hapi_volume_info()}.
volume_info_create() ->
    ?nif_stub.


% Corresponds to GetGlobalNodes function.
%-spec get_global_nodes() -> {atom(), list()}.
get_global_nodes() ->
    ?nif_stub.


% Corresponds to GetHandleBindingInfo function.
%-spec get_handle_binding_info(_AssetId, _HandleIndex, _Start, _Length) -> {atom(), list()}.
get_handle_binding_info(_AssetId, _HandleIndex, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetMaterialInfo function.
%-spec get_material_info(_AssetId, _MaterialId) -> {atom(), hapi_material_info()}.
get_material_info(_AssetId, _MaterialId) ->
    ?nif_stub.


% Corresponds to GetVolumeInfo function.
%-spec get_volume_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_info()}.
get_volume_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to TimelineOptions_Init function.
%-spec timeline_options_init() -> {atom(), hapi_timeline_options()}.
timeline_options_init() ->
    ?nif_stub.


% Corresponds to GlobalNodes_Create function.
%-spec global_nodes_create() -> {hapi_global_nodes()}.
global_nodes_create() ->
    ?nif_stub.


% Corresponds to GetImagePlanes function.
%-spec get_image_planes(_AssetId, _MaterialId, _ImagePlaneCount) -> {atom(), list()}.
get_image_planes(_AssetId, _MaterialId, _ImagePlaneCount) ->
    ?nif_stub.


% Corresponds to HandleBindingInfo_Create function.
%-spec handle_binding_info_create() -> {hapi_handle_binding_info()}.
handle_binding_info_create() ->
    ?nif_stub.


% Corresponds to SetParmStringValue function.
%-spec set_parm_string_value(_NodeId, _Value, _ParmId, _Index) -> {atom()}.
set_parm_string_value(_NodeId, _Value, _ParmId, _Index) ->
    ?nif_stub.


% Corresponds to SetParmIntValue function.
%-spec set_parm_int_value(_NodeId, _ParmName, _Index, _Value) -> {atom()}.
set_parm_int_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.


% Corresponds to GetCookingTotalCount function.
%-spec get_cooking_total_count() -> {atom(), integer()}.
get_cooking_total_count() ->
    ?nif_stub.


% Corresponds to RenderTextureToImage function.
%-spec render_texture_to_image(_AssetId, _MaterialId, _ParmId) -> {atom()}.
render_texture_to_image(_AssetId, _MaterialId, _ParmId) ->
    ?nif_stub.


% Corresponds to GetString function.
%-spec get_string(_StringHandle, _BufferLength) -> {atom(), byte()}.
get_string(_StringHandle, _BufferLength) ->
    ?nif_stub.


% Corresponds to GetImagePlaneCount function.
%-spec get_image_plane_count(_AssetId, _MaterialId) -> {atom(), integer()}.
get_image_plane_count(_AssetId, _MaterialId) ->
    ?nif_stub.


% Corresponds to ConvertMatrixToEuler function.
%-spec convert_matrix_to_euler(_Mat, _RstOrder, _RotOrder) -> {atom(), hapi_transform_euler()}.
convert_matrix_to_euler(_Mat, _RstOrder, _RotOrder) ->
    ?nif_stub.


% Corresponds to GetParmChoiceLists function.
%-spec get_parm_choice_lists(_NodeId, _Start, _Length) -> {atom(), list()}.
get_parm_choice_lists(_NodeId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to PartInfo_GetElementCountByGroupType function.
%-spec part_info_get_element_count_by_group_type(_Type) -> {integer(), hapi_part_info()}.
part_info_get_element_count_by_group_type(_Type) ->
    ?nif_stub.


% Corresponds to RemoveMultiparmInstance function.
%-spec remove_multiparm_instance(_NodeId, _ParmId, _InstancePosition) -> {atom()}.
remove_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.


% Corresponds to GetVertexList function.
%-spec get_vertex_list(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), integer()}.
get_vertex_list(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetAvailableAssetCount function.
%-spec get_available_asset_count(_LibraryId) -> {atom(), integer()}.
get_available_asset_count(_LibraryId) ->
    ?nif_stub.


% Corresponds to GetNextVolumeTile function.
%-spec get_next_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_tile_info()}.
get_next_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to SetImageInfo function.
%-spec set_image_info(_AssetId, _MaterialId, _ImageInfo) -> {atom()}.
set_image_info(_AssetId, _MaterialId, _ImageInfo) ->
    ?nif_stub.


% Corresponds to NodeInfo_Init function.
%-spec node_info_init() -> {atom(), hapi_node_info()}.
node_info_init() ->
    ?nif_stub.


% Corresponds to ParmInfo_GetStringValueCount function.
%-spec parm_info_get_string_value_count(_In) -> {integer()}.
parm_info_get_string_value_count(_In) ->
    ?nif_stub.


% Corresponds to DestroyAsset function.
%-spec destroy_asset(_AssetId) -> {atom()}.
destroy_asset(_AssetId) ->
    ?nif_stub.


% Corresponds to GetInputName function.
%-spec get_input_name(_AssetId, _InputIdx, _InputType) -> {atom(), integer()}.
get_input_name(_AssetId, _InputIdx, _InputType) ->
    ?nif_stub.


% Corresponds to GetParmIntValues function.
%-spec get_parm_int_values(_NodeId, _Start, _Length) -> {atom(), list()}.
get_parm_int_values(_NodeId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to SetParmIntValues function.
%-spec set_parm_int_values(_NodeId, _Values, _Start, _Length) -> {atom()}.
set_parm_int_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.


% Corresponds to CookOptions_Init function.
%-spec cook_options_init() -> {atom(), hapi_cook_options()}.
cook_options_init() ->
    ?nif_stub.


% Corresponds to GetParmStringValue function.
%-spec get_parm_string_value(_NodeId, _ParmName, _Index, _Evaluate) -> {atom(), integer()}.
get_parm_string_value(_NodeId, _ParmName, _Index, _Evaluate) ->
    ?nif_stub.


% Corresponds to SetAnimCurve function.
%-spec set_anim_curve(_NodeId, _ParmId, _ParmIndex, _CurveKeyframes, _KeyframeCount) -> {atom()}.
set_anim_curve(_NodeId, _ParmId, _ParmIndex, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.


% Corresponds to GetParmInfo function.
%-spec get_parm_info(_NodeId, _ParmId) -> {atom(), hapi_parm_info()}.
get_parm_info(_NodeId, _ParmId) ->
    ?nif_stub.


% Corresponds to SetCurveKnots function.
%-spec set_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) -> {atom()}.
set_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) ->
    ?nif_stub.


% Corresponds to ResetSimulation function.
%-spec reset_simulation(_AssetId) -> {atom()}.
reset_simulation(_AssetId) ->
    ?nif_stub.


% Corresponds to SetPreset function.
%-spec set_preset(_NodeId, _PresetType, _PresetName, _Buffer, _BufferLength) -> {atom()}.
set_preset(_NodeId, _PresetType, _PresetName, _Buffer, _BufferLength) ->
    ?nif_stub.


% Corresponds to Keyframe_Init function.
%-spec keyframe_init() -> {atom(), hapi_keyframe()}.
keyframe_init() ->
    ?nif_stub.


% Corresponds to Interrupt function.
%-spec interrupt() -> {atom()}.
interrupt() ->
    ?nif_stub.


% Corresponds to GetImageMemoryBuffer function.
%-spec get_image_memory_buffer(_AssetId, _MaterialId, _BufferSize) -> {atom(), byte()}.
get_image_memory_buffer(_AssetId, _MaterialId, _BufferSize) ->
    ?nif_stub.


% Corresponds to ConvertMatrixToQuat function.
%-spec convert_matrix_to_quat(_Mat, _RstOrder) -> {atom(), hapi_transform()}.
convert_matrix_to_quat(_Mat, _RstOrder) ->
    ?nif_stub.


% Corresponds to GetParameters function.
%-spec get_parameters(_NodeId, _Start, _Length) -> {atom(), list()}.
get_parameters(_NodeId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to ConvertTransform function.
%-spec convert_transform(_RstOrder, _RotOrder) -> {atom(), hapi_transform_euler()}.
convert_transform(_RstOrder, _RotOrder) ->
    ?nif_stub.


% Corresponds to CreateCurve function.
%-spec create_curve() -> {atom(), integer()}.
create_curve() ->
    ?nif_stub.


% Corresponds to ParmInfo_IsInt function.
%-spec parm_info_is_int(_In) -> {boolean()}.
parm_info_is_int(_In) ->
    ?nif_stub.


% Corresponds to ConvertTransformQuatToMatrix function.
%-spec convert_transform_quat_to_matrix(_Transform) -> {atom(), float()}.
convert_transform_quat_to_matrix(_Transform) ->
    ?nif_stub.


% Corresponds to ParmInfo_GetIntValueCount function.
%-spec parm_info_get_int_value_count(_In) -> {integer()}.
parm_info_get_int_value_count(_In) ->
    ?nif_stub.


% Corresponds to VolumeTileInfo_Init function.
%-spec volume_tile_info_init() -> {atom(), hapi_volume_tile_info()}.
volume_tile_info_init() ->
    ?nif_stub.


% Corresponds to SaveGeoToMemory function.
%-spec save_geo_to_memory(_AssetId, _ObjectId, _GeoId, _Size) -> {atom(), byte()}.
save_geo_to_memory(_AssetId, _ObjectId, _GeoId, _Size) ->
    ?nif_stub.


% Corresponds to InstantiateAsset function.
%-spec instantiate_asset(_AssetName, _CookOnLoad) -> {atom(), integer()}.
instantiate_asset(_AssetName, _CookOnLoad) ->
    ?nif_stub.


% Corresponds to GetSupportedImageFileFormatCount function.
%-spec get_supported_image_file_format_count() -> {atom(), integer()}.
get_supported_image_file_format_count() ->
    ?nif_stub.


% Corresponds to SetVolumeInfo function.
%-spec set_volume_info(_AssetId, _ObjectId, _GeoId, _VolumeInfo) -> {atom()}.
set_volume_info(_AssetId, _ObjectId, _GeoId, _VolumeInfo) ->
    ?nif_stub.


% Corresponds to SaveGeoToFile function.
%-spec save_geo_to_file(_AssetId, _ObjectId, _GeoId, _FileName) -> {atom()}.
save_geo_to_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.


% Corresponds to GetCurveOrders function.
%-spec get_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), list()}.
get_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to AssetInfo_Create function.
%-spec asset_info_create() -> {hapi_asset_info()}.
asset_info_create() ->
    ?nif_stub.


% Corresponds to GetCurveInfo function.
%-spec get_curve_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_curve_info()}.
get_curve_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to IsInitialized function.
%-spec is_initialized() -> {atom()}.
is_initialized() ->
    ?nif_stub.


% Corresponds to DisconnectAssetTransform function.
%-spec disconnect_asset_transform(_AssetId, _InputIdx) -> {atom()}.
disconnect_asset_transform(_AssetId, _InputIdx) ->
    ?nif_stub.


% Corresponds to SetCurveInfo function.
%-spec set_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) -> {atom()}.
set_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) ->
    ?nif_stub.


% Corresponds to Initialize function.
%-spec initialize(_OtlSearchPath, _DsoSearchPath, _CookOptions, _UseCookingThread, _CookingThreadStackSize) -> {atom()}.
initialize(_OtlSearchPath, _DsoSearchPath, _CookOptions, _UseCookingThread, _CookingThreadStackSize) ->
    ?nif_stub.


% Corresponds to PartInfo_Init function.
%-spec part_info_init() -> {atom(), hapi_part_info()}.
part_info_init() ->
    ?nif_stub.


% Corresponds to GetParmFloatValues function.
%-spec get_parm_float_values(_NodeId, _Start, _Length) -> {atom(), list()}.
get_parm_float_values(_NodeId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to HandleBindingInfo_Init function.
%-spec handle_binding_info_init() -> {atom(), hapi_handle_binding_info()}.
handle_binding_info_init() ->
    ?nif_stub.


% Corresponds to GetVolumeTileIntData function.
%-spec get_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_tile_info(), list()}.
get_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to ObjectInfo_Init function.
%-spec object_info_init() -> {atom(), hapi_object_info()}.
object_info_init() ->
    ?nif_stub.


% Corresponds to GetTimelineOptions function.
%-spec get_timeline_options() -> {atom(), list()}.
get_timeline_options() ->
    ?nif_stub.


% Corresponds to CommitGeo function.
%-spec commit_geo(_AssetId, _ObjectId, _GeoId) -> {atom()}.
commit_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.


% Corresponds to ParmChoiceInfo_Init function.
%-spec parm_choice_info_init() -> {atom(), hapi_parm_choice_info()}.
parm_choice_info_init() ->
    ?nif_stub.


% Corresponds to ImageInfo_Create function.
%-spec image_info_create() -> {hapi_image_info()}.
image_info_create() ->
    ?nif_stub.


% Corresponds to CookAsset function.
%-spec cook_asset(_AssetId, _CookOptions) -> {atom()}.
cook_asset(_AssetId, _CookOptions) ->
    ?nif_stub.


% Corresponds to LoadHIPFile function.
%-spec load_hipfile(_FileName, _CookOnLoad) -> {atom()}.
load_hipfile(_FileName, _CookOnLoad) ->
    ?nif_stub.


% Corresponds to GeoInfo_Init function.
%-spec geo_info_init() -> {atom(), hapi_geo_info()}.
geo_info_init() ->
    ?nif_stub.


% Corresponds to VolumeInfo_Init function.
%-spec volume_info_init() -> {atom(), hapi_volume_info()}.
volume_info_init() ->
    ?nif_stub.


% Corresponds to GetParmIntValue function.
%-spec get_parm_int_value(_NodeId, _ParmName, _Index) -> {atom(), integer()}.
get_parm_int_value(_NodeId, _ParmName, _Index) ->
    ?nif_stub.


% Corresponds to AddAttribute function.
%-spec add_attribute(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo) -> {atom()}.
add_attribute(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo) ->
    ?nif_stub.


% Corresponds to AddGroup function.
%-spec add_group(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName) -> {atom()}.
add_group(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName) ->
    ?nif_stub.


% Corresponds to ConvertTransformEulerToMatrix function.
%-spec convert_transform_euler_to_matrix(_Transform) -> {atom(), float()}.
convert_transform_euler_to_matrix(_Transform) ->
    ?nif_stub.


% Corresponds to SetAttributeFloatData function.
%-spec set_attribute_float_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) -> {atom()}.
set_attribute_float_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetParmIdFromName function.
%-spec get_parm_id_from_name(_NodeId, _ParmName) -> {atom(), integer()}.
get_parm_id_from_name(_NodeId, _ParmName) ->
    ?nif_stub.


% Corresponds to SetGroupMembership function.
%-spec set_group_membership(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName, _Start, _Length) -> {atom(), integer()}.
set_group_membership(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName, _Start, _Length) ->
    ?nif_stub.


% Corresponds to SetVolumeTileIntData function.
%-spec set_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) -> {atom()}.
set_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.


% Corresponds to GetStatus function.
%-spec get_status(_StatusType) -> {atom(), list()}.
get_status(_StatusType) ->
    ?nif_stub.


% Corresponds to GetMaterialIdsOnFaces function.
%-spec get_material_ids_on_faces(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), boolean(), list()}.
get_material_ids_on_faces(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetGeoSize function.
%-spec get_geo_size(_AssetId, _ObjectId, _GeoId, _Format) -> {atom(), integer()}.
get_geo_size(_AssetId, _ObjectId, _GeoId, _Format) ->
    ?nif_stub.


% Corresponds to SetCurveOrders function.
%-spec set_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) -> {atom()}.
set_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetStatusStringBufLength function.
%-spec get_status_string_buf_length(_StatusType, _Verbosity) -> {atom(), integer()}.
get_status_string_buf_length(_StatusType, _Verbosity) ->
    ?nif_stub.


% Corresponds to GetCurveKnots function.
%-spec get_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {atom(), list()}.
get_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetFirstVolumeTile function.
%-spec get_first_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_tile_info()}.
get_first_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to GetGroupNames function.
%-spec get_group_names(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupCount) -> {atom(), list()}.
get_group_names(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupCount) ->
    ?nif_stub.


% Corresponds to LoadGeoFromFile function.
%-spec load_geo_from_file(_AssetId, _ObjectId, _GeoId, _FileName) -> {atom()}.
load_geo_from_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.


% Corresponds to RevertGeo function.
%-spec revert_geo(_AssetId, _ObjectId, _GeoId) -> {atom()}.
revert_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.


% Corresponds to AssetInfo_Init function.
%-spec asset_info_init() -> {atom(), hapi_asset_info()}.
asset_info_init() ->
    ?nif_stub.


% Corresponds to ParmInfo_IsPath function.
%-spec parm_info_is_path(_In) -> {boolean()}.
parm_info_is_path(_In) ->
    ?nif_stub.


% Corresponds to CreateInputAsset function.
%-spec create_input_asset(_Name) -> {atom(), integer()}.
create_input_asset(_Name) ->
    ?nif_stub.


% Corresponds to SetParmFloatValue function.
%-spec set_parm_float_value(_NodeId, _ParmName, _Index, _Value) -> {atom()}.
set_parm_float_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.


% Corresponds to LoadGeoFromMemory function.
%-spec load_geo_from_memory(_AssetId, _ObjectId, _GeoId, _Format, _Size) -> {atom(), byte()}.
load_geo_from_memory(_AssetId, _ObjectId, _GeoId, _Format, _Size) ->
    ?nif_stub.


% Corresponds to ParmInfo_IsFilePath function.
%-spec parm_info_is_file_path(_In) -> {boolean()}.
parm_info_is_file_path(_In) ->
    ?nif_stub.


% Corresponds to Keyframe_Create function.
%-spec keyframe_create() -> {hapi_keyframe()}.
keyframe_create() ->
    ?nif_stub.


% Corresponds to SetGeoInfo function.
%-spec set_geo_info(_AssetId, _ObjectId, _GeoId) -> {atom(), hapi_geo_info()}.
set_geo_info(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.


% Corresponds to GeoInputInfo_Create function.
%-spec geo_input_info_create() -> {hapi_geo_input_info()}.
geo_input_info_create() ->
    ?nif_stub.


% Corresponds to MaterialInfo_Init function.
%-spec material_info_init() -> {atom(), hapi_material_info()}.
material_info_init() ->
    ?nif_stub.


% Corresponds to ParmInfo_Init function.
%-spec parm_info_init() -> {atom(), hapi_parm_info()}.
parm_info_init() ->
    ?nif_stub.


% Corresponds to GetAvailableAssets function.
%-spec get_available_assets(_LibraryId, _AssetCount) -> {atom(), list()}.
get_available_assets(_LibraryId, _AssetCount) ->
    ?nif_stub.


% Corresponds to GetMaterialOnPart function.
%-spec get_material_on_part(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_material_info()}.
get_material_on_part(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to CurveInfo_Init function.
%-spec curve_info_init() -> {atom(), hapi_curve_info()}.
curve_info_init() ->
    ?nif_stub.


% Corresponds to ParmInfo_IsNodePath function.
%-spec parm_info_is_node_path(_In) -> {boolean()}.
parm_info_is_node_path(_In) ->
    ?nif_stub.


% Corresponds to CookOptions_Create function.
%-spec cook_options_create() -> {hapi_cook_options()}.
cook_options_create() ->
    ?nif_stub.


% Corresponds to ExtractImageToFile function.
%-spec extract_image_to_file(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _DestinationFolderPath, _DestinationFileName) -> {atom(), integer()}.
extract_image_to_file(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _DestinationFolderPath, _DestinationFileName) ->
    ?nif_stub.


% Corresponds to SetTime function.
%-spec set_time(_Time) -> {atom()}.
set_time(_Time) ->
    ?nif_stub.


% Corresponds to GetImageInfo function.
%-spec get_image_info(_AssetId, _MaterialId) -> {atom(), hapi_image_info()}.
get_image_info(_AssetId, _MaterialId) ->
    ?nif_stub.


% Corresponds to GetParmStringValues function.
%-spec get_parm_string_values(_NodeId, _Evaluate, _Start, _Length) -> {atom(), list()}.
get_parm_string_values(_NodeId, _Evaluate, _Start, _Length) ->
    ?nif_stub.


% Corresponds to GetMaterialOnGroup function.
%-spec get_material_on_group(_AssetId, _ObjectId, _GeoId, _GroupName) -> {atom(), hapi_material_info()}.
get_material_on_group(_AssetId, _ObjectId, _GeoId, _GroupName) ->
    ?nif_stub.


% Corresponds to PythonThreadInterpreterLock function.
%-spec python_thread_interpreter_lock(_Locked) -> {atom()}.
python_thread_interpreter_lock(_Locked) ->
    ?nif_stub.


% Corresponds to SetTimelineOptions function.
%-spec set_timeline_options(_TimelineOptions) -> {atom()}.
set_timeline_options(_TimelineOptions) ->
    ?nif_stub.


% Corresponds to GetVolumeTileFloatData function.
%-spec get_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _PartId) -> {atom(), hapi_volume_tile_info(), list()}.
get_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.


% Corresponds to GetAttributeFloatData function.
%-spec get_attribute_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {atom(), hapi_attribute_info(), float()}.
get_attribute_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.


% Corresponds to ConnectAssetGeometry function.
%-spec connect_asset_geometry(_AssetIdFrom, _ObjectIdFrom, _AssetIdTo, _InputIdx) -> {atom()}.
connect_asset_geometry(_AssetIdFrom, _ObjectIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.


% Corresponds to ObjectInfo_Create function.
%-spec object_info_create() -> {hapi_object_info()}.
object_info_create() ->
    ?nif_stub.


% Corresponds to GetSupportedImageFileFormats function.
%-spec get_supported_image_file_formats(_FileFormatCount) -> {atom(), list()}.
get_supported_image_file_formats(_FileFormatCount) ->
    ?nif_stub.


% Corresponds to CheckForNewAssets function.
%-spec check_for_new_assets() -> {atom(), integer()}.
check_for_new_assets() ->
    ?nif_stub.

