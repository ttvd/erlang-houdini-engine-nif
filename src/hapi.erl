%%% @author Mykola Konyk <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% This file has been auto-generated from hapi.erl.template

-module(hapi).
-version(1.9).

-on_load(init/0).
-define(nif_stub, nif_stub_error(?LINE)).

% Generated records.
-include("hapi_records.hrl").

% Generated enum types and translation functions.
-include("hapi_enums.hrl").

% Generated exports.
-export([
    is_initialized/0,
    initialize/5,
    cleanup/0,
    get_env_int/1,
    get_status/1,
    get_status_string_buf_length/2,
    get_status_string/1,
    get_cooking_total_count/0,
    get_cooking_current_count/0,
    convert_transform/2,
    convert_matrix_to_quat/1,
    convert_matrix_to_euler/2,
    convert_transform_quat_to_matrix/1,
    convert_transform_euler_to_matrix/1,
    python_thread_interpreter_lock/1,
    get_string_buf_length/1,
    get_string/2,
    get_time/0,
    set_time/1,
    get_timeline_options/0,
    set_timeline_options/1,
    is_asset_valid/2,
    load_asset_library_from_file/2,
    load_asset_library_from_memory/3,
    get_available_asset_count/1,
    get_available_assets/2,
    instantiate_asset/2,
    create_curve/0,
    create_input_asset/1,
    destroy_asset/1,
    get_asset_info/1,
    cook_asset/2,
    interrupt/0,
    get_asset_transform/3,
    set_asset_transform/1,
    get_input_name/3,
    load_hip_file/2,
    check_for_new_assets/0,
    get_new_asset_ids/0,
    save_hip_file/1,
    get_node_info/1,
    get_global_nodes/0,
    get_parameters/3,
    get_parm_info/2,
    get_parm_id_from_name/2,
    get_parm_info_from_name/2,
    get_parm_int_value/3,
    get_parm_int_values/3,
    get_parm_float_value/3,
    get_parm_float_values/3,
    get_parm_string_value/4,
    get_parm_string_values/4,
    get_parm_choice_lists/3,
    set_parm_int_value/4,
    set_parm_int_values/4,
    set_parm_float_value/4,
    set_parm_float_values/4,
    set_parm_string_value/4,
    insert_multiparm_instance/3,
    remove_multiparm_instance/3,
    get_handle_info/3,
    get_handle_binding_info/4,
    get_preset_buf_length/3,
    get_preset/2,
    set_preset/5,
    get_objects/3,
    get_object_transforms/4,
    get_instance_transforms/6,
    set_object_transform/3,
    get_geo_info/3,
    get_part_info/4,
    get_face_counts/6,
    get_vertex_list/6,
    get_attribute_info/6,
    get_attribute_names/6,
    get_attribute_int_data/7,
    get_attribute_float_data/7,
    get_attribute_string_data/7,
    get_group_names/5,
    get_group_membership/8,
    set_geo_info/3,
    set_part_info/4,
    set_face_counts/6,
    set_vertex_list/6,
    add_attribute/5,
    set_attribute_int_data/8,
    set_attribute_float_data/8,
    set_attribute_string_data/8,
    add_group/5,
    set_group_membership/7,
    commit_geo/3,
    revert_geo/3,
    connect_asset_transform/3,
    disconnect_asset_transform/2,
    connect_asset_geometry/4,
    disconnect_asset_geometry/2,
    get_material_ids_on_faces/6,
    get_material_info/2,
    render_material_to_image/3,
    render_texture_to_image/3,
    get_supported_image_file_format_count/0,
    get_supported_image_file_formats/1,
    get_image_info/2,
    set_image_info/3,
    get_image_plane_count/2,
    get_image_planes/3,
    extract_image_to_file/6,
    extract_image_to_memory/4,
    get_image_memory_buffer/3,
    set_anim_curve/5,
    set_transform_anim_curve/4,
    reset_simulation/1,
    get_volume_info/4,
    get_first_volume_tile/4,
    get_next_volume_tile/4,
    get_volume_tile_float_data/4,
    get_volume_tile_int_data/4,
    set_volume_info/4,
    set_volume_tile_float_data/5,
    set_volume_tile_int_data/5,
    get_curve_info/4,
    get_curve_counts/6,
    get_curve_orders/6,
    get_curve_knots/6,
    set_curve_info/5,
    set_curve_counts/7,
    set_curve_orders/7,
    set_curve_knots/7,
    save_geo_to_file/4,
    load_geo_from_file/4,
    get_geo_size/4,
    save_geo_to_memory/4,
    load_geo_from_memory/5,
    hash_enum_value/1,
    check_enum_value_hash/2
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

% Helper function which returns hash of an atom.
hash_enum_value(_Atom) ->
    ?nif_stub.

% Helper function which checks whether atom has a given hash.
check_enum_value_hash(_Atom, _Hash) ->
    ?nif_stub.

%
%-spec is_initialized() -> hapi_result().
is_initialized() ->
    ?nif_stub.

%
%-spec initialize(_OtlSearchPath, _DsoSearchPath, _CookOptions, _UseCookingThread, _CookingThreadStackSize) -> hapi_result().
initialize(_OtlSearchPath, _DsoSearchPath, _CookOptions, _UseCookingThread, _CookingThreadStackSize) ->
    ?nif_stub.

%
%-spec cleanup() -> hapi_result().
cleanup() ->
    ?nif_stub.

%
%-spec get_env_int(_IntType) -> {hapi_result(), _Value}.
get_env_int(_IntType) ->
    ?nif_stub.

%
%-spec get_status(_StatusType) -> {hapi_result(), _Status}.
get_status(_StatusType) ->
    ?nif_stub.

%
%-spec get_status_string_buf_length(_StatusType, _Verbosity) -> {hapi_result(), _BufferSize}.
get_status_string_buf_length(_StatusType, _Verbosity) ->
    ?nif_stub.

%
%-spec get_status_string(_StatusType) -> {hapi_result(), _Buffer}.
get_status_string(_StatusType) ->
    ?nif_stub.

%
%-spec get_cooking_total_count() -> {hapi_result(), _Count}.
get_cooking_total_count() ->
    ?nif_stub.

%
%-spec get_cooking_current_count() -> {hapi_result(), _Count}.
get_cooking_current_count() ->
    ?nif_stub.

%
%-spec convert_transform(_RstOrder, _RotOrder) -> {hapi_result(), _TransformInOut}.
convert_transform(_RstOrder, _RotOrder) ->
    ?nif_stub.

%
%-spec convert_matrix_to_quat(_RstOrder) -> {hapi_result(), _Mat, _TransformOut}.
convert_matrix_to_quat(_RstOrder) ->
    ?nif_stub.

%
%-spec convert_matrix_to_euler(_RstOrder, _RotOrder) -> {hapi_result(), _Mat, _TransformOut}.
convert_matrix_to_euler(_RstOrder, _RotOrder) ->
    ?nif_stub.

%
%-spec convert_transform_quat_to_matrix(_Transform) -> {hapi_result(), _Matrix}.
convert_transform_quat_to_matrix(_Transform) ->
    ?nif_stub.

%
%-spec convert_transform_euler_to_matrix(_Transform) -> {hapi_result(), _Matrix}.
convert_transform_euler_to_matrix(_Transform) ->
    ?nif_stub.

%
%-spec python_thread_interpreter_lock(_Locked) -> hapi_result().
python_thread_interpreter_lock(_Locked) ->
    ?nif_stub.

%
%-spec get_string_buf_length(_StringHandle) -> {hapi_result(), _BufferLength}.
get_string_buf_length(_StringHandle) ->
    ?nif_stub.

%
%-spec get_string(_StringHandle, _BufferLength) -> {hapi_result(), _StringValue}.
get_string(_StringHandle, _BufferLength) ->
    ?nif_stub.

%
%-spec get_time() -> {hapi_result(), _Time}.
get_time() ->
    ?nif_stub.

%
%-spec set_time(_Time) -> hapi_result().
set_time(_Time) ->
    ?nif_stub.

%
%-spec get_timeline_options() -> {hapi_result(), _TimelineOptions}.
get_timeline_options() ->
    ?nif_stub.

%
%-spec set_timeline_options(_TimelineOptions) -> hapi_result().
set_timeline_options(_TimelineOptions) ->
    ?nif_stub.

%
%-spec is_asset_valid(_AssetId, _AssetValidationId) -> {hapi_result(), _Answer}.
is_asset_valid(_AssetId, _AssetValidationId) ->
    ?nif_stub.

%
%-spec load_asset_library_from_file(_FilePath, _AllowOverwrite) -> {hapi_result(), _LibraryId}.
load_asset_library_from_file(_FilePath, _AllowOverwrite) ->
    ?nif_stub.

%
%-spec load_asset_library_from_memory(_LibraryBuffer, _LibraryBufferSize, _AllowOverwrite) -> {hapi_result(), _LibraryId}.
load_asset_library_from_memory(_LibraryBuffer, _LibraryBufferSize, _AllowOverwrite) ->
    ?nif_stub.

%
%-spec get_available_asset_count(_LibraryId) -> {hapi_result(), _AssetCount}.
get_available_asset_count(_LibraryId) ->
    ?nif_stub.

%
%-spec get_available_assets(_LibraryId, _AssetCount) -> {hapi_result(), _AssetNames}.
get_available_assets(_LibraryId, _AssetCount) ->
    ?nif_stub.

%
%-spec instantiate_asset(_AssetName, _CookOnLoad) -> {hapi_result(), _AssetId}.
instantiate_asset(_AssetName, _CookOnLoad) ->
    ?nif_stub.

%
%-spec create_curve() -> {hapi_result(), _AssetId}.
create_curve() ->
    ?nif_stub.

%
%-spec create_input_asset(_Name) -> {hapi_result(), _AssetId}.
create_input_asset(_Name) ->
    ?nif_stub.

%
%-spec destroy_asset(_AssetId) -> hapi_result().
destroy_asset(_AssetId) ->
    ?nif_stub.

%
%-spec get_asset_info(_AssetId) -> {hapi_result(), _AssetInfo}.
get_asset_info(_AssetId) ->
    ?nif_stub.

%
%-spec cook_asset(_AssetId, _CookOptions) -> hapi_result().
cook_asset(_AssetId, _CookOptions) ->
    ?nif_stub.

%
%-spec interrupt() -> hapi_result().
interrupt() ->
    ?nif_stub.

%
%-spec get_asset_transform(_AssetId, _RstOrder, _RotOrder) -> {hapi_result(), _Transform}.
get_asset_transform(_AssetId, _RstOrder, _RotOrder) ->
    ?nif_stub.

%
%-spec set_asset_transform(_AssetId) -> {hapi_result(), _Transform}.
set_asset_transform(_AssetId) ->
    ?nif_stub.

%
%-spec get_input_name(_AssetId, _InputIdx, _InputType) -> {hapi_result(), _Name}.
get_input_name(_AssetId, _InputIdx, _InputType) ->
    ?nif_stub.

%
%-spec load_hip_file(_FileName, _CookOnLoad) -> hapi_result().
load_hip_file(_FileName, _CookOnLoad) ->
    ?nif_stub.

%
%-spec check_for_new_assets() -> {hapi_result(), _NewAssetCount}.
check_for_new_assets() ->
    ?nif_stub.

%
%-spec get_new_asset_ids() -> {hapi_result(), _AssetIds}.
get_new_asset_ids() ->
    ?nif_stub.

%
%-spec save_hip_file(_FilePath) -> hapi_result().
save_hip_file(_FilePath) ->
    ?nif_stub.

%
%-spec get_node_info(_NodeId) -> {hapi_result(), _NodeInfo}.
get_node_info(_NodeId) ->
    ?nif_stub.

%
%-spec get_global_nodes() -> {hapi_result(), _GlobalNodes}.
get_global_nodes() ->
    ?nif_stub.

%
%-spec get_parameters(_NodeId, _Start, _Length) -> {hapi_result(), _ParmInfos}.
get_parameters(_NodeId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_parm_info(_NodeId, _ParmId) -> {hapi_result(), _ParmInfo}.
get_parm_info(_NodeId, _ParmId) ->
    ?nif_stub.

%
%-spec get_parm_id_from_name(_NodeId, _ParmName) -> {hapi_result(), _ParmId}.
get_parm_id_from_name(_NodeId, _ParmName) ->
    ?nif_stub.

%
%-spec get_parm_info_from_name(_NodeId, _ParmName) -> {hapi_result(), _ParmInfo}.
get_parm_info_from_name(_NodeId, _ParmName) ->
    ?nif_stub.

%
%-spec get_parm_int_value(_NodeId, _ParmName, _Index) -> {hapi_result(), _Value}.
get_parm_int_value(_NodeId, _ParmName, _Index) ->
    ?nif_stub.

%
%-spec get_parm_int_values(_NodeId, _Start, _Length) -> {hapi_result(), _Values}.
get_parm_int_values(_NodeId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_parm_float_value(_NodeId, _ParmName, _Index) -> {hapi_result(), _Value}.
get_parm_float_value(_NodeId, _ParmName, _Index) ->
    ?nif_stub.

%
%-spec get_parm_float_values(_NodeId, _Start, _Length) -> {hapi_result(), _Values}.
get_parm_float_values(_NodeId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_parm_string_value(_NodeId, _ParmName, _Index, _Evaluate) -> {hapi_result(), _Value}.
get_parm_string_value(_NodeId, _ParmName, _Index, _Evaluate) ->
    ?nif_stub.

%
%-spec get_parm_string_values(_NodeId, _Evaluate, _Start, _Length) -> {hapi_result(), _Values}.
get_parm_string_values(_NodeId, _Evaluate, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_parm_choice_lists(_NodeId, _Start, _Length) -> {hapi_result(), _ParmChoices}.
get_parm_choice_lists(_NodeId, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_parm_int_value(_NodeId, _ParmName, _Index, _Value) -> hapi_result().
set_parm_int_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
%-spec set_parm_int_values(_NodeId, _Values, _Start, _Length) -> hapi_result().
set_parm_int_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_parm_float_value(_NodeId, _ParmName, _Index, _Value) -> hapi_result().
set_parm_float_value(_NodeId, _ParmName, _Index, _Value) ->
    ?nif_stub.

%
%-spec set_parm_float_values(_NodeId, _Values, _Start, _Length) -> hapi_result().
set_parm_float_values(_NodeId, _Values, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_parm_string_value(_NodeId, _Value, _ParmId, _Index) -> hapi_result().
set_parm_string_value(_NodeId, _Value, _ParmId, _Index) ->
    ?nif_stub.

%
%-spec insert_multiparm_instance(_NodeId, _ParmId, _InstancePosition) -> hapi_result().
insert_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.

%
%-spec remove_multiparm_instance(_NodeId, _ParmId, _InstancePosition) -> hapi_result().
remove_multiparm_instance(_NodeId, _ParmId, _InstancePosition) ->
    ?nif_stub.

%
%-spec get_handle_info(_AssetId, _Start, _Length) -> {hapi_result(), _HandleInfos}.
get_handle_info(_AssetId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_handle_binding_info(_AssetId, _HandleIndex, _Start, _Length) -> {hapi_result(), _HandleInfos}.
get_handle_binding_info(_AssetId, _HandleIndex, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_preset_buf_length(_NodeId, _PresetType, _PresetName) -> {hapi_result(), _BufferLength}.
get_preset_buf_length(_NodeId, _PresetType, _PresetName) ->
    ?nif_stub.

%
%-spec get_preset(_NodeId, _BufferLength) -> {hapi_result(), _Buffer}.
get_preset(_NodeId, _BufferLength) ->
    ?nif_stub.

%
%-spec set_preset(_NodeId, _PresetType, _PresetName, _Buffer, _BufferLength) -> hapi_result().
set_preset(_NodeId, _PresetType, _PresetName, _Buffer, _BufferLength) ->
    ?nif_stub.

%
%-spec get_objects(_AssetId, _Start, _Length) -> {hapi_result(), _ObjectInfos}.
get_objects(_AssetId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_object_transforms(_AssetId, _RstOrder, _Start, _Length) -> {hapi_result(), _Transforms}.
get_object_transforms(_AssetId, _RstOrder, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_instance_transforms(_AssetId, _ObjectId, _GeoId, _RstOrder, _Start, _Length) -> {hapi_result(), _Transforms}.
get_instance_transforms(_AssetId, _ObjectId, _GeoId, _RstOrder, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_object_transform(_AssetId, _ObjectId, _Transform) -> hapi_result().
set_object_transform(_AssetId, _ObjectId, _Transform) ->
    ?nif_stub.

%
%-spec get_geo_info(_AssetId, _ObjectId, _GeoId) -> {hapi_result(), _GeoInfo}.
get_geo_info(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.

%
%-spec get_part_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {hapi_result(), _PartInfo}.
get_part_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.

%
%-spec get_face_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {hapi_result(), _FaceCounts}.
get_face_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_vertex_list(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {hapi_result(), _VertexList}.
get_vertex_list(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_attribute_info(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Owner) -> {hapi_result(), _AttrInfo}.
get_attribute_info(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Owner) ->
    ?nif_stub.

%
%-spec get_attribute_names(_AssetId, _ObjectId, _GeoId, _PartId, _Owner, _Count) -> {hapi_result(), _AttributeNames}.
get_attribute_names(_AssetId, _ObjectId, _GeoId, _PartId, _Owner, _Count) ->
    ?nif_stub.

%
%-spec get_attribute_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {hapi_result(), _AttrInfo, _Data}.
get_attribute_int_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_attribute_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {hapi_result(), _AttrInfo, _Data}.
get_attribute_float_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_attribute_string_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) -> {hapi_result(), _AttrInfo, _Data}.
get_attribute_string_data(_AssetId, _ObjectId, _GeoId, _PartId, _Name, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_group_names(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupCount) -> {hapi_result(), _GroupNames}.
get_group_names(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupCount) ->
    ?nif_stub.

%
%-spec get_group_membership(_AssetId, _ObjectId, _GeoId, _PartId, _GroupType, _GroupName, _Start, _Length) -> {hapi_result(), _Membership}.
get_group_membership(_AssetId, _ObjectId, _GeoId, _PartId, _GroupType, _GroupName, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_geo_info(_AssetId, _ObjectId, _GeoId) -> {hapi_result(), _GeoInfo}.
set_geo_info(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.

%
%-spec set_part_info(_AssetId, _ObjectId, _GeoId, _PartInfo) -> hapi_result().
set_part_info(_AssetId, _ObjectId, _GeoId, _PartInfo) ->
    ?nif_stub.

%
%-spec set_face_counts(_AssetId, _ObjectId, _GeoId, _FaceCounts, _Start, _Length) -> hapi_result().
set_face_counts(_AssetId, _ObjectId, _GeoId, _FaceCounts, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_vertex_list(_AssetId, _ObjectId, _GeoId, _VertexList, _Start, _Length) -> hapi_result().
set_vertex_list(_AssetId, _ObjectId, _GeoId, _VertexList, _Start, _Length) ->
    ?nif_stub.

%
%-spec add_attribute(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo) -> hapi_result().
add_attribute(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo) ->
    ?nif_stub.

%
%-spec set_attribute_int_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) -> hapi_result().
set_attribute_int_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_attribute_float_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) -> hapi_result().
set_attribute_float_data(_AssetId, _ObjectId, _GeoId, _Name, _AttrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_attribute_string_data(_AssetId, _ObjectId, _GeoId, _Name, _attrInfo, _Data, _Start, _Length) -> hapi_result().
set_attribute_string_data(_AssetId, _ObjectId, _GeoId, _Name, _attrInfo, _Data, _Start, _Length) ->
    ?nif_stub.

%
%-spec add_group(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName) -> hapi_result().
add_group(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName) ->
    ?nif_stub.

%
%-spec set_group_membership(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName, _Start, _Length) -> {hapi_result(), _Membership}.
set_group_membership(_AssetId, _ObjectId, _GeoId, _GroupType, _GroupName, _Start, _Length) ->
    ?nif_stub.

%
%-spec commit_geo(_AssetId, _ObjectId, _GeoId) -> hapi_result().
commit_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.

%
%-spec revert_geo(_AssetId, _ObjectId, _GeoId) -> hapi_result().
revert_geo(_AssetId, _ObjectId, _GeoId) ->
    ?nif_stub.

%
%-spec connect_asset_transform(_AssetIdFrom, _AssetIdTo, _InputIdx) -> hapi_result().
connect_asset_transform(_AssetIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.

%
%-spec disconnect_asset_transform(_AssetId, _InputIdx) -> hapi_result().
disconnect_asset_transform(_AssetId, _InputIdx) ->
    ?nif_stub.

%
%-spec connect_asset_geometry(_AssetIdFrom, _ObjectIdFrom, _AssetIdTo, _InputIdx) -> hapi_result().
connect_asset_geometry(_AssetIdFrom, _ObjectIdFrom, _AssetIdTo, _InputIdx) ->
    ?nif_stub.

%
%-spec disconnect_asset_geometry(_AssetId, _InputIdx) -> hapi_result().
disconnect_asset_geometry(_AssetId, _InputIdx) ->
    ?nif_stub.

%
%-spec get_material_ids_on_faces(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {hapi_result(), _AreAllTheSame, _MaterialIds}.
get_material_ids_on_faces(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_material_info(_AssetId, _MaterialId) -> {hapi_result(), _MaterialInfo}.
get_material_info(_AssetId, _MaterialId) ->
    ?nif_stub.

%
%-spec render_material_to_image(_AssetId, _MaterialId, _ShaderType) -> hapi_result().
render_material_to_image(_AssetId, _MaterialId, _ShaderType) ->
    ?nif_stub.

%
%-spec render_texture_to_image(_AssetId, _MaterialId, _ParmId) -> hapi_result().
render_texture_to_image(_AssetId, _MaterialId, _ParmId) ->
    ?nif_stub.

%
%-spec get_supported_image_file_format_count() -> {hapi_result(), _FileFormatCount}.
get_supported_image_file_format_count() ->
    ?nif_stub.

%
%-spec get_supported_image_file_formats(_FileFormatCount) -> {hapi_result(), _formats}.
get_supported_image_file_formats(_FileFormatCount) ->
    ?nif_stub.

%
%-spec get_image_info(_AssetId, _MaterialId) -> {hapi_result(), _ImageInfo}.
get_image_info(_AssetId, _MaterialId) ->
    ?nif_stub.

%
%-spec set_image_info(_AssetId, _MaterialId, _ImageInfo) -> hapi_result().
set_image_info(_AssetId, _MaterialId, _ImageInfo) ->
    ?nif_stub.

%
%-spec get_image_plane_count(_AssetId, _MaterialId) -> {hapi_result(), _ImagePlaneCount}.
get_image_plane_count(_AssetId, _MaterialId) ->
    ?nif_stub.

%
%-spec get_image_planes(_AssetId, _MaterialId, _ImagePlaneCount) -> {hapi_result(), _ImagePlanes}.
get_image_planes(_AssetId, _MaterialId, _ImagePlaneCount) ->
    ?nif_stub.

%
%-spec extract_image_to_file(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _DestinationFolderPath, _DestinationFileName) -> {hapi_result(), _DestinationFilePath}.
extract_image_to_file(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes, _DestinationFolderPath, _DestinationFileName) ->
    ?nif_stub.

%
%-spec extract_image_to_memory(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes) -> {hapi_result(), _BufferSize}.
extract_image_to_memory(_AssetId, _MaterialId, _ImageFileFormatName, _ImagePlanes) ->
    ?nif_stub.

%
%-spec get_image_memory_buffer(_AssetId, _MaterialId, _BufferSize) -> {hapi_result(), _Buffer}.
get_image_memory_buffer(_AssetId, _MaterialId, _BufferSize) ->
    ?nif_stub.

%
%-spec set_anim_curve(_NodeId, _ParmId, _ParmIndex, _CurveKeyframes, _KeyframeCount) -> hapi_result().
set_anim_curve(_NodeId, _ParmId, _ParmIndex, _CurveKeyframes, _KeyframeCount) ->
    ?nif_stub.

%
%-spec set_transform_anim_curve(_NodeId, _TransComp, _curveKeyframes, _KeyframeCount) -> hapi_result().
set_transform_anim_curve(_NodeId, _TransComp, _curveKeyframes, _KeyframeCount) ->
    ?nif_stub.

%
%-spec reset_simulation(_AssetId) -> hapi_result().
reset_simulation(_AssetId) ->
    ?nif_stub.

%
%-spec get_volume_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {hapi_result(), _VolumeInfo}.
get_volume_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.

%
%-spec get_first_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) -> {hapi_result(), _Tile}.
get_first_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.

%
%-spec get_next_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) -> {hapi_result(), _Next}.
get_next_volume_tile(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.

%
%-spec get_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _PartId) -> {hapi_result(), _Tile, _Values}.
get_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.

%
%-spec get_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _PartId) -> {hapi_result(), _Tile, _Values}.
get_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.

%
%-spec set_volume_info(_AssetId, _ObjectId, _GeoId, _VolumeInfo) -> hapi_result().
set_volume_info(_AssetId, _ObjectId, _GeoId, _VolumeInfo) ->
    ?nif_stub.

%
%-spec set_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) -> hapi_result().
set_volume_tile_float_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.

%
%-spec set_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) -> hapi_result().
set_volume_tile_int_data(_AssetId, _ObjectId, _GeoId, _Tile, _Values) ->
    ?nif_stub.

%
%-spec get_curve_info(_AssetId, _ObjectId, _GeoId, _PartId) -> {hapi_result(), _Info}.
get_curve_info(_AssetId, _ObjectId, _GeoId, _PartId) ->
    ?nif_stub.

%
%-spec get_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {hapi_result(), _Counts}.
get_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {hapi_result(), _Orders}.
get_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.

%
%-spec get_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) -> {hapi_result(), _Knots}.
get_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) -> hapi_result().
set_curve_info(_AssetId, _ObjectId, _GeoId, _PartId, _Info) ->
    ?nif_stub.

%
%-spec set_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) -> hapi_result().
set_curve_counts(_AssetId, _ObjectId, _GeoId, _PartId, _Counts, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) -> hapi_result().
set_curve_orders(_AssetId, _ObjectId, _GeoId, _PartId, _Orders, _Start, _Length) ->
    ?nif_stub.

%
%-spec set_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) -> hapi_result().
set_curve_knots(_AssetId, _ObjectId, _GeoId, _PartId, _Knots, _Start, _Length) ->
    ?nif_stub.

%
%-spec save_geo_to_file(_AssetId, _ObjectId, _GeoId, _FileName) -> hapi_result().
save_geo_to_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.

%
%-spec load_geo_from_file(_AssetId, _ObjectId, _GeoId, _FileName) -> hapi_result().
load_geo_from_file(_AssetId, _ObjectId, _GeoId, _FileName) ->
    ?nif_stub.

%
%-spec get_geo_size(_AssetId, _ObjectId, _GeoId, _Format) -> {hapi_result(), _Size}.
get_geo_size(_AssetId, _ObjectId, _GeoId, _Format) ->
    ?nif_stub.

%
%-spec save_geo_to_memory(_AssetId, _ObjectId, _GeoId, _Size) -> {hapi_result(), _Buffer}.
save_geo_to_memory(_AssetId, _ObjectId, _GeoId, _Size) ->
    ?nif_stub.

%
%-spec load_geo_from_memory(_AssetId, _ObjectId, _GeoId, _Format, _Size) -> {hapi_result(), _Buffer}.
load_geo_from_memory(_AssetId, _ObjectId, _GeoId, _Format, _Size) ->
    ?nif_stub.
