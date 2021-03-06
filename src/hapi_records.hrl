%%% @author Mykola Konyk <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% @license MS-RL
%%% This file is autogenerated from util/hapi_records.hrl.template


% Corresponds to HAPI_CookOptions structure.
-record(hapi_cook_options,
{
    split_geos_by_group,
    max_vertices_per_primitive,
    refine_curve_to_linear,
    curve_refine_lod,
    clear_errors_and_warnings,
    cook_templated_geos
}).


% Corresponds to HAPI_Keyframe structure.
-record(hapi_keyframe,
{
    time,
    value,
    in_tangent,
    out_tangent
}).


% Corresponds to HAPI_PartInfo structure.
-record(hapi_part_info,
{
    id,
    name_sh,
    face_count,
    vertex_count,
    point_count,
    point_attribute_count,
    face_attribute_count,
    vertex_attribute_count,
    detail_attribute_count,
    has_volume,
    is_curve
}).


% Corresponds to HAPI_MaterialInfo structure.
-record(hapi_material_info,
{
    id,
    asset_id,
    node_id,
    exists,
    has_changed
}).


% Corresponds to HAPI_GlobalNodes structure.
-record(hapi_global_nodes,
{
    default_camera,
    default_light,
    mantra_renderer
}).


% Corresponds to HAPI_HandleBindingInfo structure.
-record(hapi_handle_binding_info,
{
    handle_parm_name_sh,
    asset_parm_name_sh,
    asset_parm_id
}).


% Corresponds to HAPI_ImageInfo structure.
-record(hapi_image_info,
{
    image_file_format_name_sh,
    x_res,
    y_res,
    data_format,
    interleaved,
    packing,
    gamma
}).


% Corresponds to HAPI_VolumeInfo structure.
-record(hapi_volume_info,
{
    name_sh,
    x_length,
    y_length,
    z_length,
    min_x,
    min_y,
    min_z,
    tuple_size,
    storage,
    tile_size,
    transform,
    has_taper,
    x_taper,
    y_taper
}).


% Corresponds to HAPI_GeoInputInfo structure.
-record(hapi_geo_input_info,
{
    object_id,
    geo_id,
    object_node_id
}).


% Corresponds to HAPI_VolumeTileInfo structure.
-record(hapi_volume_tile_info,
{
    min_x,
    min_y,
    min_z,
    is_valid
}).


% Corresponds to HAPI_CurveInfo structure.
-record(hapi_curve_info,
{
    curve_type,
    curve_count,
    vertex_count,
    knot_count,
    is_periodic,
    is_rational,
    order,
    has_knots
}).


% Corresponds to HAPI_Transform structure.
-record(hapi_transform,
{
    position,
    rotation_quaternion,
    scale,
    rst_order
}).


% Corresponds to HAPI_ImageFileFormat structure.
-record(hapi_image_file_format,
{
    name_sh,
    description_sh,
    default_extension_sh
}).


% Corresponds to HAPI_TransformEuler structure.
-record(hapi_transform_euler,
{
    position,
    rotation_euler,
    scale,
    rotation_order,
    rst_order
}).


% Corresponds to HAPI_AttributeInfo structure.
-record(hapi_attribute_info,
{
    exists,
    owner,
    storage,
    original_owner,
    count,
    tuple_size
}).


% Corresponds to HAPI_GeoInfo structure.
-record(hapi_geo_info,
{
    id,
    type,
    name_sh,
    node_id,
    is_editable,
    is_templated,
    is_display_geo,
    has_geo_changed,
    has_material_changed,
    point_group_count,
    primitive_group_count,
    part_count
}).


% Corresponds to HAPI_AssetInfo structure.
-record(hapi_asset_info,
{
    id,
    type,
    sub_type,
    validation_id,
    node_id,
    object_node_id,
    has_ever_cooked,
    name_sh,
    label_sh,
    file_path_sh,
    version_sh,
    full_op_name_sh,
    help_text_sh,
    object_count,
    handle_count,
    transform_input_count,
    geo_input_count,
    have_objects_changed,
    have_materials_changed
}).


% Corresponds to HAPI_ParmChoiceInfo structure.
-record(hapi_parm_choice_info,
{
    parent_parm_id,
    label_sh,
    value_sh
}).


% Corresponds to HAPI_ObjectInfo structure.
-record(hapi_object_info,
{
    id,
    name_sh,
    object_instance_path_sh,
    has_transform_changed,
    have_geos_changed,
    is_visible,
    is_instancer,
    geo_count,
    node_id,
    object_to_instance_id
}).


% Corresponds to HAPI_HandleInfo structure.
-record(hapi_handle_info,
{
    name_sh,
    type_name_sh,
    bindings_count
}).


% Corresponds to HAPI_TimelineOptions structure.
-record(hapi_timeline_options,
{
    fps,
    start_time,
    end_time
}).


% Corresponds to HAPI_NodeInfo structure.
-record(hapi_node_info,
{
    id,
    asset_id,
    name_sh,
    is_valid,
    total_cook_count,
    unique_houdini_node_id,
    internal_node_path_sh,
    parm_count,
    parm_int_value_count,
    parm_float_value_count,
    parm_string_value_count,
    parm_choice_count
}).


% Corresponds to HAPI_ParmInfo structure.
-record(hapi_parm_info,
{
    id,
    parent_id,
    type,
    type_info_sh,
    permissions,
    size,
    choice_count,
    name_sh,
    label_sh,
    template_name_sh,
    help_sh,
    has_min,
    has_max,
    has_uimin,
    has_uimax,
    min,
    max,
    uimin,
    uimax,
    invisible,
    disabled,
    spare,
    join_next,
    label_none,
    int_values_index,
    float_values_index,
    string_values_index,
    choice_index,
    is_child_of_multi_parm,
    instance_num,
    instance_length,
    instance_count,
    instance_start_offset,
    ramp_type
}).

