%%% @author Mykola Konyk <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% @license MS-RL
%%% This file is autogenerated from util/hapi_record.hrl.template
%%% This file corresponds to HAPI_GeoInfo structure from HAPI_Common.h

-record(hapi_geoinfo,
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