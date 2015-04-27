-module(test_initialize).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/records/hapi_cookoptions.hrl").

nil_test() ->
    %?assertEqual(hapi_result_success, hapi:initialize(nil, nil, nil, false, -1)),
    %?assertEqual(hapi_result_success, hapi:is_initialized()),
    %?assertEqual(hapi_result_success, hapi:cleanup()),
    ok.

record_test() ->
    %CookOptions = #hapi_cookoptions{
    %    split_geos_by_group=true,
    %    max_vertices_per_primitive=3,
    %    refine_curve_to_linear=false,
    %    curve_refine_lod=1.0,
    %    clear_errors_and_warnings=false,
    %    cook_template_geos=false},
    %?assertEqual(hapi_result_success, hapi:initialize(nil, nil, CookOptions, false, -1)),
    %?assertEqual(hapi_result_success, hapi:is_initialized()),
    %?assertEqual(hapi_result_success, hapi:cleanup()),
    ok.
