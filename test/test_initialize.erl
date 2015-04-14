-module(test_initialize).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/hapi_cook_options.hrl").

function_test() ->
    CookOptions = #hapi_cook_options{
        split_geos_by_group=true,
        max_vertices_per_primitive=3,
        refine_curve_to_linear=false,
        curve_refine_lod=1.0,
        clear_errors_and_warnings=false,
        cook_template_geos=false},
    ?assertEqual(hapi_result_success, hapi:initialize("", "", CookOptions, false, -1)),
    ok.
