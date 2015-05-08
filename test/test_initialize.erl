-module(test_initialize).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/hapi_records.hrl").

% Attempt to initialize hapi, check initialization status and then clean up.
initialize_true_test() ->
    ?assertEqual(hapi_result_success, hapi:initialize("", "", hapi:cook_options_create(), true, -1)),
    ?assertEqual(hapi_result_success, hapi:is_initialized()),
    ?assertEqual(hapi_result_success, hapi:cleanup()),
    ok.

% Create custom cook options record and use it for initialization.
initialize_record_cookoptions_test() ->
    CookOptions = #hapi_cook_options{
        split_geos_by_group=true,
        max_vertices_per_primitive=3,
        refine_curve_to_linear=false,
        curve_refine_lod=1.0,
        clear_errors_and_warnings=false,
        cook_templated_geos=false},
    ?assertEqual(hapi_result_success, hapi:initialize("", "", CookOptions, true, -1)),
    ?assertEqual(hapi_result_success, hapi:is_initialized()),
    ?assertEqual(hapi_result_success, hapi:cleanup()),
    ok.

% Attempt to check if hapi is initialized when hapi has not been initialized.
initialize_check_false_test() ->
    ?assertEqual(hapi_result_not_initialized, hapi:is_initialized()),
    ok.
