-module(test_load_asset).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/hapi_records.hrl").

load_asset_library_file_test() ->
    % Initialize HAPI.
    ?assertEqual(hapi_result_success, hapi:initialize("", "", hapi:cook_options_create(), true, -1)),
    % Load test_dummyboxes HDA.
    {HapiStatus, _LibraryId} = hapi:load_asset_library_from_file("../otls/test_dummyboxes.otl", true),
    ?assertEqual(hapi_result_success, HapiStatus),
    % Clean up HAPI.
    ?assertEqual(hapi_result_success, hapi:cleanup()),
    ok.
