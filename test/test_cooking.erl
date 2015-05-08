-module(test_cooking).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/hapi_records.hrl").

instantiate_and_cook_test() ->
    % Initialize HAPI.
    ?assertEqual(hapi_result_success, hapi:initialize("", "", hapi:cook_options_create(), true, -1)),
    % Load test_dummyboxes HDA.
    {HapiStatus, LibraryId} = hapi:load_asset_library_from_file("../otls/test_dummyboxes.otl", true),
    ?assertEqual(hapi_result_success, HapiStatus),
    % Get number of assets in loaded HDA.
    {HapiStatus, AssetCount} = hapi:get_available_asset_count(LibraryId),
    ?assertEqual(hapi_result_success, HapiStatus),
    ?assertEqual(1, AssetCount),
    % Get names of available assets in loaded HDA.
    {HapiStatus, [AssetNameId]} = hapi:get_available_assets(LibraryId, AssetCount),
    % Get length of retrieved asset name.
    {HapiStatus, AssetNameLength} = hapi:get_string_buf_length(AssetNameId),
    ?assertEqual(hapi_result_success, HapiStatus),
    ?assertEqual(29, AssetNameLength),
    % Retrieve name of the asset.
    {HapiStatus, AssetNameBinary} = hapi:get_string(AssetNameId, AssetNameLength),
    ?assertEqual(hapi_result_success, HapiStatus),
    % We need to convert binary to string.
    AssetName = binary_to_list(AssetNameBinary),
    % Instantiate the asset.
    {HapiStatus, AssetId} = hapi:instantiate_asset(AssetName, false),
    ?assertEqual(hapi_result_success, HapiStatus),
    % Cook the asset.
    ?assertEqual(hapi_result_success, hapi:cook_asset(AssetId, hapi:cook_options_create())),
    % Clean up HAPI.
    ?assertEqual(hapi_result_success, hapi:cleanup()),
    ok.
