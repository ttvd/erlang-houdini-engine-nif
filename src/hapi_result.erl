%%% @author Mykola Konyk <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% @license MS-RL
%%% HAPI_Result enum.

-module(hapi_result).
-version(1.9).


-type hapi_result() ::
    hapi_result_success |
    hapi_result_failure |
    hapi_result_already_initialized |
    hapi_result_not_initialized |
    hapi_result_cant_load_file |
    hapi_result_parm_set_failed |
    hapi_result_invalid_argument |
    hapi_result_cant_load_geo |
    hapi_result_cant_generate_preset |
    hapi_result_cant_load_asset |
    hapi_result_asset_def_already_loaded |
    hapi_result_no_license_found |
    hapi_result_disallowed_nc_license_found |
    hapi_result_disallowed_nc_asset_with_c_license |
    hapi_result_disallowed_nc_asset_with_lc_license |
    hapi_result_disallowed_lc_asset_with_c_license.

-export_type([hapi_result/0]).
-export([hapi_result_to_int/1, int_to_hapi_result/1]).

% Convert HAPI result to integer.
hapi_result_to_int(hapi_result_success) -> 0;
hapi_result_to_int(hapi_result_failure) -> 1;
hapi_result_to_int(hapi_result_already_initialized) -> 2;
hapi_result_to_int(hapi_result_not_initialized) -> 3;
hapi_result_to_int(hapi_result_cant_load_file) -> 4;
hapi_result_to_int(hapi_result_parm_set_failed) -> 5;
hapi_result_to_int(hapi_result_invalid_argument) -> 6;
hapi_result_to_int(hapi_result_cant_load_geo) -> 7;
hapi_result_to_int(hapi_result_cant_generate_preset) -> 8;
hapi_result_to_int(hapi_result_cant_load_asset) -> 9;
hapi_result_to_int(hapi_result_asset_def_already_loaded) -> 10;
hapi_result_to_int(hapi_result_no_license_found) -> 110;
hapi_result_to_int(hapi_result_disallowed_nc_license_found) -> 120;
hapi_result_to_int(hapi_result_disallowed_nc_asset_with_c_license) -> 130;
hapi_result_to_int(hapi_result_disallowed_nc_asset_with_lc_license) -> 140;
hapi_result_to_int(hapi_result_disallowed_lc_asset_with_c_license) -> 150.

% Convert integer to HAPI result.
int_to_hapi_result(0) -> hapi_result_success;
int_to_hapi_result(1) -> hapi_result_failure;
int_to_hapi_result(2) -> hapi_result_already_initialized;
int_to_hapi_result(3) -> hapi_result_not_initialized;
int_to_hapi_result(4) -> hapi_result_cant_load_file;
int_to_hapi_result(5) -> hapi_result_parm_set_failed;
int_to_hapi_result(6) -> hapi_result_invalid_argument;
int_to_hapi_result(7) -> hapi_result_cant_load_geo;
int_to_hapi_result(8) -> hapi_result_cant_generate_preset;
int_to_hapi_result(9) -> hapi_result_cant_load_asset;
int_to_hapi_result(10) -> hapi_result_asset_def_already_loaded;
int_to_hapi_result(110) -> hapi_result_no_license_found;
int_to_hapi_result(120) -> hapi_result_disallowed_nc_license_found;
int_to_hapi_result(130) -> hapi_result_disallowed_nc_asset_with_c_license;
int_to_hapi_result(140) -> hapi_result_disallowed_nc_asset_with_lc_license;
int_to_hapi_result(150) -> hapi_result_disallowed_lc_asset_with_c_license.
