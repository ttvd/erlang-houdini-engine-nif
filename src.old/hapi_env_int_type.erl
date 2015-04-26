%%% @author Mykola Konyk <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% @license MS-RL
%%% HAPI_EnvIntType enum.

-module(hapi_env_int_type).
-version(1.9).


-type hapi_env_int_type() ::
    hapi_env_int_invalid |
    hapi_env_int_version_houdini_major |
    hapi_env_int_version_houdini_minor |
    hapi_env_int_version_houdini_build |
    hapi_env_int_version_houdini_patch |
    hapi_env_int_version_orig_houdini_major |
    hapi_env_int_version_orig_houdini_minor |
    hapi_env_int_version_orig_houdini_build |
    hapi_env_int_version_orig_houdini_patch |
    hapi_env_int_version_houdini_engine_major |
    hapi_env_int_version_houdini_engine_minor |
    hapi_env_int_version_houdini_engine_api |
    hapi_env_int_license |
    hapi_env_int_max.


-export_type([hapi_env_int_type/0]).
-export([hapi_env_int_to_int/1, int_to_hapi_env_int/1]).

% Convert HAPI env int to integer.
hapi_env_int_to_int(hapi_env_int_invalid) -> -1;
hapi_env_int_to_int(hapi_env_int_version_houdini_major) -> 0;
hapi_env_int_to_int(hapi_env_int_version_houdini_minor) -> 1;
hapi_env_int_to_int(hapi_env_int_version_houdini_build) -> 2;
hapi_env_int_to_int(hapi_env_int_version_houdini_patch) -> 3;
hapi_env_int_to_int(hapi_env_int_version_orig_houdini_major) -> 4;
hapi_env_int_to_int(hapi_env_int_version_orig_houdini_minor) -> 5;
hapi_env_int_to_int(hapi_env_int_version_orig_houdini_build) -> 6;
hapi_env_int_to_int(hapi_env_int_version_orig_houdini_patch) -> 7;
hapi_env_int_to_int(hapi_env_int_version_houdini_engine_major) -> 8;
hapi_env_int_to_int(hapi_env_int_version_houdini_engine_minor) -> 9;
hapi_env_int_to_int(hapi_env_int_version_houdini_engine_api) -> 10;
hapi_env_int_to_int(hapi_env_int_license) -> 11;
hapi_env_int_to_int(hapi_env_int_max) -> 12.

% Convert integer to HAPI env int.
int_to_hapi_env_int(-1) -> hapi_env_int_invalid;
int_to_hapi_env_int(0) -> hapi_env_int_version_houdini_major;
int_to_hapi_env_int(1) -> hapi_env_int_version_houdini_minor;
int_to_hapi_env_int(2) -> hapi_env_int_version_houdini_build;
int_to_hapi_env_int(3) -> hapi_env_int_version_houdini_patch;
int_to_hapi_env_int(4) -> hapi_env_int_version_orig_houdini_major;
int_to_hapi_env_int(5) -> hapi_env_int_version_orig_houdini_minor;
int_to_hapi_env_int(6) -> hapi_env_int_version_orig_houdini_build;
int_to_hapi_env_int(7) -> hapi_env_int_version_orig_houdini_patch;
int_to_hapi_env_int(8) -> hapi_env_int_version_houdini_engine_major;
int_to_hapi_env_int(9) -> hapi_env_int_version_houdini_engine_minor;
int_to_hapi_env_int(10) -> hapi_env_int_version_houdini_engine_api;
int_to_hapi_env_int(11) -> hapi_env_int_license;
int_to_hapi_env_int(12) -> hapi_env_int_max.
