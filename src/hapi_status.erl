%%% @author Mykola Konyk  <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% HAPI_Status enum.

-module(hapi_status).
-version(1.9).


-type hapi_status() ::
    hapi_status_call_result |
    hapi_status_cook_result |
    hapi_status_cook_state |
    hapi_status_max.

-export_type([hapi_status/0]).
-export([hapi_status_to_int/1, int_to_hapi_status/1]).

% Convert HAPI status to integer.
hapi_status_to_int(hapi_status_call_result) -> 0;
hapi_status_to_int(hapi_status_cook_result) -> 1;
hapi_status_to_int(hapi_status_cook_state) -> 2;
hapi_status_to_int(hapi_status_max) -> 3.

% Convert int to HAPI status.
int_to_hapi_status(0) -> hapi_status_call_result;
int_to_hapi_status(1) -> hapi_status_cook_result;
int_to_hapi_status(2) -> hapi_status_cook_state;
int_to_hapi_status(3) -> hapi_status_max.
