-module(test_is_initialized).
-include_lib("eunit/include/eunit.hrl").

function_test() ->
    ?assertEqual(hapi_result_not_initialized, hapi:is_initialized()),
    ok.
