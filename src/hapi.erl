%%% @author Mykola Konyk  <mykola@konyk.org>
%%%
%%% @copyright 2015

-module(hapi).
-version(1.9).

-on_load(init/0).

-spec init() -> ok | {error, any()}.
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
        end,
    erlang:load_nif(filename:join(PrivDir, hapi_nif), 0).
