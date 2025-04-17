-module(elbowstc).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
									  {'_', [
            {"/", cowboy_static, {priv_file, elbowstc, "static/index.html"}},
            {"/api/code", code_provider, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    {ok, self()}.

stop(_State) ->
    ok.

