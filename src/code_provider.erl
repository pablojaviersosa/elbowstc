-module(code_provider).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            % Suponemos que subes texto plano.
            io:format("Archivo recibido:~n~s~n", [Body]),
            Resp = jsx:encode(#{status => <<"ok">>, content => Body}),
            {ok, Resp2} = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Resp, Req2),
            {ok, Resp2, State};
        _ ->
            {ok, Resp2} = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req),
            {ok, Resp2, State}
    end.
