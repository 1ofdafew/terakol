-module (cors_middleware).
-behaviour (cowboy_middleware).

-include ("terakol.hrl").

-export ([execute/2]).

execute(Req, State) ->
  Req2 = set_cors_headers(Req),
  {Method, Req3} = cowboy_req:method(Req2),
  case Method of
    <<"OPTIONS">> ->
      {ok, Req4} = cowboy_req:reply(200, Req3),
      {halt, Req4};
    _ ->
      {ok, Req, State}
  end.

set_cors_headers(Req) ->
  Headers = [
    {<<"access-control-allow-origin">>, <<$*>>},
    {<<"access-control-allow-headers">>, <<"Origin, X-Requested-With, Content-Type, Accept">>},
    {<<"access-control-max-age">>, <<"1000">>}
  ],
  set_headers(Req, Headers).

set_headers(Req, Headers) ->
  F = fun({H, V}, R) ->
        NewReq = cowboy_req:set_resp_header(H, V, R),
        NewReq
      end,
  lists:foldl(F, Req, Headers).
