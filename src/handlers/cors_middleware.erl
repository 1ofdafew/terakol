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
      {ok, Req3, State}
  end.

set_cors_headers(Req) ->
  Headers = [
    {<<"Access-Control-Allow-Origin">>, <<$*>>},
    {<<"Access-Control-Allow-Headers">>, <<"Origin, X-Requested-With, Content-Type, Accept, Authorization">>}
  ],
  add_headers(Req, Headers).

add_headers(Req, Headers) ->
  F = fun({Tag, Val}, Req1) ->
        Req2 = cowboy_req:set_resp_header(Tag, Val, Req1),
        Req2
      end,
  lists:foldl(F, Req, Headers).
