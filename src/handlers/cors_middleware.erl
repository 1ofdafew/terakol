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
  cowboy_req:set_resp_header(
    <<"access-control-allow-origin">>, <<$*>>, Req).
