-module (terakol_admin_handler).
-include ("terakol.hrl").

-export ([init/3]).
-export ([handle/2]).
-export ([terminate/3]).

init(_, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
  handle_req(Req, State).

terminate(_Reason, _Req, _State) ->
	ok.

handle_req(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path, Req2} = cowboy_req:path(Req1),
  {ok, BodyQs, Req3} = cowboy_req:body_qs(Req2),
  {QsVals, Req4} = cowboy_req:qs_vals(Req3),

  ?DEBUG("Method: ~p, Path: ~p, BodyQs: ~p, QsVals: ~p",
    [Method, Path, BodyQs, QsVals]),

  Resp =
    case Method of
      <<"GET">>   -> do_get(Path, {BodyQs, QsVals}, Req4);
      <<"POST">>  -> do_post(Path, {BodyQs, QsVals}, Req4);
      Method      -> do_error({method, Method}, Req4)
    end,
  case Resp of
    {render, Template, Data} ->
      render_template(Template, Data, Req4);
    {redirect, Location} ->
      cowboy_req:reply(302, [{<<"Location">>, Location}], [], Req4)
  end,
  {ok, Req4, State}.

do_get(<<"/admin">>, {_BodyQs, _QsVals}, _Req4) ->
  {redirect, <<"/admin/home">>};

do_get(<<"/admin/home">>, {_BodyQs, _QsVals}, _Req4) ->
  {render, <<"home">>, []};

do_get(<<"/admin/manage">>, {_BodyQs, _QsVals}, _Req4) ->
  {render, <<"manage">>, []};

do_get(_, {_BodyQs, _QsVals}, _Req4) ->
  {render, <<"home">>, []}.

do_post(_Path, {_BodyQs, _QsVals}, _Req4) ->
  {render, <<"home">>, []}.

do_error({method, Method}, Req) ->
  Message = <<"Method ", Method/binary, " is not allowed">>,
  {ok, Content} = error_dtl:render([{error, Message}]),
  cowboy_req:reply(404, [], Content, Req);

do_error({template, Message}, Req) ->
  {ok, Content} = error_dtl:render([{error, Message}]),
  cowboy_req:reply(404, [], Content, Req).

render_template(Template, Data, Req) ->
  T1 = binary_to_list(<<Template/binary, "_dtl">>),
  T2 = list_to_atom(T1),

  ?DEBUG("Rendering template using: ~p", [T2]),
  case catch T2:render(Data) of
    {ok, Content} ->
      cowboy_req:reply(200, [], Content, Req);
    _ ->
      M = <<"Template ", Template/binary, ".dtl is not available!">>,
      do_error({template, M}, Req)
  end.
