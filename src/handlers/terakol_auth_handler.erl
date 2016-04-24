-module (terakol_auth_handler).
-include ("terakol.hrl").

-import (terakol_user_update_handler, [fetch_user_data/1]).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-export ([list_docs/2]).
-export ([process_auth/2]).
-export ([ensure_exists/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

%  for GET
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, list_docs}], Req, State}.

% for POST
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, process_auth}], Req, State}.

list_docs(Req, State) ->
  {[], Req, State}.

process_auth(Req, State) ->
  try
    ?DEBUG("Enter..."),
    {ok, Body, Req1} = cowboy_req:body(Req),
    Data = jsx:decode(Body, [return_maps]),
    ?DEBUG("Data: ~p", [Data]),
    ensure_exists([<<"id">>, <<"password">>], Data),

    Id = maps:get(<<"id">>, Data),
    ?DEBUG("Fetching user data..."),

    % below might throw {ok, []} when no such user
    {ok, [User]} = fetch_user_data(Id),
    ?DEBUG("User: ~p", [User]),
    Pass = maps:get(<<"password">>, Data),
    Hash = maps:get(<<"password">>, User),

    ?DEBUG("Matching password..."),
    case erlpass:match(Pass, Hash) of
      true ->
        ?DEBUG("Done matching...password ok!"),
        % ok, generate uuid as token
        Token = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),

        % save the session
        session_worker:set_session(Token, Id, 5),
        ?DEBUG("Token: ~p, Generating Auth...", [Token]),
        Auth = base64:encode_to_string(<<"token:", Token/binary>>),
        ?DEBUG("Auth: ~p", [Auth]),

        Reply = [
          {token, Token},
          {basic, list_to_binary(Auth)}
        ],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        ?DEBUG("Done reply..."),
        {true, Req2, State};
      false ->
        {false, Req1, State}
    end
  catch
    _:{reason, Error} ->
      ?ERROR("Error thrown: ~p", [Error]),
      Req4 = cowboy_req:set_resp_body(jsx:encode([Error]), Req),
      {false, Req4, State};
    _:{badmatch, _} ->
      ?ERROR("No such user error..."),
      Err = [{error, <<"Invalid email, or password">>}],
      Req5 = cowboy_req:set_resp_body(jsx:encode(Err), Req),
      {false, Req5, State};
    _:Else ->
      ?ERROR("Error: ~p", [Else]),
      Req6 = cowboy_req:set_resp_body(jsx:encode([{error, Else}]), Req),
      {false, Req6, State}
  end.

ensure_exists([H|T], Map) ->
  case maps:find(H, Map) of
    error ->
      ?ERROR("Param ~p is missing", [H]),
      throw(<<"Parameter ", H/binary, " is mandatory">>);
    _ ->
      ensure_exists(T, Map)
  end;
ensure_exists([], _Map) -> ok.
