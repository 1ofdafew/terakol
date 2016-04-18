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
    {ok, Body, Req1} = cowboy_req:body(Req),
    Data = jsx:decode(Body, [return_maps]),
    ?DEBUG("Data: ~p", [Data]),
    ensure_exists([<<"id">>, <<"password">>], Data),

    Id = maps:get(<<"id">>, Data),
    {ok, [User]} = fetch_user_data(Id),
    ?DEBUG("User: ~p", [User]),
    Pass = maps:get(<<"password">>, Data),
    Hash = maps:get(<<"password">>, User),
    case erlpass:match(Pass, Hash) of
      true ->
        % ok, generate uuid as token
        Token = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),

        % save the session
        session_worker:set_session(Token, Id, 5),
        Auth = base64:encode_to_string(<<"token:", Token/binary>>),
        ?DEBUG("Auth: ~p", [Auth]),

        Reply = [{basic, list_to_binary(Auth)}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {true, Req2, State};
      false ->
        {false, Req1, State}
    end
  catch
    _:{badmatch, Error} ->
      ?ERROR("Error thrown: ~p", [Error]),
      Req4 = cowboy_req:set_resp_body(jsx:encode([Error]), Req),
      {false, Req4, State}
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
