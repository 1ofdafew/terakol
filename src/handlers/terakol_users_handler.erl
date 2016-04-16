-module (terakol_users_handler).
-include("terakol.hrl").

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

% Implementation
-export([create_user/2]).
-export([list_users/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

%  for GET
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, list_users}], Req, State}.

% for POST
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, create_user}], Req, State}.

list_users(Req, State) ->
  URL = get_URL(select, <<"localhost">>, 8983),
  ?INFO("Running for URL: ~p", [URL]),
  case ?HTTP_GET(URL) of
    {ok, _Code, _Headers, Body} ->
      Res = jsx:decode(Body, [return_maps]),
      ResHeader = maps:get(<<"responseHeader">>, Res),
      case maps:get(<<"status">>, ResHeader) of
        0 ->
          % ok, successful
          R1 = maps:get(<<"response">>, Res),
          Docs = maps:get(<<"docs">>, R1),
          {jsx:encode(Docs, ?JSX_OPTS), Req, State};
        _ ->
          {jsx:encode(ResHeader, ?JSX_OPTS), Req, State}
      end;
    _ ->
      {jsx:encode([]), Req, State}
  end.

create_user(Req, State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    % ?INFO("Body: ~p", [Body]),
    Data = jsx:decode(Body, [return_maps]),
    % ?INFO("Data: ~p", [Data]),
    Password = maps:get(<<"password">>, Data),
    Id = maps:get(<<"id">>, Data),
    Payload = [
      {id, Id},
      {password, erlpass:hash(Password)},
      {realm, <<"user">>},
      {created_at, iso8601:format(calendar:local_time())},
      {updated_at, iso8601:format(calendar:local_time())}
    ],
    JSON = jsx:encode(Payload, ?JSX_OPTS),
    URL = get_URL(update, <<"localhost">>, 8983),
    ?INFO("Adding new user: URL=~p, JSON=~p", [URL, JSON]),
    case ?HTTP_POST(URL, JSON) of
      {ok, "200", _, _Resp} ->
        ?INFO("Adding user is ok"),
        URI = <<"/api/users/", Id/binary>>,
        {{true, URI}, Req1, State};
      {ok, Code, _, Resp} ->
        ?ERROR("Error adding user: ~p:~p", [Code, Resp]),
        {false, Req1, State}
    end
  catch
    _:Error ->
      Err = jsx:encode([
        {error, <<"Invalid requests">>},
        {details, Error}
      ]),
      Req2 = cowboy_req:set_resp_body(Err, Req),
      {false, Req2, State}
  end.

%%% ===========================================================================
%%% private functions
%%% ===========================================================================
get_URL(select, Host, Port) ->
  ?FMT("http://~s:~p/solr/users/select?wt=json&q=*:*", [Host, Port]);
get_URL(update, Host, Port) ->
  ?FMT("http://~s:~p/solr/users/update/json/docs?commit=true", [Host, Port]).
