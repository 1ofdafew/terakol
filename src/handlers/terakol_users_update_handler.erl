-module (terakol_users_update_handler).
-include("terakol.hrl").

%% Standard callbacks.
-export ([init/3]).
-export ([allowed_methods/2]).
-export ([content_types_provided/2]).
-export ([content_types_accepted/2]).
-export ([delete_resource/2, delete_completed/2]).
-export ([resource_exists/2]).

-export([get_user/2]).
-export([update_user/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>], Req, State}.

%  for GET
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_user}], Req, State}.

% for PUT, PATCH
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, update_user}], Req, State}.

% for GET
get_user(Req, State) ->
  ?DEBUG("Processing GET Request..."),
  {Id, Req1} = cowboy_req:binding(id, Req),
  case fetch_user_data(Id) of
    {ok, [Data]} ->
      {jsx:encode(Data), Req1, State};
    {error, _} = Error ->
      {jsx:encode(Error), Req1, State}
  end.

update_user(Req, State) ->
  ?DEBUG("Processing PUT Request..."),
  try
    {Id, Req1} = cowboy_req:binding(id, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Data = jsx:decode(Body, [return_maps]),
    case fetch_user_data(Id) of
      {ok, [User]} ->
        ?DEBUG("UserData: ~p", [User]),
        % See if password is sent
        D2 =
          case maps:find(<<"password">>, Data) of
            error -> Data;
            {ok, Pass} ->
              % update the timestamp, and password
              NewPass = erlpass:hash(Pass),
              maps:update(<<"password">>, NewPass, Data)
          end,
        % merge the maps, and remove the _version_
        D3 = maps:merge(User, D2),
        D4 = maps:remove(<<"_version_">>, D3),
        D5 = maps:update(<<"updated_at">>,
          iso8601:format(calendar:local_time()), D4),

        % update the user data
        URL = get_URL(update, <<"localhost">>, 8983),
        JSON = jsx:encode(D5),
        ?INFO("Updating user: URL=~p, Data=~p", [URL, JSON]),
        case ?HTTP_POST(URL, JSON) of
          {ok, "200", _, Resp} ->
            ?INFO("Updating user is successful: ~p", [Resp]),
            {true, Req2, State};
          {ok, Code, _, Resp} ->
            ?ERROR("Can't update user: ~p - ~p", [Code, Resp]),
            {false, Req2, State}
        end;
      {error, _} ->
        {false, Req2, State}
    end
  catch
    _:_ ->
      Err = [{error, <<"Malformed JSON Request">>}],
      Req4 = cowboy_req:set_resp_body(jsx:encode(Err), Req),
      {false, Req4, State}
  end.

% for DELETE
delete_resource(Req, State) ->
  {Id, Req1} = cowboy_req:binding(id, Req),
  URL = get_URL(update, <<"localhost">>, 8983),
  JSON = jsx:encode([{delete, Id}]),
  ?INFO("Deleting user: URL=~p, JSON=~p", [URL, JSON]),
  case ?HTTP_POST(URL, JSON) of
    {ok, "200", _, Body} ->
      ?INFO("Res Body: ~p", [Body]),
      {true, Req1, State};
    {ok, "400", _, Body} ->
      ?ERROR("Delete error: ~p", [Body]),
      {false, Req1, State};
    {_, _, Body} ->
      ?ERROR("Post Error: ~p", [Body]),
      {false, Req1, State}
  end.

delete_completed(Req, State) ->
  {true, Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

%%% ===========================================================================
%%% private functions
%%% ===========================================================================
get_URL(update, Host, Port) ->
  ?FMT("http://~s:~p/solr/users/update/json/docs?commit=true", [Host, Port]).

get_URL(select, Host, Port, Email) ->
  ?FMT("http://~s:~p/solr/users/select?wt=json&q=id:~s", [Host, Port, Email]).

fetch_user_data(Id) ->
  URL = get_URL(select, <<"localhost">>, 8983, Id),
  ?INFO("Getting user: URL=~p", [URL]),
  case ?HTTP_GET(URL) of
    {ok, "200", _, Body} ->
      R1 = jsx:decode(Body, [return_maps]),
      R2 = maps:get(<<"response">>, R1),
      {ok, maps:get(<<"docs">>, R2)};
    _ ->
      {error, <<"Can't fetch user info">>}
  end.
