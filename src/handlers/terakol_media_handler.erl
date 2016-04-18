-module (terakol_media_handler).
-include("terakol.hrl").

-export([init/3]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
% -export([content_types_accepted/2]).

% Implementation
% -export([create_media/2]).
-export([list_media/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

is_authorized(Req, State) ->
  terakol_users_handler:is_authorized(Req, State).

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%  for GET
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, list_media}], Req, State}.

% for POST
% content_types_accepted(Req, State) ->
%   {[{{<<"application">>, <<"json">>, []}, create_media}], Req, State}.

list_media(Req, State) ->
  URL = get_URL(select, <<"localhost">>, 8983),
  case ?HTTP_GET(URL) of
    {ok, "200", _, Body} ->
      Res = jsx:decode(Body, [return_maps]),
      R1 = maps:get(<<"response">>, Res),
      Docs = maps:get(<<"docs">>, R1),
      {jsx:encode(Docs, ?JSX_OPTS), Req, State};
    _ ->
      {jsx:encode([]), Req, State}
  end.

% create_media(Req, State) ->
%   {ok, Headers, Req2} = cowboy_req:part(Req),
% 	{ok, Data, Req3} = cowboy_req:part_body(Req2),
% 	{file, <<"inputfile">>, Filename, ContentType, _TE}
% 		= cow_multipart:form_data(Headers),
% 	io:format("Received file ~p of content-type ~p as follow:~n~p~n~n",
% 		[Filename, ContentType, Data]),
%   {false, Req3, State}.

%%% ===========================================================================
%%% private functions
%%% ===========================================================================
get_URL(select, Host, Port) ->
  ?FMT("http://~s:~p/solr/media/select?wt=json&q=*:*", [Host, Port]);
get_URL(update, Host, Port) ->
  ?FMT("http://~s:~p/solr/media/update/json/docs?commit=true", [Host, Port]).
