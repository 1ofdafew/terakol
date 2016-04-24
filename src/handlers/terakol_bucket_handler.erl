-module (terakol_bucket_handler).
-include("terakol.hrl").

%% Standard callbacks.
-export ([init/3]).
-export ([is_authorized/2]).
-export ([allowed_methods/2]).
-export ([content_types_accepted/2]).
-export ([resource_exists/2]).

-export([process_bucket/2]).

-define (HOST, <<"localhost">>).
-define (PORT, 8983).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

is_authorized(Req, State) ->
  terakol_users_handler:is_authorized(Req, State).

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

% for POST
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, process_bucket}], Req, State}.

resource_exists(Req, State) ->
  {false, Req, State}.

process_bucket(Req, State) ->
  ?DEBUG("Processing POST Request..."),
  try
    {Action, Req1} = cowboy_req:binding(action, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Data = jsx:decode(Body, [return_maps]),
    case Action of
      <<"create">> ->
        % user should send data of type
        %  bucket { name: <bucket_name> }
        %
        create_bucket(Data, Req2, State);
      <<"delete">> ->
        delete_bucket(Data, Req2, State);
      <<"schema">> ->
        create_schema(Data, Req2, State)
    end
  catch
    _:_ ->
      Err = [{error, <<"Malformed JSON Request">>}],
      send_reply(false, Err, Req, State)
  end.

%%% ===========================================================================
%%% private functions
%%% ===========================================================================

create_bucket(Map, Req, State) ->
  try
    ?DEBUG("Maps: ~p", [Map]),
    Bucket = maps:get(<<"bucket">>, Map),
    Coll = maps:get(<<"name">>, Bucket),

    % ok, let's create the bucket
    % bin/solr create_collection -c <coll> -d data_driven_schema_configs
    Cmd =
      <<"cd /opt/solr; bin/solr ",
        "create_collection -c ", Coll/binary, " ",
        "-d data_driven_schema_configs">>,
    case catch os:cmd(binary_to_list(Cmd)) of
      Resp ->
        % ?DEBUG("Resp: ~p", [Resp]),
        R2 = string:tokens(Resp, "\n\n"),
        % ?DEBUG("Length: ~p, Response: ~p", [length(R2), R2]),
        case length(R2) of
          6 ->
            % error,
            Err = [{error, list_to_binary(lists:nth(4, R2))}],
            send_reply(false, Err, Req, State);
          _ ->
            {_, R3} = lists:split(4, R2),
            R4 = lists:flatten(R3),
            M1 = jsx:decode(list_to_binary(R4), [return_maps]),
            % ?DEBUG("M1: ~p", [M1]),
            M2 = maps:get(<<"success">>, M1),
            Msg = [{success, M2}],
            send_reply(true, Msg, Req, State)
        end
    end

  catch
    _:_ ->
      Err2 = [{error, <<"Invalid data for creating bucket">>}],
      send_reply(false, Err2, Req, State)
  end.

delete_bucket(Map, Req, State) ->
  try
    ?DEBUG("Maps: ~p", [Map]),
    Bucket = maps:get(<<"bucket">>, Map),
    Coll = maps:get(<<"name">>, Bucket),

    % ok, let's delete the bucket
    Cmd =
      <<"cd /opt/solr; bin/solr delete -c ", Coll/binary>>,
    case catch os:cmd(binary_to_list(Cmd)) of
      Resp ->
        ?DEBUG("Resp: ~p", [Resp]),
        R2 = string:tokens(Resp, "\n\n"),
        ?DEBUG("Length: ~p, Response: ~p", [length(R2), R2]),
        case length(R2) of
          2 ->
            % error,
            Err = [{error, list_to_binary(lists:nth(2, R2))}],
            send_reply(false, Err, Req, State);
          10 ->
            {_, R3} = lists:split(3, R2),
            R4 = lists:flatten(R3),
            M1 = jsx:decode(list_to_binary(R4), [return_maps]),
            ?DEBUG("M1: ~p", [M1]),
            M2 = maps:get(<<"success">>, M1),
            Msg = [{success, M2}],
            send_reply(true, Msg, Req, State)
        end
    end

  catch
    _:_ ->
      Err2 = [{error, <<"Invalid data for creating bucket">>}],
      send_reply(false, Err2, Req, State)
  end.

create_schema(Map, Req, State) ->
  % {
  %     "bucket": {
  %         "name": "users"
  %     },
  %     "schema": [
  %         { "name": "password", "type": "string", "stored": true },
  %         { "name": "realm", "type": "string", "stored": true },
  %         { "name": "created_at", "type": "string", "stored": true },
  %         { "name": "updated_at", "type": "string", "stored": true },
  %     ]
  % }
  ?DEBUG("Map: ~p", [Map]),
  try
    Coll = maps:get(<<"bucket">>, Map),
    Schema = maps:get(<<"schema">>, Map),
    % ?DEBUG("~nColl: ~p, ~nSchema: ~p", [Coll, Schema]),
    J1 = concat_fields(Schema),
    J2 = <<"{", J1/binary, "}">>,
    ?DEBUG("JSON: ~p", [J2]),

    URL = get_URL(schema, ?HOST, ?PORT, maps:get(<<"name">>, Coll)),
    case ?HTTP_POST(URL, J2) of
      {ok, "200", _, Body} ->
        Hdr = maps:get(<<"responseHeader">>, jsx:decode(Body, [return_maps])),
        send_reply(true, [{success, Hdr}], Req, State);
      _ ->
        send_reply(false, [{error, <<"Unable to set schema!">>}], Req, State)
    end
  catch
    _:_ ->
      Err2 = [{error, <<"Invalid data for creating schema">>}],
      send_reply(false, Err2, Req, State)
  end.

send_reply(Resp, Msg, Req, State) ->
  Req1 = cowboy_req:set_resp_body(jsx:encode(Msg), Req),
  {Resp, Req1, State}.

concat_fields(List) ->
  concat_fields(List, <<"">>).

concat_fields([H|T], Accu) ->
  J = jsx:encode(H),
  concat_fields(T, <<Accu/binary, "\"add-field\": ", J/binary, ",">>);
concat_fields([], Accu) ->
  Size = size(Accu) - 1,
  <<Accu:Size/binary>>.

get_URL(schema, Host, Port, Coll) ->
  ?FMT("http://~s:~p/solr/~s/schema?wt=json", [Host, Port, Coll]).

% exec_cmd(Command) ->
%   ?DEBUG("Command: ~p", [Command]),
%   Port = open_port({spawn, Command}, [binary, {line, 255}]),
%   get_data(Port).

% get_data(Port) ->
%   receive
%     {Port, {data, Bytes}} ->
%       ?DEBUG("data: ~p", [Bytes]);
%     {Port, eof} ->
%       Port ! {self(), close};
%     {Port, Any} ->
%       ?DEBUG("Any: ~p", [Any])
%   end.



