-module (terakol_upload_handler).
-include ("terakol.hrl").

-export ([init/3]).
-export ([handle/2]).
-export ([terminate/3]).

init(_, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
  handle_files_upload(Req, State).

terminate(_Reason, _Req, _State) ->
	ok.

%%% ===========================================================================
%%% private functions
%%% ===========================================================================
handle_files_upload(Req, State) ->
  case cowboy_req:part(Req) of
    {ok, Headers, Req2} ->
      case cow_multipart:form_data(Headers) of
        {file, _FieldName, Filename, _ContentType, _TE} ->
          AbsoluteFile = <<"/opt/dms/", Filename/binary>>,
          {ok, IoDevice} = file:open(AbsoluteFile, [raw, write]),
          Req3 = stream_file(Req2, IoDevice),
          ok = file:close(IoDevice),
          ?INFO("File uploaded: /opt/dms/~p", [Filename]),

          % update solr docs
          {ok, [{post, Post}]} = application:get_env(terakol, solr),
          Cmd = ?FMT("~s -c media '/opt/dms/~s'", [Post, Filename]),
          os:cmd(Cmd),
          handle_files_upload(Req3, State);
        {data, _FieldName} ->
          % save the data
          Req3 = stream_data(Req2),
          handle_files_upload(Req3, State)
      end;
    {done, Req2} ->
    	{ok, Req2, State}
  end.

stream_file(Req, IoDevice) ->
  case cowboy_req:part_body(Req) of
    {ok, Data, Req2} ->
      % write the data
      % ?DEBUG("..."),
      ok = file:write(IoDevice, Data),
      Req2;
    {more, Data, Req2} ->
      % ?DEBUG("..."),
      ok = file:write(IoDevice, Data),
      stream_file(Req2, IoDevice)
  end.

stream_data(Req) ->
  case cowboy_req:part_body(Req) of
    {ok, Data, Req2} ->
      % write the data
      ?ERROR("Data: ~p", [Data]),
      Req2;
    {more, Data, Req2} ->
      ?ERROR("Data: ~p", [Data]),
      stream_data(Req2)
  end.
