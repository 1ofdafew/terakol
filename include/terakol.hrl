-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(DEBUG(Text), lager:log(debug, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(DEBUG(Text, Args), lager:log(debug, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(INFO(Text), lager:log(info, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(INFO(Text, Args), lager:log(info, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(ERROR(Text), lager:log(error, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(ERROR(Text, Args), lager:log(error, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

% formatting
-define(FMT(S, Args), lists:flatten(io_lib:format(S, Args))).

-define(JSX_OPTS, [{indent, 2}]).
-define(JSX_MAPS, [return_maps]).

% iBrowse components
-define(IB_OPTS, [{response_format, binary}]).
-define(IB_TIMEOUT, 10000).
-define(IB_HEADER, [{content_type, "application/json"}]).

-define(HTTP_POST(URL, JSON),
  ibrowse:send_req(URL, ?IB_HEADER, post, JSON, ?IB_OPTS, ?IB_TIMEOUT)).
-define(HTTP_GET(URL),
  ibrowse:send_req(URL, [], get, [], ?IB_OPTS, ?IB_TIMEOUT)).

% Our session store
-record(terakol_session, {key, val, expiry, timestamp = calendar:local_time()}).
