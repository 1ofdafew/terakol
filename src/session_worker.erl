-module(session_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include ("terakol.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define (SESSION, terakol_session).
-record(state, {nodes}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0]).
-export ([get_session/1]).
-export ([set_session/3]).
-export ([list_session/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

set_session(Key, Val, Days) ->
  gen_server:call(?MODULE, {set_session, Key, Val, Days}).

get_session(Key) ->
  gen_server:call(?MODULE, {get_session, Key}).

list_session() ->
  gen_server:call(?MODULE, {list_session}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  ?DEBUG("Initializing..."),
  Nodes = [node()|nodes()],
  rpc:multicall(Nodes, application, stop, [mnesia]),
  case mnesia:create_schema(Nodes) of
    ok ->
      rpc:multicall(Nodes, application, start, [mnesia]),
      mnesia:create_table(?SESSION, [
        {attributes, record_info(fields, ?SESSION)},
        {disc_copies, Nodes},
        {type, set}
      ]),
      mnesia:wait_for_tables([?SESSION], 1000),
      {ok, #state{nodes = Nodes}};
    {error, _} ->
      rpc:multicall(Nodes, application, start, [mnesia]),
      {ok, #state{nodes = Nodes}}
  end.

handle_call({get_session, Id}, _From, State) ->
  F = fun() ->
        qlc:eval(qlc:q([
          {Key, Val} || #?SESSION{key=Key, val=Val}
            <- mnesia:table(?SESSION), Key =:= Id
        ]))
      end,
  {reply, mnesia:activity(transaction, F), State};

handle_call({set_session, Key, Val, Days}, _From, State) ->
  Expiry = iso8601:add_time(calendar:local_time(), Days * 24, 0, 0),
  F = fun() ->
          mnesia:write(#?SESSION{key=Key, val=Val, expiry=Expiry})
      end,
  ok = mnesia:activity(transaction, F),
  {reply, {ok, success}, State};

handle_call({list_session}, _From, State) ->
  F = fun() ->
        qlc:eval(qlc:q(
          [{Key, Val} || #?SESSION{key=Key, val=Val} <- mnesia:table(?SESSION)]
        ))
      end,
  % {reply, mnesia:dirty_all_keys(?SESSION), State};
  {reply, mnesia:activity(transaction, F), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
