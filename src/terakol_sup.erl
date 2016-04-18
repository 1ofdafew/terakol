-module(terakol_sup).
-behaviour(supervisor).

-include ("terakol.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Session = ?CHILD(session_worker, worker),
  Procs = [Session],
  {ok, {{one_for_one, 1, 5}, Procs}}.
