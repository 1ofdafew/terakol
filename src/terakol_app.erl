-module(terakol_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([start_phase/3]).

start(_Type, _Args) ->
  lager:set_loglevel(lager_console_backend, debug),
	terakol_sup:start_link().

stop(_State) ->
	ok.

start_phase(start_listeners, _StartType, []) ->
  {ok, [{port, Port}, {listeners, Listeners}]} =
    application:get_env(terakol, http),
  Dispatch = cowboy_router:compile([
    {'_', [
      {<<"/api/users">>, terakol_users_handler, []},
      {<<"/api/users/:id">>, terakol_users_update_handler, []}
    ]}
  ]),
  RanchOptions = [{port, Port}],
  CowboyOptions = [
    {env, [{dispatch, Dispatch}]},
    {compress, true},
    {timeout, 12000}
  ],
  cowboy:start_http(terakol_http, Listeners, RanchOptions, CowboyOptions),
  ok.

  %
  % {start_phases, [
  %   {start_listeners, []}
  % ]},
  % {env, [
  %   {http, [
  %     {port, 5000},
  %     {listeners, 10}
  %   ]}
  % ]}.
