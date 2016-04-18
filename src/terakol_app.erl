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
      {<<"/api/auth">>, terakol_auth_handler, []},
      {<<"/api/users">>, terakol_users_handler, []},
      {<<"/api/users/:id">>, terakol_users_update_handler, []},
      {<<"/api/media">>, terakol_media_handler, []},
      {<<"/api/upload">>, terakol_upload_handler, []},

      % our admin UI
      {<<"/">>, cowboy_static, {priv_file, terakol, "dist/index.html"}},
			{<<"/static/[...]">>, cowboy_static, {priv_dir, terakol, "dist/static",
				[{mimetypes, cow_mimetypes, all}]}}
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
