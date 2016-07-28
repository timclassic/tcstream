%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% Copyright 2013 trueEX Group, LLC

%%%-------------------------------------------------------------------------
%%% @author Tim Stewart <tim@trueex.com>
%%%  [http://www.trueex.com]
%%% @copyright 2013 trueEX Group, LLC
%%% @doc XHR/XDR HTTP Streaming Top-level Supervisor
%%% @end
%%%-------------------------------------------------------------------------

-module(tcstream_sup).
-behavior(supervisor).

%% Public API
-export([start_link/0, start_link_demo/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%=========================================================================
%%% Public API
%%%=========================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_link_demo() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [demo]).


%%%=========================================================================
%%% Supervisor callbacks
%%%=========================================================================

init([]) ->
    init_opts([], []);

init(Opts) ->
    init_opts(Opts, []).

init_opts([ demo | Opts ], Children) ->
    Ip = tcstream:get_listen_ip(),
    Port = tcstream:get_listen_port(),
    {ok, Dispatch} = file:consult(filename:join(
                                    [filename:dirname(code:which(?MODULE)),
                                     "..", "priv", "dispatch.conf"])),
    WebConfig = [{ip, Ip},
                 {port, Port},
                 {log_dir, "log"},
                 {dispatch, Dispatch}],
    Webmachine = {webmachine_mochiweb,
                  {webmachine_mochiweb, start, [WebConfig]},
                  permanent, 5000, worker, dynamic},

    init_opts(Opts, [ Webmachine | Children ]);

init_opts([], Children) ->
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
