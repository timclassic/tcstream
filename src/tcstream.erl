%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% Copyright 2013 trueEX Group, LLC

%%%-------------------------------------------------------------------------
%%% @author Tim Stewart <tim@trueex.com>
%%%  [http://www.trueex.com]
%%% @copyright 2013 trueEX Group, LLC
%%% @doc Public interface for tcstream
%%% @end
%%%-------------------------------------------------------------------------

-module(tcstream).

-export([debug/0, start/0, stop/0]).
-export([get_listen_ip/0,
         get_listen_port/0,
         get_idle_path_timeout/0,
         get_close_timeout/0,
         get_path_limit/0,
         get_path_noop_interval/0]).

-define(APP, ?MODULE).

-define(IDLE_PATH_TIMEOUT, 5000).
-define(CLOSE_TIMEOUT, 35000). %% Larger than the client error timeout
-define(PATH_LIMIT, 204800).
-define(PATH_NOOP_INTERVAL, 1000).


%%%=========================================================================
%%% Public API
%%%=========================================================================

start() ->
    application:start(tcstream).

stop() ->
    application:stop(tcstream).

debug() ->
    ok = application:start(sasl),
    ok = application:start(asn1),
    ok = application:start(gproc),
    ok = application:start(inets),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(xmerl),
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(mochiweb),
    ok = application:start(webmachine),
    ok = application:start(tcstream).


%%%=========================================================================
%%% Public API, configuration access
%%%=========================================================================

get_listen_ip()          -> get_env(listen_ip,          "0.0.0.0").
get_listen_port()        -> get_env(listen_port,        8020).
get_idle_path_timeout()  -> get_env(idle_path_timeout,  ?IDLE_PATH_TIMEOUT).
get_close_timeout()      -> get_env(close_timeout,      ?CLOSE_TIMEOUT).
get_path_limit()         -> get_env(path_limit,         ?PATH_LIMIT).
get_path_noop_interval() -> get_env(path_noop_interval, ?PATH_NOOP_INTERVAL).


%% ========================================================================
%% Internal functions
%% ========================================================================

%%get_env(Parameter) ->
%%    {ok, V} = application:get_env(?APP, Parameter),
%%    V.
get_env(Parameter, Default) ->
    case application:get_env(?APP, Parameter) of
        undefined -> Default;
        {ok, V}   -> V
    end.
