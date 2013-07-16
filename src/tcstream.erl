-module(tcstream).

-export([debug/0, start/0, stop/0]).
-export([get_listen_ip/0,
         get_listen_port/0,
         get_idle_path_timeout/0,
         get_close_timeout/0,
         get_path_limit/0]).

-define(APP, ?MODULE).

-define(IDLE_PATH_TIMEOUT, 5000).
-define(CLOSE_TIMEOUT, 35000). %% Larger than the client error timeout
-define(PATH_LIMIT, 204800).


%%%=========================================================================
%%% Public API
%%%=========================================================================

start() ->
    application:start(tcstream).

stop() ->
    application:stop(tcstream).

debug() ->
    ok = application:start(sasl),
    ok = application:start(gproc),
    ok = application:start(inets),
    ok = application:start(crypto),
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
