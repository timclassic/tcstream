-module(tcstream).

-export([debug/0, start/0, stop/0]).
-export([get_listen_ip/0, get_listen_port/0]).

-define(APP, ?MODULE).

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

get_listen_ip()    -> get_env(listen_ip, "0.0.0.0").
get_listen_port()  -> get_env(listen_port, 8020).


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
