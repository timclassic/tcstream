%%%-------------------------------------------------------------------------
%%% @author Tim Stewart <tim@trueex.com>
%%%  [http://www.trueex.com]
%%% @copyright 2013 trueEX Group, LLC
%%% @end
%%%-------------------------------------------------------------------------

-module(tcstream_app).
-behavior(application).

%% OTP Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
    Reply = case lists:member("enable_demo", init:get_plain_arguments()) of
                true  -> tcstream_sup:start_link_demo();
                false -> tcstream_sup:start_link()
            end,
    case Reply of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.

stop(_State) ->
    ok.
