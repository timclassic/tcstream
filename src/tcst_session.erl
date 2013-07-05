%%%-------------------------------------------------------------------------
%%% @author Tim Stewart <tim@trueex.com>
%%%  [http://www.trueex.com]
%%% @copyright 2013 trueEX Group, LLC
%%% @doc A streaming session
%%% @end
%%%-------------------------------------------------------------------------

-module(tcst_session).
-behavior(gen_fsm).

%% Public API
-export([new_session/4,
         recover_session/3,
         continue_session/4,
         generate_nonce/0,
         set_next_nonce/2,
         send/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4]).

%% State machine
-export([connected_0/2,   connected_0/3,
         connected_0_1/2, connected_0_1/3,
         connected_1/2,   connected_1/3,
         connected_1_0/2, connected_1_0/3]).

-define(SERVER, ?MODULE).
-define(IDLE_PATH_TIMEOUT, 5000).
-define(CLOSE_TIMEOUT, 35000). %% Slightly larger than the client error timeout
-define(NONCE_LEN, 6).
-define(NONCE_CHARS, "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789").

-record(state, {id,
                mod,
                args,
                user_state,
                msg_q = queue:new(),
                seq = 2,
                next_nonce = <<"000000">>,
                path0 = undefined,
                path1 = undefined,
                path_limit = 204800,
                path_bytes = 0,
                idle_path_tref = undefined,
                close_tref = undefined}).


%%%=========================================================================
%%% Public API
%%%=========================================================================

new_session(ID, Path0Pid, Mod, Args) ->
    case gen_fsm:start_link({via, gproc, {n, l, {?MODULE, ID}}},
                            ?MODULE, [ID, Path0Pid, Mod, Args], []) of
        {ok, Pid} ->
            error_logger:info_msg("Session ~p created with ID ~s\n",
                                  [Pid, ID]),
            {ok, Pid};

        {error, {already_started, Pid}} ->
            %% Ask existing session to shut down so we can replace it
            error_logger:info_msg("Shutting down old instance of session ID ~s",
                                  [ID]),
            ok = gen_fsm:sync_send_all_state_event(Pid, shutdown),
            new_session(ID, Path0Pid, Mod, Args);

        {error, Error} ->
            Error
    end.

recover_session(ID, Seq, Path0Pid) ->
    try
        Pid = gproc:lookup_pid({n, l, {?MODULE, ID}}),
        true = link(Pid),
        ok = gen_fsm:sync_send_all_state_event(Pid, {recover, Seq, Path0Pid}),
        {ok, Pid}
    catch
        error:badarg ->
            error_logger:info_msg("Recovery attempt for nonexistent "
                                  "session ID ~s, ignoring\n", [ID]),
            no_such_session
    end.

continue_session(ID, Nonce, Seq, PathPid) ->
    Pid = try
              gproc:lookup_pid({n, l, {?MODULE, ID}})
          catch
              error:badarg ->
                  error_logger:info_msg("Continue attempt for nonexistent "
                                        "session ID ~s, ignoring\n", [ID]),
                  no_such_session
          end,

    case Pid of
        no_such_session ->
            no_such_session;
        _ ->
            true = link(Pid),
            case gen_fsm:sync_send_event(Pid, {newconn, PathPid, Nonce, Seq}) of
                ok ->
                    {ok, Pid};
                session_full ->
                    error_logger:info_msg("Connection attempt to full session "
                                          "ID ~s, ignoring (Page Reload on "
                                          "Firefox??)\n", [ID]),
                    session_full
            end
    end.

generate_nonce() ->
    list_to_binary(generate_random_string(?NONCE_LEN, ?NONCE_CHARS)).

set_next_nonce(Pid, Nonce) ->
    gen_fsm:sync_send_all_state_event(Pid, {set_next_nonce, Nonce}).

send(Pid, Channel, Data) ->
    gen_fsm:send_event(Pid, {send, Channel, Data}).


%%%=========================================================================
%%% gen_fsm callbacks
%%%=========================================================================

init([ID, Path0Pid, Mod, Args]) ->
    StateData = #state{id    = ID,
                       path0 = Path0Pid,
                       mod   = Mod,
                       args  = Args},

    %% Run the user's session_init callback
    case Mod:session_init(ID, Args) of
        {stop, Reason} ->
            {stop, Reason};
        {ok, UserState} ->
            %% Transition to first connected state
            {_, State, NewStateData} = enter_connected_0(StateData),

            %% Run user's session_start callback
            NewUserState = Mod:session_start(UserState),

            {ok, State, NewStateData#state{user_state = NewUserState}}
    end.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event({set_next_nonce, Nonce}, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData#state{next_nonce = Nonce}};
handle_sync_event({recover, _Seq, Path0Pid}, _From, _StateName, StateData) ->
    StateData2 = reset_paths(StateData),
    StateData3 = StateData2#state{path0 = Path0Pid},
    {_, State, StateData4} = enter_connected_0(StateData3),
    {reply, ok, State, StateData4};
handle_sync_event(shutdown, _From, _StateName, StateData) ->
    {stop, normal, ok, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

handle_info({timeout, TRef, close}, _StateName,
            #state{close_tref = TRef, id = ID} = StateData) ->
    error_logger:info_msg("Session ~s shutting down due to idleness\n", [ID]),
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName,
          #state{mod = Mod,
                 user_state = UserState} = _StateData) ->
    Mod:session_end(UserState),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%=========================================================================
%%% State machine
%%%=========================================================================

%% Entry point - runs when entering state from any other state
enter_connected_0(StateData) ->
    NewStateData = reset_idle_path_timeout(?IDLE_PATH_TIMEOUT, StateData),
    {next_state, connected_0, NewStateData}.
enter_connected_0(Reply, StateData) ->
    {_, State, NewStateData} = enter_connected_0(StateData),
    {reply, Reply, State, NewStateData}.

%% Idle path timeout
connected_0(idle_path_timeout, StateData) ->
    StateData2 = cancel_idle_path_timeout(StateData),
    StateData3 = reset_close_timeout(?CLOSE_TIMEOUT, StateData2),
    {next_state, connected_0, StateData3};

%% Only Path0 is connected.  Send data and update byte count.
connected_0({send, Channel, Data},
            #state{seq        = Seq,
                   path0      = Path0Pid,
                   path_bytes = PathBytes} = StateData) ->
    Bytes = tcst_path:send(Path0Pid, Seq, Channel, Data),
    StateData2 = set_next_seq(StateData),
    {next_state, connected_0,
     StateData2#state{path_bytes = PathBytes + Bytes}}.

%% Path0 is connected but has yet to stream enough data to exceed
%% limit.  Record new Path1 connection and transition to
%% connected_0_1.
connected_0({newconn, NewPid, Nonce, _Seq}, _From,
            #state{next_nonce     = Nonce,
                   idle_path_tref = IdlePathTRef,
                   path_bytes     = PathBytes,
                   path_limit     = PathLimit} = StateData)
  when PathBytes < PathLimit,
       IdlePathTRef =/= undefined ->
    StateData2 = reset_idle_path_timeout(?IDLE_PATH_TIMEOUT, StateData),
    StateData3 = cancel_close_timeout(StateData2),
    enter_connected_0_1(ok, StateData3#state{path1 = NewPid});

%% Path0 is connected and has surpassed path limit (bytes or time).
%% Path1 has just connected.  Signal Path0 to shut down and transition
%% to connected_1.
connected_0({newconn, NewPid, Nonce, _Seq}, _From,
            #state{next_nonce = Nonce,
                   path0      = Path0Pid} = StateData) ->
    ok = tcst_path:close(Path0Pid),
    NewStateData = cancel_close_timeout(StateData),
    enter_connected_1(ok, NewStateData#state{path0      = undefined,
                                             path1      = NewPid,
                                             path_bytes = 0}).


%% Entry point - runs when entering state from any other state
enter_connected_0_1(StateData) ->
    {next_state, connected_0_1, StateData}.
enter_connected_0_1(Reply, StateData) ->
    {_, State, NewStateData} = enter_connected_0_1(StateData),
    {reply, Reply, State, NewStateData}.

%% Idle path timeout
connected_0_1(idle_path_timeout, #state{path0 = Path0Pid} = StateData) ->
    ok = tcst_path:close(Path0Pid),
    enter_connected_1(StateData#state{path0 = undefined,
                                      path_bytes = 0});

%% Path0 and Path1 are connected.  Send data.  If path byte limit is
%% exceeded, close Path0 and transition to connected_1.
connected_0_1({send, Channel, Data},
              #state{seq        = Seq,
                     path0      = Path0Pid,
                     path_bytes = PathBytes,
                     path_limit = PathLimit} = StateData) ->
    Bytes = tcst_path:send(Path0Pid, Seq, Channel, Data),
    StateData2 = set_next_seq(StateData),
    case PathBytes + Bytes of
        B when B >= PathLimit ->
            ok = tcst_path:close(Path0Pid),
            enter_connected_1(StateData2#state{path0      = undefined,
                                               path_bytes = 0});
        B ->
            {next_state, connected_0_1, StateData2#state{path_bytes = B}}
    end.

connected_0_1({newconn, _NewPid, _Nonce, _Seq}, _From, StateData) ->
    {reply, session_full, connected_0_1, StateData}.


%% Entry point - runs when entering state from any other state
enter_connected_1(StateData) ->
    NewStateData = reset_idle_path_timeout(?IDLE_PATH_TIMEOUT, StateData),
    {next_state, connected_1, NewStateData}.
enter_connected_1(Reply, StateData) ->
    {_, State, NewStateData} = enter_connected_1(StateData),
    {reply, Reply, State, NewStateData}.

%% Idle path timeout
connected_1(idle_path_timeout, StateData) ->
    StateData2 = cancel_idle_path_timeout(StateData),
    StateData3 = reset_close_timeout(?CLOSE_TIMEOUT, StateData2),
    {next_state, connected_1, StateData3};

%% Only Path1 is connected.  Send data and update byte count.
connected_1({send, Channel, Data},
            #state{seq        = Seq,
                   path1      = Path1Pid,
                   path_bytes = PathBytes} = StateData) ->
    Bytes = tcst_path:send(Path1Pid, Seq, Channel, Data),
    StateData2 = set_next_seq(StateData),
    {next_state, connected_1,
     StateData2#state{path_bytes = PathBytes + Bytes}}.

%% Path1 is connected but has yet to stream enough data to exceed
%% limit.  Record new Path0 connection and transition to
%% connected_1_0.
connected_1({newconn, NewPid, Nonce, _Seq}, _From,
            #state{next_nonce     = Nonce,
                   idle_path_tref = IdlePathTRef,
                   path_bytes     = PathBytes,
                   path_limit     = PathLimit} = StateData)
  when PathBytes < PathLimit,
       IdlePathTRef =/= undefined ->
    StateData2 = reset_idle_path_timeout(?IDLE_PATH_TIMEOUT, StateData),
    StateData3 = cancel_close_timeout(StateData2),
    enter_connected_1_0(ok, StateData3#state{path0 = NewPid});

%% Path1 is connected and has surpassed path limit (bytes or time).
%% Path0 has just connected.  Signal Path1 to shut down and transition
%% to connected_0.
connected_1({newconn, NewPid, Nonce, _Seq}, _From,
            #state{next_nonce = Nonce,
                   path1      = Path1Pid} = StateData) ->
    ok = tcst_path:close(Path1Pid),
    NewStateData = cancel_close_timeout(StateData),
    enter_connected_0(ok, NewStateData#state{path0      = NewPid,
                                             path1      = undefined,
                                             path_bytes = 0}).


%% Entry point - runs when entering state from any other state
enter_connected_1_0(StateData) ->
    {next_state, connected_1_0, StateData}.
enter_connected_1_0(Reply, StateData) ->
    {_, State, NewStateData} = enter_connected_1_0(StateData),
    {reply, Reply, State, NewStateData}.

%% Idle path timeout
connected_1_0(idle_path_timeout, #state{path1 = Path1Pid} = StateData) ->
    ok = tcst_path:close(Path1Pid),
    enter_connected_0(StateData#state{path1      = undefined,
                                      path_bytes = 0});

%% Path1 and Path0 are connected.  Send data.  If path byte limit is
%% exceeded, close Path1 and transition to connected_0.
connected_1_0({send, Channel, Data},
              #state{seq        = Seq,
                     path1      = Path1Pid,
                     path_bytes = PathBytes,
                     path_limit = PathLimit} = StateData) ->
    Bytes = tcst_path:send(Path1Pid, Seq, Channel, Data),
    StateData2 = set_next_seq(StateData),
    case PathBytes + Bytes of
        B when B >= PathLimit ->
            ok = tcst_path:close(Path1Pid),
            enter_connected_0(StateData2#state{path1      = undefined,
                                               path_bytes = 0});
        B ->
            {next_state, connected_1_0, StateData2#state{path_bytes = B}}
    end.

connected_1_0({newconn, _NewPid, _Nonce, _Seq}, _From, StateData) ->
    {reply, session_full, connected_1_0, StateData}.


%%%=========================================================================
%%% Private functions
%%%=========================================================================

reset_paths(#state{path0 = Path0Pid, path1 = Path1Pid} = StateData) ->

    %% Close down any open paths
    lists:foreach(
      fun(Pid) ->
              ok = case Pid of
                       undefined -> ok;
                       _         -> tcst_path:close(Path0Pid),
                                    ok
                   end
      end, [Path0Pid, Path1Pid]),

    %% Cancel idle path timer
    NewStateData = cancel_idle_path_timeout(StateData),

    %% Set path bytes to zero and return new state data
    NewStateData#state{path_bytes = 0}.

cancel_idle_path_timeout(#state{idle_path_tref = TRef} = StateData) ->
    ok = case TRef of
             undefined -> ok;
             _         -> gen_fsm:cancel_timer(TRef),
                          ok
         end,
    StateData#state{idle_path_tref = undefined}.

reset_idle_path_timeout(Ms, StateData) ->
    NewStateData = cancel_idle_path_timeout(StateData),
    NewTRef = gen_fsm:send_event_after(Ms, idle_path_timeout),
    NewStateData#state{idle_path_tref = NewTRef}.

cancel_close_timeout(#state{close_tref = TRef} = StateData) ->
    ok = case TRef of
             undefined -> ok;
             _         -> erlang:cancel_timer(TRef),
                          ok
         end,
    StateData#state{close_tref = undefined}.

reset_close_timeout(Ms, StateData) ->
    NewStateData = cancel_close_timeout(StateData),
    NewTRef = erlang:start_timer(Ms, self(), close),
    NewStateData#state{close_tref = NewTRef}.

set_next_seq(#state{seq = Seq} = StateData) ->
    %% Sequence numbers wrap from 999999 to 2
    case Seq + 1 of
        999999 ->
            StateData#state{seq = 2};
        NextSeq ->
            StateData#state{seq = NextSeq}
    end.

generate_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).
