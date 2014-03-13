%%%-------------------------------------------------------------------------
%%% @author Tim Stewart <tim@trueex.com>
%%%  [http://www.trueex.com]
%%% @copyright 2013 trueEX Group, LLC
%%% @doc An individual stream path
%%% @end
%%%-------------------------------------------------------------------------

-module(tcst_path).

%% Public API
-export([send/4, close/1]).

%% Webmachine interface
-export([init/1,
         allowed_methods/2,
         known_content_type/2,
         content_types_provided/2,
         content_types_accepted/2,
         process_post/2]).

%% Other callbacks
-export([to_plain/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(CHUNK_SIZE, 10240).

-record(context, {session, module, args = []}).


%%%=========================================================================
%%% Public API
%%%=========================================================================

send(Pid, Seq, Channel, Data) ->
    Frame = build_data_frame(Seq, Channel, Data),
    Pid ! {send, Frame},
    erlang:byte_size(Frame).

close(Pid) ->
    try
        Pid ! {close, self()},
        ok
    catch
        error:badarg ->
            %% Connection already closed, ignore
            ok
    end.


%%%=========================================================================
%%% Webmachine callbacks - Official interface
%%%=========================================================================

init(Config) ->
    init(Config, #context{}).

init([], Context) ->
    %% Seed random number generator for Nonce building
    random:seed(erlang:now()),
    {ok, Context};
init([ {module, Mod} | Config ], Context) ->
    init(Config, Context#context{module = Mod});
init([ {args, Args} | Config ], Context) ->
    init(Config, Context#context{args = Args}).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

%% HACK: Force `Content-type' to be "text/plain" if it does not
%% already exist.  This is a known issue with IE's XDomainRequest
%% object (in that it never supplies a Content-type header), and is
%% required because the rest of Webmachine relies on this header
%% heavily.
%%
%% This code is extra hacky because it understands the details of
%% Mochiweb's header management, making it non-portable to other
%% Webmachine backends, probably.
known_content_type(ReqData, Context) ->
    Headers = ReqData#wm_reqdata.req_headers,

    case mochiweb_headers:get_value("content-type", Headers) of
        undefined ->
            NewHeaders = mochiweb_headers:enter("Content-type", "text/plain",
                                                Headers),
            {true, ReqData#wm_reqdata{req_headers=NewHeaders}, Context};
        _ContentType ->
            {true, ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
    Map = [{"text/plain", to_plain}],
    {Map, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    Map = [{"text/plain", to_plain}],
    {Map, ReqData, Context}.

process_post(ReqData, #context{module = Mod, args = Args} = Context) ->
    Data = wrq:req_body(ReqData),

    %% Parse out nonce, sequence number, and body
    [Nonce, Data2] = binary:split(Data, [<<32>>], []),
    [RawAttempt, Data3] = binary:split(Data2, [<<32>>], []),
    [RawSeq, Body] = binary:split(Data3, [<<32>>], []),
    Attempt = list_to_integer(binary_to_list(RawAttempt)),
    Seq = list_to_integer(binary_to_list(RawSeq)),

    case {Nonce, Seq} of
        {<<"000000">>, 0} ->
            %% TODO: Handle unsuccessful new_session() call
            %% New session
            {ok, Pid} = tcst_session:new_session(Body, self(), Mod, Args),
            post_return_with_stream(Pid, ReqData, Context);
        {<<"000000">>, _} ->
            %% Recovering session
            case tcst_session:recover_session(Body, Attempt, Seq, self()) of
                {ok, Pid} ->
                    post_return_with_stream(Pid, ReqData, Context);
                session_full ->
                    post_return_with_session_full(ReqData, Context);
                no_such_session ->
                    post_return_with_no_session(ReqData, Context);
                bad_attempt ->
                    post_return_with_bad_attempt(ReqData, Context)
            end;
        {_, _} ->
            %% Existing session
            case tcst_session:continue_session(Body, Nonce, Seq, self()) of
                {ok, Pid} ->
                    post_return_with_stream(Pid, ReqData, Context);
                session_full ->
                    post_return_with_session_full(ReqData, Context);
                no_such_session ->
                    post_return_with_no_session(ReqData, Context);
                bad_nonce ->
                    post_return_with_bad_nonce(ReqData, Context)
            end
    end.


%%%=========================================================================
%%% Webmachine callbacks - Local
%%%=========================================================================

%% This should never get called, but we have to define something in
%% content_types_provided/2 and content_types_accepted/2, so the
%% specified function should at least exist
to_plain(ReqData, Context) ->
    {"You should not be seeing this.", ReqData, Context}.


%%%=========================================================================
%%% Internal functions
%%%=========================================================================

post_return_with_stream(Pid, ReqData, Context) ->
    NewReqData = set_stream_headers(ReqData),
    {{halt, 200},
     wrq:set_resp_body({stream, setup_stream(Pid)}, NewReqData),
     Context#context{session = Pid}}.

post_return_with_session_full(ReqData, Context) ->
    {{halt, 409},
     wrq:set_resp_body("ERROR: Two paths already connected", ReqData),
     Context}.

post_return_with_bad_nonce(ReqData, Context) ->
    {{halt, 409},
     wrq:set_resp_body("ERROR: Incorrect nonce", ReqData),
     Context}.

post_return_with_bad_attempt(ReqData, Context) ->
    {{halt, 409},
     wrq:set_resp_body("ERROR: Bad (repeated?) recovery attempt", ReqData),
     Context}.

post_return_with_no_session(ReqData, Context) ->
    {{halt, 404},
     wrq:set_resp_body("ERROR: No such session", ReqData),
     Context}.

set_stream_headers(ReqData) ->
    %% IE's XDomainRequest requires that Access-Control-Allow-Origin
    %% be set
    ReqData1 = wrq:set_resp_header("Access-Control-Allow-Origin",
                                   "*", ReqData),

    %% Don't let browsers do any caching of this resource
    ReqData2 = wrq:set_resp_header("Cache-Control", "no-cache",
                                   ReqData1),

    %% Let browser know the content is gzipped
    ReqData3 = wrq:set_resp_header("Content-Encoding", "gzip",
                                   ReqData2),

    ReqData3.

setup_stream(SPid) ->
    Frame = build_sync_frame(),
    Z = zlib:open(),

    %% Set up streaming gzip compression.  We use gzip instead of
    %% deflate because deflate is interpreted differently by different
    %% browsers.  IE expects no zlib headers (in violation of RFC2616
    %% section 3.5) and other browsers correctly require them.  It's
    %% simpler to just use gzip and avoid the issue.
    %%
    %% The `31' below is magical.  It is the default value of 15, plus
    %% 16 to specify the use of gzip headers instead of the default
    %% zlib headers.  Other values are set to their usual defaults.
    %% See the description of `deflateInit2()' at
    %% http://zlib.net/manual.html#Advanced for details.  Erlang's
    %% documentation does not include information regarding adding 16
    %% to use gzip headers.
    %%
    %% If the compression parameters are changed, please make sure
    %% that the padding data (in build_sync_frame/0) is long enough to
    %% provide at least a 2KB pad size.
    ok = zlib:deflateInit(Z, default, deflated, 31, 8, default),
    CFrame = zlib:deflate(Z, Frame),
    {CFrame, fun() -> send_nonce(Z, SPid) end}.

send_nonce(Z, SPid) ->
    %% Generate the next nonce and inform session of new value.
    Nonce = tcst_session:generate_nonce(),
    ok = tcst_session:set_nonce(SPid, Nonce),

    %% Send NONCE frame to client
    Frame = build_nonce_frame(Nonce),
    CFrame = iolist_to_binary(zlib:deflate(Z, Frame, sync)),
    {CFrame, fun() -> wait_for_events(Z) end}.

wait_for_events(Z) ->
    receive
        {send, Frame} ->
            FrameSize = byte_size(Frame),
            send_next_chunk(Z, Frame, FrameSize, ?CHUNK_SIZE);

        {close, From} ->
            CFrame = iolist_to_binary(zlib:deflate(Z, [], finish)),
            ok = zlib:deflateEnd(Z),
            ok = zlib:close(Z),
            From ! ok,
            {CFrame, done}
    end.

send_next_chunk(Z, Frame, Remaining, ChunkSize)
  when ChunkSize < Remaining ->
    %% This is not the last chunk--split off a chunk
    {Chunk, NewFrame} = split_binary(Frame, ChunkSize),
    CChunk = iolist_to_binary(zlib:deflate(Z, Chunk, sync)),
    NewRem = Remaining - ChunkSize,
    {CChunk, fun() -> send_next_chunk(Z, NewFrame, NewRem, ChunkSize) end};
send_next_chunk(Z, Frame, Remaining, ChunkSize)
  when ChunkSize >= Remaining ->
    %% Last chunk
    CChunk = iolist_to_binary(zlib:deflate(Z, Frame, sync)),
    {CChunk, fun() -> wait_for_events(Z) end}.

build_sync_frame() ->
    %% We need some uncompressible data to use in the sync frame to
    %% reach our 2KB pad size even after compression.  Let's use
    %% RFC1097, gzipped with --fast and then encoded using base64
    %% (wrapping at 60 columns).
    %%
    %% This data results in a 2200-byte chunk being sent by the server
    %% after compression.
    Pad = <<"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"
            "UatVyBQAAA==">>,

    <<"1 S ", (list_to_binary(integer_to_list(byte_size(Pad))))/binary,
      " ", Pad/binary>>.

build_nonce_frame(Nonce) ->
    <<"N ", Nonce/binary>>.

build_data_frame(Seq, Channel, Data) ->
    <<"D ",
      (list_to_binary(integer_to_list(Seq)))/binary, " ",
      Channel/binary, " ",
      (list_to_binary(integer_to_list(byte_size(Data))))/binary, " ",
      Data/binary>>.
