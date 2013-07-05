-module(tcst_demo_resource).
-export([init/1]).
-export([allowed_methods/2,
         content_types_provided/2,
         to_html/2]).

-record(context, {}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(_ConfigProps) ->
    {ok, #context{}}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    Map = [{"text/html", to_html}],
    {Map, ReqData, Context}.

to_html(ReqData, Context) ->
    IndexPath = filename:join(
                  [filename:dirname(code:which(?MODULE)),
                   "..", "priv", "index.html"]),
    {ok, Index} = file:read_file(IndexPath),
    {Index, ReqData, Context}.
