-module(erlxml).
-author("silviu.caragea").

-include("erlxml.hrl").

-export([
    new_stream/0,
    new_stream/1,
    parse_stream/2,
    reset_stream/1,
    parse/1,
    to_binary/1
]).

-spec new_stream() ->
    {ok, reference()} | {error, reason()}.

new_stream() ->
    new_stream([]).

-spec new_stream([erlxml_option()]) ->
    {ok, reference()} | {error, reason()}.

new_stream(Options) ->
    erlxml_nif:new_stream(Options).

-spec parse_stream(reference(), iolist() | binary()) ->
    {ok, [#xmlstreamstart{} | #xmlel{} | #xmlstreamend{}]} | {error, reason()} | {error, reason(), binary()}.

parse_stream(Parser, Data) ->
    erlxml_nif:chunk_feed_stream(Parser, Data).

-spec reset_stream(reference()) ->
    ok | {error, reason()}.

reset_stream(Parser) ->
    erlxml_nif:reset_stream(Parser).

-spec parse(iolist() | binary()) ->
    {ok, #xmlel{}} | {error, reason()}.

parse(Data) ->
    erlxml_nif:dom_parse(Data).

-spec to_binary(#xmlel{}) ->
    binary() | {error, reason()}.

to_binary(Data) ->
    erlxml_nif:to_binary(Data).
