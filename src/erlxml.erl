-module(erlxml).
-author("silviu.caragea").

-include("erlxml.hrl").

-export([

    %stream based parsing

    new_stream/1,
    parse_stream/2,
    parse_reset/2,

    %dom based parsing

    parse/1,
    to_binary/1
]).

new_stream(Options) ->
    erlxml_nif:new_stream(Options).

parse_stream(Parser, Data) ->
    erlxml_nif:chunk_feed_stream(Parser, Data).

parse_reset(Parser, SkipRoot) ->
    erlxml_nif:reset_stream(Parser, SkipRoot).

parse(Data) ->
    erlxml_nif:dom_parse(Data).

to_binary(Data) ->
    erlxml_nif:to_binary(Data).