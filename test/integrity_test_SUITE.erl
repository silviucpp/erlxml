-module(integrity_test_SUITE).
-author("silviu.caragea").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [
    {group, erlxml_group}
].

groups() -> [
    {erlxml_group, [sequence], [
        test_chunks,
        test_skip_header_and_comments,
        test_one_by_one_char
    ]}
].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_chunks(_Config) ->
    Chunk1 = <<"\n\r dsds <stream ss='aa'><foo attr1=\"bar">>,
    Chunk2 = <<"\">Some Value</foo><el2 ss='asd'/></stream>">>,

    {ok, Parser} = erlxml:new_stream(),
    {ok,[{xmlstreamstart,<<"stream">>,[{<<"ss">>,<<"aa">>}]}]} = erlxml:parse_stream(Parser, Chunk1),
    {ok,[{xmlel,<<"foo">>,
        [{<<"attr1">>,<<"bar">>}],
        [{xmlcdata,<<"Some Value">>}]},
        {xmlel,<<"el2">>,[{<<"ss">>,<<"asd">>}],[]},
        {xmlstreamend,<<"stream">>}]} = erlxml:parse_stream(Parser, Chunk2),
    true.

test_skip_header_and_comments(_Config) ->
    Data = <<"<?xml version='1.0'?>
    <!-- comment is here -->
    <stream>
        <!-- comment is inside -->
        <tag>1</tag>
        <tag>2</tag>
        <tag>3</tag>
    </stream>">>,

    {ok, Parser} = erlxml:new_stream(),
    {ok,[{xmlstreamstart,<<"stream">>,[]},
        {xmlel,<<"tag">>,[],[{xmlcdata,<<"1">>}]},
        {xmlel,<<"tag">>,[],[{xmlcdata,<<"2">>}]},
        {xmlel,<<"tag">>,[],[{xmlcdata,<<"3">>}]},
        {xmlstreamend,<<"stream">>}]} = erlxml:parse_stream(Parser, Data),

    ok = erlxml:reset_stream(Parser),

    {ok,[{xmlstreamstart,<<"stream">>,[]},
        {xmlel,<<"tag">>,[],[{xmlcdata,<<"1">>}]},
        {xmlel,<<"tag">>,[],[{xmlcdata,<<"2">>}]},
        {xmlel,<<"tag">>,[],[{xmlcdata,<<"3">>}]},
        {xmlstreamend,<<"stream">>}]} = erlxml:parse_stream(Parser, binary_to_list(Data)),
    true.

test_one_by_one_char(_Config) ->
    Data = <<"<?xml version='1.0'?>
    <!-- comment is here -->
    <stream>
        <!-- comment is inside -->
        <tag>1</tag>
        <tag>2</tag>
        <tag>3</tag>
    </stream>">>,

    {ok, Parser} = erlxml:new_stream(),
    [{ok, _} = erlxml:parse_stream(Parser, [X]) || <<X:1/binary>> <= Data],
    true.