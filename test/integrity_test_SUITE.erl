-module(integrity_test_SUITE).
-author("silviu.caragea").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [
    {group, erlxml_group}
].

groups() -> [
    {erlxml_group, [sequence], [
        test_bad_options,
        test_to_binary_ok,
        test_to_binary_error,
        test_dom_parsing_ok,
        test_dom_parsing_error,
        test_stream_parsing_error,
        test_max_stanza_limit_hit,
        test_max_stanza_limit_hit_cdata,
        test_chunks,
        test_skip_header_and_comments,
        test_one_by_one_char,
        test_strip_invalid_utf8
    ]}
].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_bad_options(_Config) ->
    {error,{options,{unavailable_option,1}}} = erlxml:new_stream([{unavailable_option, 1}]),
    true.

test_to_binary_ok(_Config) ->
    Xml = {xmlel,<<"foo">>, [{<<"attr1">>,<<"bar">>}], [{xmlcdata,<<"Some Value">>}]},
    <<"<foo attr1=\"bar\">Some Value</foo>">> = erlxml:to_binary(Xml),
    true.

test_to_binary_error(_Config) ->
    Xml = {axmlel,<<"foo">>, [{<<"attr1">>,<<"bar">>}], [{xmlcdata,<<"Some Value">>}]},
    {error, badarg} = erlxml:to_binary(Xml),
    true.

test_dom_parsing_ok(_Config) ->
    {ok,{xmlel,<<"foo">>, [{<<"attr1">>,<<"bar">>}], [{xmlcdata,<<"Some Value">>}]}} =
        erlxml:parse(<<"<foo attr1='bar'>Some Value</foo>">>),
    true.

test_dom_parsing_error(_Config) ->
    InvalidStaza = <<"<foo attr1='bar'>Some Value<foo">>,
    {error,invalid_stanza} = erlxml:parse(InvalidStaza),
    true.

test_stream_parsing_error(_Config) ->
    InvalidStaza = <<"foo attr1='bar'>Some Value<foo">>,
    {ok, Parser} = erlxml:new_stream(),
    {error, {invalid_stanza, InvalidStaza}} = erlxml:parse_stream(Parser, InvalidStaza),
    true.

test_max_stanza_limit_hit(_Config) ->
    Data = <<"<stream><tag>1</tag></stream>">>,
    {ok, Parser} = erlxml:new_stream([{stanza_limit, 11}]),
    {ok, Parser2} = erlxml:new_stream([{stanza_limit, 12}]),
    {error, {max_stanza_limit_hit, <<"<tag>1</tag></stream>">>}} = erlxml:parse_stream(Parser, Data),
    {ok, _} = erlxml:parse_stream(Parser2, Data),
    true.

test_max_stanza_limit_hit_cdata(_Config) ->
    MaxLimit = 65536,
    Overflow = 1,

    Head = <<"<message type='chat' to='user_1@tsung.wxxw.com' id='c3d6824652fdacdafcc56b9d8fccb550' timestamp='1492994733441777'><body>">>,
    Tail = <<"</body></message>">>,
    Body = binary:copy(<<"1">>, (MaxLimit - (byte_size(Head) + byte_size(Tail)))+Overflow),
    PendingBuffer = <<Head/binary, Body/binary, Tail/binary, "</stream>">>,
    Stanza = <<"<stream>", PendingBuffer/binary>>,
    {ok, Parser} = erlxml:new_stream([{stanza_limit, MaxLimit}]),
    {error, {max_stanza_limit_hit, PendingBuffer}} = erlxml:parse_stream(Parser, Stanza),
    true.

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

test_strip_invalid_utf8(_Config) ->
    Data0 = <<"123🏇4567">>,
    Length = byte_size(Data0) -1,
    <<Data:Length/binary, _/binary>> = Data0,
    Msg= <<"<stream><node a='", Data/binary, "'>", Data/binary, "</node></stream>">>,
    {ok, Parser} = erlxml:new_stream([{strip_non_utf8, true}]),
    {ok,[{xmlstreamstart,<<"stream">>,[]},
        {xmlel,<<"node">>,
            [{<<"a">>,<<"123456">>}],
            [{xmlcdata,<<"123456">>}]},
        {xmlstreamend,<<"stream">>}]} = erlxml:parse_stream(Parser, Msg),
    true.