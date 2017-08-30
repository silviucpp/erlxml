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
        test_stream_parsing_invalid_stanza_start_error,
        test_max_stanza_limit_hit,
        test_max_stanza_limit_hit_cdata,
        test_chunks,
        test_skip_header_and_comments,
        test_one_by_one_char,
        test_strip_invalid_utf8,
        test_strip_invalid_token_EF_B7_9F,
        test_strip_invalid_token_EF_B7_90,
        test_strip_invalid_token_EF_B7_A4,
        test_strip_invalid_token_EF_B7_AF,
        test_strip_invalid_token_EF_BF_BE,
        test_strip_invalid_token_EF_BF_BF,
        test_succeeded_C3_AF__C2_BF__C2_B0,
        test_succeeded_C6_87,
        test_succeeded_EF_B7_89,
        test_succeeded_EF_B7_B0,
        test_succeeded_EF_B8_80,
        test_succeeded_EF_BF_AE,
        test_succeeded_F0_90_8C_88
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

test_stream_parsing_invalid_stanza_start_error(_Config) ->
    {ok, Parser} = erlxml:new_stream(),
    {ok,[{xmlstreamstart,<<"stream">>,[]}]} = erlxml:parse_stream(Parser, <<"<stream>">>),
    {ok,[{xmlel,<<"tag1">>,[], [
        {xmlel,<<"g">>,[],[{xmlcdata,<<"sss">>}]}]}]} = erlxml:parse_stream(Parser, <<" <tag1><g>sss</g></tag1>">>),
    {error,{invalid_stanza,<<" tag1">>}} = erlxml:parse_stream(Parser, <<" tag1">>),
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
    Chunk1 = <<"\n\r <stream ss='aa'><foo attr1=\"bar">>,
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

test_strip_invalid_token_EF_B7_9F(_Config) ->
    {ok, InvalidToken} = file:read_file("../../test/data/invalid_token_EF_B7_9F.txt"),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

test_strip_invalid_token_EF_B7_90(_Config) ->
    {ok, InvalidToken} = file:read_file("../../test/data/invalid_token_EF_B7_90.txt"),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

test_strip_invalid_token_EF_B7_A4(_Config) ->
    {ok, InvalidToken} = file:read_file("../../test/data/invalid_token_EF_B7_A4.txt"),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

test_strip_invalid_token_EF_B7_AF(_Config) ->
    {ok, InvalidToken} = file:read_file("../../test/data/invalid_token_EF_B7_AF.txt"),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

test_strip_invalid_token_EF_BF_BE(_Config) ->
    {ok, InvalidToken} = file:read_file("../../test/data/invalid_token_EF_BF_BE.txt"),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

test_strip_invalid_token_EF_BF_BF(_Config) ->
    {ok, InvalidToken} = file:read_file("../../test/data/invalid_token_EF_BF_BF.txt"),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

test_succeeded_C3_AF__C2_BF__C2_B0(_Config) ->
    {ok, Token} = file:read_file("../../test/data/succeeded_C3_AF__C2_BF__C2_B0.txt"),
    true = test_strip_invalid_token(Token, Token).

test_succeeded_C6_87(_Config) ->
    {ok, Token} = file:read_file("../../test/data/succeeded_C6_87.txt"),
    true = test_strip_invalid_token(Token, Token).

test_succeeded_EF_B7_89(_Config) ->
    {ok, Token} = file:read_file("../../test/data/succeeded_EF_B7_89.txt"),
    true = test_strip_invalid_token(Token, Token).

test_succeeded_EF_B7_B0(_Config) ->
    {ok, Token} = file:read_file("../../test/data/succeeded_EF_B7_B0.txt"),
    true = test_strip_invalid_token(Token, Token).

test_succeeded_EF_B8_80(_Config) ->
    {ok, Token} = file:read_file("../../test/data/succeeded_EF_B8_80.txt"),
    true = test_strip_invalid_token(Token, Token).

test_succeeded_EF_BF_AE(_Config) ->
    {ok, Token} = file:read_file("../../test/data/succeeded_EF_BF_AE.txt"),
    true = test_strip_invalid_token(Token, Token).

test_succeeded_F0_90_8C_88(_Config) ->
    {ok, Token} = file:read_file("../../test/data/succeeded_F0_90_8C_88.txt"),
    true = test_strip_invalid_token(Token, Token).

test_strip_invalid_token(InvalidToken, ExpectedResult) ->
    Data = <<"<iq xmlns='namespace'><body>", InvalidToken/binary,"</body></iq>">>,
    {ok, Parser} = erlxml:new_stream([{strip_non_utf8, true}]),
    {ok,[{xmlstreamstart,<<"stream">>,[]}]} = erlxml:parse_stream(Parser, <<"<stream>">>),
    {ok,[{xmlel,<<"iq">>,
        [{<<"xmlns">>,<<"namespace">>}],
        [{xmlel,<<"body">>,[],[{xmlcdata, ExpectedResult}]}]}]} = erlxml:parse_stream(Parser, Data),
    true.