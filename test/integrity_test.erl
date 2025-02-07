-module(integrity_test).

-include_lib("eunit/include/eunit.hrl").

-define(ROOT_DATA, "test/data").

bad_options_test() ->
    {error,{options,{unavailable_option,1}}} = erlxml:new_stream([{unavailable_option, 1}]),
    true.

to_binary_ok_test() ->
    Xml = {xmlel,<<"foo">>, [{<<"attr1">>,<<"bar">>}], [{xmlcdata,<<"Some Value">>}]},
    <<"<foo attr1=\"bar\">Some Value</foo>">> = erlxml:to_binary(Xml),
    true.

to_binary_error_test() ->
    Xml = {axmlel,<<"foo">>, [{<<"attr1">>,<<"bar">>}], [{xmlcdata,<<"Some Value">>}]},
    {error, badarg} = erlxml:to_binary(Xml),
    true.

dom_parsing_ok_test() ->
    {ok,{xmlel,<<"foo">>, [{<<"attr1">>,<<"bar">>}], [{xmlcdata,<<"Some Value">>}]}} =
        erlxml:parse(<<"<foo attr1='bar'>Some Value</foo>">>),
    true.

dom_parsing_error_test() ->
    InvalidStaza = <<"<foo attr1='bar'>Some Value<foo">>,
    {error,invalid_stanza} = erlxml:parse(InvalidStaza),
    true.

stream_parsing_error_test() ->
    InvalidStaza = <<"foo attr1='bar'>Some Value<foo">>,
    {ok, Parser} = erlxml:new_stream(),
    {error, {invalid_stanza, InvalidStaza}} = erlxml:parse_stream(Parser, InvalidStaza),
    true.

stream_parsing_invalid_stanza_start_error_test() ->
    {ok, Parser} = erlxml:new_stream(),
    {ok,[{xmlstreamstart,<<"stream">>,[]}]} = erlxml:parse_stream(Parser, <<"<stream>">>),
    {ok,[{xmlel,<<"tag1">>,[], [
        {xmlel,<<"g">>,[],[{xmlcdata,<<"sss">>}]}]}]} = erlxml:parse_stream(Parser, <<" <tag1><g>sss</g></tag1>">>),
    {error,{invalid_stanza,<<" tag1">>}} = erlxml:parse_stream(Parser, <<" tag1">>),
    true.

max_stanza_limit_hit_test() ->
    Data = <<"<stream><tag>1</tag></stream>">>,
    {ok, Parser} = erlxml:new_stream([{stanza_limit, 11}]),
    {ok, Parser2} = erlxml:new_stream([{stanza_limit, 12}]),
    {error, {max_stanza_limit_hit, <<"<tag>1</tag></stream>">>}} = erlxml:parse_stream(Parser, Data),
    {ok, _} = erlxml:parse_stream(Parser2, Data),
    true.

max_stanza_limit_hit_cdata_test() ->
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

chunks_test() ->
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

skip_header_and_comments_test() ->
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

one_by_one_char_test() ->
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

strip_invalid_utf8_test() ->
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

strip_invalid_token_EF_B7_9F_test() ->
    {ok, InvalidToken} = file:read_file(<<?ROOT_DATA, "/invalid_token_EF_B7_9F.txt">>),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

strip_invalid_token_EF_B7_90_test() ->
    {ok, InvalidToken} = file:read_file(<<?ROOT_DATA, "/invalid_token_EF_B7_90.txt">>),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

strip_invalid_token_EF_B7_A4_test() ->
    {ok, InvalidToken} = file:read_file(<<?ROOT_DATA, "/invalid_token_EF_B7_A4.txt">>),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

strip_invalid_token_EF_B7_AF_test() ->
    {ok, InvalidToken} = file:read_file(<<?ROOT_DATA, "/invalid_token_EF_B7_AF.txt">>),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

strip_invalid_token_EF_BF_BE_test() ->
    {ok, InvalidToken} = file:read_file(<<?ROOT_DATA, "/invalid_token_EF_BF_BE.txt">>),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

strip_invalid_token_EF_BF_BF_test() ->
    {ok, InvalidToken} = file:read_file(<<?ROOT_DATA, "/invalid_token_EF_BF_BF.txt">>),
    true = test_strip_invalid_token(InvalidToken, <<"123456">>).

succeeded_C3_AF__C2_BF__C2_B0_test() ->
    {ok, Token} = file:read_file(<<?ROOT_DATA, "/succeeded_C3_AF__C2_BF__C2_B0.txt">>),
    true = test_strip_invalid_token(Token, Token).

succeeded_C6_87_test() ->
    {ok, Token} = file:read_file(<<?ROOT_DATA, "/succeeded_C6_87.txt">>),
    true = test_strip_invalid_token(Token, Token).

succeeded_EF_B7_89_test() ->
    {ok, Token} = file:read_file(<<?ROOT_DATA, "/succeeded_EF_B7_89.txt">>),
    true = test_strip_invalid_token(Token, Token).

succeeded_EF_B7_B0_test() ->
    {ok, Token} = file:read_file(<<?ROOT_DATA, "/succeeded_EF_B7_B0.txt">>),
    true = test_strip_invalid_token(Token, Token).

succeeded_EF_B8_80_test() ->
    {ok, Token} = file:read_file(<<?ROOT_DATA, "/succeeded_EF_B8_80.txt">>),
    true = test_strip_invalid_token(Token, Token).

succeeded_EF_BF_AE_test() ->
    {ok, Token} = file:read_file(<<?ROOT_DATA, "/succeeded_EF_BF_AE.txt">>),
    true = test_strip_invalid_token(Token, Token).

succeeded_F0_90_8C_88_test() ->
    {ok, Token} = file:read_file(<<?ROOT_DATA, "/succeeded_F0_90_8C_88.txt">>),
    true = test_strip_invalid_token(Token, Token).

% internals

test_strip_invalid_token(InvalidToken, ExpectedResult) ->
    Data = <<"<iq xmlns='namespace'><body>", InvalidToken/binary,"</body></iq>">>,
    {ok, Parser} = erlxml:new_stream([{strip_non_utf8, true}]),
    {ok,[{xmlstreamstart,<<"stream">>,[]}]} = erlxml:parse_stream(Parser, <<"<stream>">>),
    {ok,[{xmlel,<<"iq">>,
        [{<<"xmlns">>,<<"namespace">>}],
        [{xmlel,<<"body">>,[],[{xmlcdata, ExpectedResult}]}]}]} = erlxml:parse_stream(Parser, Data),
    true.
