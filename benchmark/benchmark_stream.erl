-module(benchmark_stream).

-define(CHUNK_SIZE, 1024).

-export([
    bench/4
]).

bench(Module, File, Number, Concurrency) ->
    {Chunks, BinarySize} = readlines(File, ?CHUNK_SIZE),

    Self = self(),
    List = lists:seq(1, Concurrency),
    LoopNumbers = Number div Concurrency,

    ProcFun = fun() ->
        {ok, Parser} = new_parser(Module),
        NewParser1 = run_parser([<<"<stream>">>], Module, Parser),
        NewParser2 = loop(LoopNumbers, Chunks, Module, NewParser1),
        NewParser3 = run_parser([<<"</stream>">>], Module, NewParser2),
        close(Module, NewParser3),
        Self ! {self(), done}
    end,

    A = os:timestamp(),
    Pids = [spawn_link(ProcFun) || _ <- List],
    [receive {Pid, done} -> ok end || Pid <- Pids],
    B = os:timestamp(),

    print(Module, Concurrency, BinarySize*Number, A, B).

loop(0, _Chunks, _Module, Parser) ->
    Parser;
loop(Nr, Chunks, Module, Parser) ->
    NewParser = run_parser(Chunks, Module, Parser),
    loop(Nr-1, Chunks, Module, NewParser).

run_parser([H|T], Module, Parser) ->
    case stream_parse(Module, Parser, H) of
        {ok, _} ->
            run_parser(T, Module, Parser);
        {ok, NewParser, _} ->
            run_parser(T, Module, NewParser)
    end;
run_parser([], _Module, Parser) ->
    Parser.

new_parser(erlxml) ->
    erlxml:new_stream([{stanza_limit, 65000}]);
new_parser(exml) ->
    exml_stream:new_parser();
new_parser(fast_xml) ->
    Parent = self(),
    ConsumerPid = spawn_link(fun() -> ok = fxml_receive_till_end(), Parent ! {fxml_completed, self()} end),
    {ok, fxml_stream:new(ConsumerPid)};
new_parser(dummy) ->
    {ok, null}.

stream_parse(erlxml, Parser, Data) ->
    erlxml:parse_stream(Parser, Data);
stream_parse(exml, Parser , Data) ->
    exml_stream:parse(Parser, Data);
stream_parse(fast_xml, Parser, Data) ->
    {ok, fxml_stream:parse(Parser, Data)};
stream_parse(dummy, _Parser , _Data) ->
    {ok, []}.

close(fast_xml, Parser) ->
    fxml_stream:close(Parser),
    receive
        {fxml_completed, _ConsumerPid} ->
            ok
    end;
close(exml, Parser) ->
    exml_stream:free_parser(Parser);
close(_Module, _Parser) ->
    ok.

readlines(FileName, LengthChunks) ->
    {ok, Device} = file:open(FileName, [read]),
    Lines = get_lines(Device),
    Binary = binary_join(Lines),
    Size = byte_size(Binary),
    {build_chunks(Binary, LengthChunks, []), Size}.

build_chunks(Binary, Length, Acc) ->
    case byte_size(Binary) > Length of
        true ->
            <<Chunk:Length/binary, Rest/binary>>  = Binary,
            build_chunks(Rest, Length, [Chunk | Acc]);
        _ ->
            lists:reverse([Binary|Acc])
    end.

get_lines(Device) ->
    lists:reverse(get_lines(Device, [])).

get_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  ->
            file:close(Device), Accum;
        Line ->
            get_lines(Device, [list_to_binary(Line)|Accum])
    end.

binary_join([Part]) ->
    Part;
binary_join([Head|Tail]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary , Value/binary>> end, Head, Tail).

print(Module, Concurrency, Bytes, A, B) ->
    Microsecond = timer:now_diff(B, A),
    Milliseconds = Microsecond /1000,
    Secs = Milliseconds/1000,
    BytesPerSec = Bytes/Secs,
    io:format("### engine: ~p concurrency: ~p -> ~p ms ~s/sec total bytes processed: ~s ~n", [Module, Concurrency, Milliseconds, format_size(BytesPerSec), format_size(Bytes)]).

format_size(Size) ->
    format_size(Size, ["B","KB","MB","GB","TB","PB"]).

format_size(S, [_|[_|_] = L]) when S >= 1024 -> format_size(S/1024, L);
format_size(S, [M|_]) ->
    io_lib:format("~.2f ~s", [float(S), M]).

fxml_receive_till_end() ->
    receive
        {'$gen_event', Msg} ->
            case Msg of
                {xmlstreamend, _} ->
                    ok;
                _ ->
                    fxml_receive_till_end()
            end
    end.
