-module(benchmark).
-author("silviu.caragea").

-export([
    bench_encoding/3,
    bench_parsing/3
]).

-define(ELEMENT, {xmlel,<<"iq">>,
    [{<<"type">>,<<"get">>}],
    [{xmlel,<<"query">>,
        [{<<"xmlns">>,<<"jabber:iq:bulk">>}],
        [{xmlel,<<"r">>, [{<<"ver">>,<<"1489702723756">>},{<<"client">>,<<"0.41.16">>}], []},
            {xmlel,<<"b">>,[{<<"ver">>,<<"1470998323471">>}],[]},
            {xmlel,<<"pf">>, [{<<"type">>,<<"roster">>},{<<"ver">>,<<"1473925451360">>}], []},
            {xmlel,<<"pf">>, [{<<"type">>,<<"addressbook">>},{<<"ver">>,<<"1410177959174">>}], []}]},
        {xmlel,<<"message">>,
            [{<<"from">>,<<"user@wdomain/resource-TMnXEhgkGN">>},
                {<<"to">>,<<"user2@domain/mac70c36269">>},
                {<<"ts">>,<<"1490255206161">>},
                {<<"id">>,<<"YgP1z-18834681">>},
                {<<"type">>,<<"headline">>}],
            [{xmlel,<<"backendmessage">>,
                [{<<"xmlns">>,<<"notification">>},{<<"push">>,<<"0">>}],
                [{xmlel,<<"resreceived">>,[],
                    [{xmlel,<<"accountId">>,[],
                        [{xmlcdata,<<"23423534534">>}]},
                        {xmlel,<<"amount">>,[],
                            [{xmlcdata,
                                <<"0.200000000000000">>}]},
                        {xmlel,<<"type">>,[],
                            [{xmlcdata,<<"AEW-12">>}]},
                        {xmlel,<<"description">>,[],
                            [{xmlcdata,<<"884340">>}]}]}]}]}]}
).

-define(STANZA, <<"<iq type='get'>
   <query xmlns='jabber:iq:bulk'>
     <r ver='1489702723756' client='0.41.16'/>
     <b ver='1470998323471'/>
     <pf type='roster' ver='1473925451360'/>
     <pf type='addressbook' ver='1410177959174'/>
   </query>
   <message from='user@wdomain/resource-TMnXEhgkGN' to='user2@domain/mac70c36269' ts='1490255206161' id='YgP1z-18834681' type='headline'>
   <backendmessage xmlns='notification' push='0'>
     <resreceived>
       <accountId>
         23423534534
       </accountId>
       <amount>
         0.200000000000000
       </amount>
       <type>
         AEW-12
       </type>
       <description>
         884340
       </description>
     </resreceived>
   </backendmessage>
    </message>
 </iq>">>).

bench_encoding(Engine, Number, Concurrency) ->
    init(Engine),
    Fun = fun() -> to_binary(Engine) end,
    bench(Fun, Number, Concurrency).

bench_parsing(Engine, Number, Concurrency) ->
    init(Engine),
    Fun = fun() -> parse(Engine, ?STANZA) end,
    bench(Fun, Number, Concurrency).

bench(Fun, Number, Concurrency) ->
    Self = self(),
    List = lists:seq(1, Concurrency),
    LoopNumbers = Number div Concurrency,

    A = os:timestamp(),
    Pids = [spawn_link(fun() -> loop(LoopNumbers, Fun), Self ! {self(), done} end) || _ <- List],
    [receive {Pid, done} -> ok end || Pid <- Pids],
    B = os:timestamp(),

    print(Number, A, B).

print(Num, A, B) ->
    Microsecs = timer:now_diff(B, A),
    Milliseconds = Microsecs/1000,
    Secs = Milliseconds/1000,
    StanzaPerSec = Num/Secs,
    io:format("### ~p ms ~.2f stanza/sec ~n", [Milliseconds, StanzaPerSec]).

loop(0, _Fun) ->
    ok;
loop(Nr, Fun) ->
    Fun(),
    loop(Nr-1, Fun).

init(fast_xml) ->
    application:ensure_all_started(fast_xml);
init(_) ->
    ok.

to_binary(erlxml) ->
    erlxml:to_binary(?ELEMENT);
to_binary(exml) ->
    exml:to_binary(?ELEMENT);
to_binary(fast_xml) ->
    fxml:element_to_binary(?ELEMENT).

parse(erlxml, Data) ->
    erlxml:parse(Data);
parse(exml, Data) ->
    exml:parse(Data);
parse(fast_xml, Data) ->
    fxml_stream:parse_element(Data).
