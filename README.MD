# erlxml

*erlxml - Erlang XML parsing library based on pugixml*

[![Build Status](https://app.travis-ci.com/silviucpp/erlxml.svg?branch=master)](https://travis-ci.com/github/silviucpp/erlxml)
[![GitHub](https://img.shields.io/github/license/silviucpp/erlxml)](https://github.com/silviucpp/erlxml/blob/master/LICENSE)

# Implementation notes

[pugixml][1] is the fastest dom parser available in c++ based on the benchmarks available [here][2]. The streaming parser works by dividing the
stream into independent stanzas, which are then processed using pugixml. While the splitting algorithm is quite fast, it is designed for simplicity,
which currently imposes some limitations on the streaming mode:

- Does not support `CDATA`
- Does not support comments containing special XML characters
- Does not support `DOCTYPE` declarations

All of the above limitations apply only to streaming mode and not to DOM parsing mode. 

### Getting starting:

##### DOM parsing

```erlang
erlxml:parse(<<"<foo attr1='bar'>Some Value</foo>">>).
```

Which results in

```erlang
{ok,{xmlel,<<"foo">>,
           [{<<"attr1">>,<<"bar">>}],
           [{xmlcdata,<<"Some Value">>}]}}
```

##### Generate an XML document from Erlang terms

```erlang
Xml = {xmlel,<<"foo">>,
    [{<<"attr1">>,<<"bar">>}],  % Attributes
    [{xmlcdata,<<"Some Value">>}]   % Elements
},
erlxml:to_binary(Xml).
```

Which results in

```erlang
<<"<foo attr1=\"bar\">Some Value</foo>">>
```

##### Streaming parsing

```erlang
Chunk1 = <<"<stream><foo attr1=\"bar">>,
Chunk2 = <<"\">Some Value</foo></stream>">>,
{ok, Parser} = erlxml:new_stream(),
{ok,[{xmlstreamstart,<<"stream">>,[]}]} = erlxml:parse_stream(Parser, Chunk1),
Rs = erlxml:parse_stream(Parser, Chunk2),
{ok,[{xmlel,<<"foo">>,
        [{<<"attr1">>,<<"bar">>}],
        [{xmlcdata,<<"Some Value">>}]},
     {xmlstreamend,<<"stream">>}]} = Rs.
```

### Options 

When you create a stream using `new_stream/1` you can specify the following options:

- `stanza_limit` - Specify the maximum size a stanza can have. In case the library parses more than this number of bytes 
without finding a stanza will return and error `{error, {max_stanza_limit_hit, binary()}}`. Example: `{stanza_limit, 65000}`. By default, it is 0 that means unlimited.

- `strip_non_utf8` - Will strip from attributes values and node values elements all invalid utf8 characters. This is considered 
user input and might have malformed chars. Default is `false`.

### Benchmarks

The benchmark code is inside the benchmark `folder`. The performances are compared against:

- [exml][3] version used: 3.4.1
- [fast_xml][4] version used: 1.1.55

All tests are running with three different concurrency levels (how many erlang processes are spawn)

- C1 (concurrency level 1)
- C5 (concurrency level 5)
- C10 (concurrency level 10)

##### DOM parsing

Parse the same stanza defined in `benchmark/benchmark.erl` for 600000 times:

```sh
make bench_encoding
```

| Library    | C1 (ms)      |   C5 (ms) | C10 (ms)  |
|:----------:|:------------:|:---------:|:---------:|
| erlxml     |  1875.128    |  417.368  |  315.65   |
| exml       |  2417.334    |  578.226  |  407.516  |
| fast_xml   | 24159.517    | 5854.817  | 4007.837  |

Note: 

- Starting version 3.0.0, [exml][3] saw significant improvements by replacing Expat with RapidXML.
- `erlxml` delivers the best performance, followed by `exml`, while `fast_xml` performs the worst (huge difference).

##### Generate an XML document from Erlang terms

Encode the same erlang term defined in `benchmark/benchmark.erl` for 600000 times:

```sh
make bench_encoding
```

|   Library   |   C1 (ms)    | C5 (ms) | C10 (ms)  |
|:-----------:|:------------:|:-------:|:---------:|
|  `erlxml`   |   1786.683   | 448.943 |  312.348  |
|   `exml`    |   1383.223   | 334.816 |  236.011  |
| `fast_xml`  |   1073.679   | 274.55  |  198.719  |

Note:

- `fast_xml` delivers the best performance, followed by `exml`, and `erlxml`.

##### Streaming parsing

Test is located in `benchmark/benchmark_stream.erl`, and will load all stanza's from `test/data/stream.txt` and run the parsing mode over that stanza's for 60000 times:

```sh
make bench_streaming
```

```sh
### engine: erlxml concurrency: 1 -> 2337.112 ms 193.81 MB/sec total bytes processed: 452.96 MB
### engine: erlxml concurrency: 5 -> 598.737 ms 756.52 MB/sec total bytes processed: 452.96 MB
### engine: erlxml concurrency: 10 -> 407.379 ms 1.09 GB/sec total bytes processed: 452.96 MB
### engine: exml concurrency: 1 -> 11790.975 ms 38.42 MB/sec total bytes processed: 452.96 MB
### engine: exml concurrency: 5 -> 2552.339 ms 177.47 MB/sec total bytes processed: 452.96 MB
### engine: exml concurrency: 10 -> 1840.267 ms 246.14 MB/sec total bytes processed: 452.96 MB
### engine: fast_xml concurrency: 1 -> 22677.758 ms 19.97 MB/sec total bytes processed: 452.96 MB
### engine: fast_xml concurrency: 5 -> 5184.096 ms 87.37 MB/sec total bytes processed: 452.96 MB
### engine: fast_xml concurrency: 10 -> 3854.402 ms 117.52 MB/sec total bytes processed: 452.96 MB 
```

|   Library   | C1 (MB/s)      | C5 (MB/s) | C10 (MB/s) |
|:-----------:|:--------------:|:---------:|:----------:|
|   erlxml    | 193.81         |  756.52   |   1090     |
|    exml     |  38.42         |  177.47   |    246     |
| fast_xml    |  19.97         |   87.37   |    117     |

Notes:

- `erlxml` is the clear winner.

[1]:http://pugixml.org
[2]:http://pugixml.org/benchmark.html
[3]:https://github.com/esl/exml
[4]:https://github.com/processone/fast_xml
