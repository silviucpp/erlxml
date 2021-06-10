# erlxml

*erlxml - Erlang XML parsing library based on pugixml*

[![Build Status](https://travis-ci.com/silviucpp/erlxml.svg?branch=master)](https://travis-ci.com/silviucpp/erlxml)
![GitHub](https://img.shields.io/github/license/silviucpp/erlxml) 

# Implementation notes

[pugixml][1] is the fastest dom parser available in c++ based on the benchmarks available [here][2]. The streaming parsing is implemented by 
splitting the stream into independent stanzas which are parsed using pugixml. The algorithm for splitting is pretty fast but in order to keep it simple as possible 
adds some limitations at this moment for the streaming mode:

- not supporting `CDATA`
- not supporting comments with special xml characters inside
- not supporting `DOCTYPE`

All above limitations applies only to streaming mode and not for DOM parsing mode. 

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

- `stanza_limit` - Specify the maximum size a stanza can have. In case the library parses more than this amount of bytes 
without finding a stanza will return and error `{error, {max_stanza_limit_hit, binary()}}`. Example: `{stanza_limit, 65000}`. By default is 0 which means unlimited.

- `strip_non_utf8` - Will strip from attributes values and node values elements all invalid utf8 characters. This is considered 
user input and might have malformed chars. Default is `false`.

### Benchmarks

The benchmark code is inside the benchmark folder. You need to get [exml][3] from Erlang Solutions and [fast_xml][4] from ProcessOne as dependencies 
because all measurements are against this libraries. 

- [exml][3] version used: 3.0.1
- [fast_xml][4] version used: 1.1.30

All tests are run with 3 concurrency levels (how many erlang processes are spawn)

- C1 (concurrency level 1)
- C5 (concurrency level 5)
- C10 (concurrency level 10)

##### DOM parsing

Parse the same stanza defined in `benchmark/benchmark.erl` for 600000 times:

``` erlang
benchmark:bench_parsing(erlxml|exml, 600000, 1|5|10).
```

| Library    | C1 (ms)      |   C5 (ms) | C10 (ms)  |
|:----------:|:------------:|:---------:|:---------:|
| erlxml     |  1942.053    |  511.619  |  522.938  |
| exml       |  1847.879    |  523.957  |  567.417  |
| fast_xml   | 21153.454    | 5584.026  | 5812.703  |

Note: 

- Starting version 3.0.0, [exml][3] improved a lot by replacing Expat with RapidXML, for example before this switch the results
were 26704.861 ms for C1, 7094.698 for C5 and 5812.703 for C10. Now results are comparable with erlxml.
- Difference between erlxml and exml performances because is so small we can say they offer same performance from speed point of view.

##### Generate an XML document from Erlang terms

Encode the same erlang term defined in `benchmark/benchmark.erl` for 600000 times:

``` erlang
benchmark:bench_encoding(erlxml|exml, 600000, 1|5|10).
```

| Library    | C1 (ms)      |   C5 (ms) | C10 (ms)  |
|:----------:|:------------:|:---------:|:---------:|
| erlxml     | 2285.361     |  635.57   |   687.78  |
| exml       | 2035.966     |  571.194  |  603.406  |
| fast_xml   | 1282.113     |  361.599  |  392.007  |

##### Streaming parsing

Will load all stanza's from a file and run the parsing mode over that stanza's for 30000 times (total bytes processed in 
my test is around 1.38 GB) :

```erlang
benchmark_stream:bench(exml, "/Users/silviu/Desktop/example.txt", 30000, 1).
### 32933.493 ms 42.90 MB/sec total bytes processed: 1.38 GB
benchmark_stream:bench(exml, "/Users/silviu/Desktop/example.txt", 30000, 5).
### 9231.714 ms 153.05 MB/sec total bytes processed: 1.38 GB
benchmark_stream:bench(exml, "/Users/silviu/Desktop/example.txt", 30000, 10).
### 9693.043 ms 145.77 MB/sec total bytes processed: 1.38 GB

benchmark_stream:bench(erlxml, "/Users/silviu/Desktop/example.txt", 30000, 1). 
### 10580.888 ms 133.53 MB/sec total bytes processed: 1.38 GB
benchmark_stream:bench(erlxml, "/Users/silviu/Desktop/example.txt", 30000, 5).
### 2662.581 ms 530.66 MB/sec total bytes processed: 1.38 GB
benchmark_stream:bench(erlxml, "/Users/silviu/Desktop/example.txt", 30000, 10).
### 2579.559 ms 547.74 MB/sec total bytes processed: 1.38 GB
```

| Library    | C1 (MB/s)      |   C5 (MB/s) | C10 (MB/s)  |
|:----------:|:--------------:|:-----------:|:-----------:|
| erlxml     | 133.53         |  530.66     |  547.74     |
| exml       |  42.90         |  153.05     |  145.77     |

Notes:

- Starting version 3.0.0, [exml][3] improved by replacing Expat with RapidXML with arount 27% but is still way behind erlxml.


[1]:http://pugixml.org
[2]:http://pugixml.org/benchmark.html
[3]:https://github.com/esl/exml
[4]:https://github.com/processone/fast_xml
