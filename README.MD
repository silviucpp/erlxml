# erlxml

*erlxml - Erlang XML parsing library based on pugixml* 

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
without finding a stanza will return and error `{error, max_stanza_limit_hit}`. Example: `{stanza_limit, 65000}`. By default is 0 which means unlimited.

- `strip_non_utf8` - Will strip from attributes values and node values elements all invalid utf8 characters. This is considered 
user input and might have malformed chars. Default is `false`.

### Benchmarks

The benchmark code is inside the benchmark folder. You need to get `exml` from Erlang Solutions as dependencies 
because all measurements are against this library. 

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
| erlxml     |  2082.619    |  552.734  |  593.192  |
| exml       | 26704.861    | 7094.698  | 6805.855  |

##### Generate an XML document from Erlang terms

Encode the same erlang term defined in `benchmark/benchmark.erl` for 600000 times:

``` erlang
benchmark:bench_encoding(erlxml|exml, 600000, 1|5|10).
```

| Library    | C1 (ms)      |   C5 (ms) | C10 (ms)  |
|:----------:|:------------:|:---------:|:---------:|
| erlxml     | 2471.132     |  662.597  |  711.514  |
| exml       | 6687.634     | 1853.416  | 1765.119  |


##### Streaming parsing

Will load all stanza's from a file and run the parsing mode over that stanza's for 30000 times (total bytes processed in 
my test is around 1.38 GB) :

```erlang
benchmark_stream:bench(exml, "/Users/silviu/Desktop/example.txt", 30000, 1).
### 44522.645 ms 31.73 MB/sec total bytes processed: 1.38 GB 
benchmark_stream:bench(exml, "/Users/silviu/Desktop/example.txt", 30000, 5).
### 12297.572 ms 114.89 MB/sec total bytes processed: 1.38 GB 
benchmark_stream:bench(exml, "/Users/silviu/Desktop/example.txt", 30000, 10).
### 12535.643 ms 112.71 MB/sec total bytes processed: 1.38 GB 

benchmark_stream:bench(erlxml, "/Users/silviu/Desktop/example.txt", 30000, 1). 
### 9898.998 ms 142.73 MB/sec total bytes processed: 1.38 GB 
benchmark_stream:bench(erlxml, "/Users/silviu/Desktop/example.txt", 30000, 5).
### 2620.594 ms 539.16 MB/sec total bytes processed: 1.38 GB 
benchmark_stream:bench(erlxml, "/Users/silviu/Desktop/example.txt", 30000, 10).
### 2529.72 ms 558.53 MB/sec total bytes processed: 1.38 GB 
```

| Library    | C1 (MB/s)      |   C5 (MB/s) | C10 (MB/s)  |
|:----------:|:--------------:|:-----------:|:-----------:|
| erlxml     | 142.73         |  539.16     |  558.53     |
| exml       | 31.73          |  114.89     | 112.71      |

[1]:http://pugixml.org
[2]:http://pugixml.org/benchmark.html
[3]:https://github.com/esl/exml