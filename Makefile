REBAR ?= rebar3

compile:
	@make V=0 -C c_src -j 8

clean:
	@make -C c_src clean

bench_encoding:
	$(REBAR) as bench compile
	erl -pa _build/bench/lib/*/ebin -noshell \
    	-eval "lists:foreach(	\
              fun(Module) ->	\
                  lists:foreach(fun(N) -> benchmark:bench_encoding(Module, 600000, N) end, [1, 5, 10]) \
              end, [erlxml, exml, fast_xml])" \
    -eval "init:stop()."

bench_parsing:
	$(REBAR) as bench compile
	erl -pa _build/bench/lib/*/ebin -noshell \
    	-eval "lists:foreach(	\
              fun(Module) ->	\
                  lists:foreach(fun(N) -> benchmark:bench_parsing(Module, 600000, N) end, [1, 5, 10]) \
              end, [erlxml, exml, fast_xml])" \
    -eval "init:stop()."

bench_streaming:
	$(REBAR) as bench compile
	erl -pa _build/bench/lib/*/ebin -noshell \
		-eval "lists:foreach(	\
			  fun(Module) ->	\
				  lists:foreach(fun(N) -> benchmark_stream:bench(Module, \"test/data/stream.txt\", 60000, N) end, [1, 5, 10]) \
			  end, [erlxml, exml, fast_xml])" \
	-eval "init:stop()."
