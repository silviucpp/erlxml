compile:
	@make V=0 -C c_src -j 8

clean:
	@make -C c_src clean

ct:
	mkdir -p log
	ct_run -suite integrity_test_SUITE -pa ebin -include include -logdir log