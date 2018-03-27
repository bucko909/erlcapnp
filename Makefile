all:
	make contrib_r
	sleep 1
	mkdir -p ebin
	erlc -pa contrib/uberpt/ebin -o ebin -I include src/capnp_common.erl
	erlc -pa contrib/uberpt/ebin -o ebin -I include src/capnp_schema_wrangle.erl
	erlc -pa contrib/uberpt/ebin -o ebin -I include src/capnp_compile.erl
	erlc -pa contrib/uberpt/ebin -o ebin -I include src/capnp_format.erl
	erlc -pa contrib/uberpt/ebin -o ebin -I include src/capnp_load.erl
	erlc -pa contrib/uberpt/ebin -o ebin -I include src/capnp_records.erl
	erlc -o ebin -I include src/capnp.erl
	erlc -o ebin -I include src/capnp_schema.erl
	erlc -o ebin -I include src/capnp_raw.erl
	erlc -o ebin -I include src/capnp_compile_tests.erl

contrib_r:
	$(MAKE) -C contrib

bootstrap:
	make all
	erl -noshell -pa ebin -eval 'file:write_file("include/capnp_schema.hrl", capnp_compile:header_only("data/schema.raw", capnp_schema, ""))' -s erlang halt
	erl -noshell -pa ebin -eval 'file:write_file("src/capnp_schema.erl", capnp_compile:source_with_include("data/schema.raw", capnp_schema, "include", ""))' -s erlang halt

bench:
	(cd data/tests; capnpc -o/bin/cat test1.capnp > test1.raw)
	erl -noshell -pa ebin -eval 'file:write_file("src/erlcapnp_test1.erl", capnp_compile:source_with_include("data/tests/test1.raw", erlcapnp_test1, "include", "erlcapnp_"))' -s erlang halt
	erl -noshell -pa ebin -eval 'file:write_file("include/erlcapnp_test1.hrl", capnp_compile:header_only("data/tests/test1.raw", erlcapnp_test1, "erlcapnp_"))' -s erlang halt
	erlc -o ebin -I include src/test1_capnp.erl
	erlc -o ebin -I include src/erlcapnp_test1.erl
	erlc -o ebin -I include src/capnp_compile_bench.erl
	erl -pa ./ebin -pa ../ecapnp/ebin -s capnp_compile_bench bench -s erlang halt

test1:
	capnpc -o/bin/cat test.capnp > test.raw
	erl -noshell -pa ebin -eval 'file:write_file("erlcapnp_test.erl", capnp_compile:self_contained_source("test.raw", erlcapnp_test, "erlcapnp_"))' -s erlang halt
