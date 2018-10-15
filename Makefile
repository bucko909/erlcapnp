all:
	make contrib_r
	sleep 1
	mkdir -p ebin
	erlc +debug_info -pa contrib/uberpt/ebin -o ebin -I include src/capnp_common.erl
	erlc +debug_info -pa contrib/uberpt/ebin -o ebin -I include src/capnp_schema_wrangle.erl
	erlc +debug_info -pa contrib/uberpt/ebin -o ebin -I include src/capnp_compile.erl
	erlc +debug_info -pa contrib/uberpt/ebin -o ebin -I include src/capnp_format.erl
	erlc +debug_info -pa contrib/uberpt/ebin -o ebin -I include src/capnp_load.erl
	erlc +debug_info -pa contrib/uberpt/ebin -o ebin -I include src/capnp_records.erl
	erlc +debug_info -o ebin -I include src/capnp.erl
	erlc +debug_info -o ebin -I include src/capnp_schema.erl
	erlc +debug_info -o ebin -I include src/capnp_raw.erl
	erlc +debug_info -o ebin -I include src/capnp_compile_tests.erl

contrib_r:
	$(MAKE) -C contrib

bootstrap:
	make all
	erl -noshell -pa ebin -eval 'file:write_file("include/capnp_schema.hrl", capnp_compile:header_only("data/schema.raw", capnp_schema, ""))' -s erlang halt
	erl -noshell -pa ebin -eval 'file:write_file("src/capnp_schema.erl", capnp_compile:source_with_include("data/schema.raw", capnp_schema, "include", ""))' -s erlang halt

bench: test1
	erlc +debug_info -o ebin -I include src/capnp_compile_bench.erl
	erl -pa ./ebin -pa ../ecapnp/ebin -s capnp_compile_bench bench -s erlang halt

test1:
	(cd data/tests; capnpc -o/bin/cat test1.capnp > test1.raw)
	erl -noshell -pa ebin -eval 'file:write_file("src/erlcapnp_test1.erl", capnp_compile:source_with_include("data/tests/test1.raw", erlcapnp_test1, "include", "erlcapnp_"))' -s erlang halt
	erl -noshell -pa ebin -eval 'file:write_file("include/erlcapnp_test1.hrl", capnp_compile:header_only("data/tests/test1.raw", erlcapnp_test1, "erlcapnp_"))' -s erlang halt
	erlc +debug_info -o ebin -I include src/erlcapnp_test1.erl
