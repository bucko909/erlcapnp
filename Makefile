all:
	make contrib_r
	sleep 1
	mkdir -p ebin
	erlc -pa contrib/uberpt/ebin -o ebin -I include src/capnp_compile.erl
	erlc -o ebin -I include src/capnp_schema.erl
	erlc -o ebin -I include src/capnp_raw.erl
	erlc -o ebin -I include src/capnp_compile_tests.erl
	erlc -o ebin -I include src/test_capnp.erl

contrib_r:
	$(MAKE) -C contrib

bootstrap:
	make all
	erl -noshell -pa ebin -eval 'file:write_file("include/capnp_schema.hrl", capnp_compile:header_only("data/schema.raw", capnp_schema))' -s erlang halt
	erl -noshell -pa ebin -eval 'file:write_file("src/capnp_schema.erl", capnp_compile:source_with_include("data/schema.raw", capnp_schema, "include"))' -s erlang halt
