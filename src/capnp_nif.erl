-module(capnp_nif).

-on_load(init/0).

-compile([export_all]).

init() ->
	ok = erlang:load_nif("./capnp_nif", 0).

new_message_builder() ->
	erlang:error(nif_not_loaded).

lol() ->
	erlang:error(nif_not_loaded).

to_binary(_ReaderOrBuilder) ->
	erlang:error(nif_not_loaded).

initRoot_TestTextType(_Builder) ->
	erlang:error(nif_not_loaded).

set_TestTextType_testVar1(_TestTextType, _String) ->
	erlang:error(nif_not_loaded).

set_TestTextType_testVar2(_TestTextType, _String) ->
	erlang:error(nif_not_loaded).

bench2(X) ->
	M=capnp_nif:new_message_builder(),
	capnp_nif:initRoot_TestTextType(M),
	capnp_nif:to_binary(M),
	bench2(X, M).
bench2(0, _) -> ok;
bench2(X, M2) ->
	M=capnp_nif:new_message_builder(),
	capnp_nif:initRoot_TestTextType(M),
	capnp_nif:to_binary(M),
	2,
	bench2(X-1,M2).

bench(0) -> ok;
bench(X) ->
	M=capnp_nif:new_message_builder(),
	capnp_nif:initRoot_TestTextType(M),
	capnp_nif:to_binary(M),
	bench(X-1).

benchlol(0) -> ok;
benchlol(X) ->
	capnp_nif:lol(),
	benchlol(X-1).
