-module(capnp_nif).

-on_load(init/0).

-compile([export_all]).

init() ->
	FName = "/tmp/" ++ atom_to_list(?MODULE) ++ "_" ++ integer_to_list(erlang:unique_integer()),
	file:copy("./capnp_nif.so", FName ++ ".so"),
	ok = erlang:load_nif(FName, 0),
	file:delete(FName ++ ".so"),
	ok.

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

initRoot_TestMultipleIntegers(_Builder) ->
	erlang:error(nif_not_loaded).

set_TestMultipleIntegers_testVar1(_TestTextType, _Int) ->
	erlang:error(nif_not_loaded).

set_TestMultipleIntegers_testVar2(_TestTextType, _Int) ->
	erlang:error(nif_not_loaded).

bench2(X) ->
	M=new_message_builder(),
	initRoot_TestTextType(M),
	to_binary(M),
	bench2(X, M).
bench2(0, _) -> ok;
bench2(X, M2) ->
	M=new_message_builder(),
	initRoot_TestTextType(M),
	to_binary(M),
	2,
	bench2(X-1,M2).

bench(0) -> ok;
bench(X) ->
	M=new_message_builder(), % 250ns
	initRoot_TestTextType(M), % 1250ns
	to_binary(M), % 500ns
	bench(X-1).

bench_set(0) -> ok;
bench_set(X) ->
	M=new_message_builder(),
	TTT=initRoot_TestTextType(M),
	set_TestTextType_testVar1(TTT, [<<"Foooooooo">>]),
	set_TestTextType_testVar2(TTT, [<<"Bar">>]),
	to_binary(M),
	bench_set(X-1).

bench_set_TestMultipleIntegers(0) -> ok;
bench_set_TestMultipleIntegers(X) ->
	M=new_message_builder(),
	TTT=initRoot_TestMultipleIntegers(M),
	set_TestMultipleIntegers_testVar1(TTT, 1),
	set_TestMultipleIntegers_testVar2(TTT, 2),
	to_binary(M),
	bench_set_TestMultipleIntegers(X-1).

bench_set_TestMultipleIntegers2(X) ->
	M=new_message_builder(),
	TTT=initRoot_TestMultipleIntegers(M),
	bench_set_TestMultipleIntegers_(TTT, X).
bench_set_TestMultipleIntegers_(TTT, 0) -> ok;
bench_set_TestMultipleIntegers_(TTT, X) ->
	set_TestMultipleIntegers_testVar1(TTT, 1),
	set_TestMultipleIntegers_testVar2(TTT, 2),
	bench_set_TestMultipleIntegers_(TTT, X-1).

benchlol(0) -> ok;
benchlol(X) ->
	lol(),
	benchlol(X-1).

raise() -> 0=1.

test(A, B) ->
	M=new_message_builder(),
	TTT=initRoot_TestTextType(M),
	set_TestTextType_testVar1(TTT, A),
	set_TestTextType_testVar2(TTT, B),
	to_binary(M).

test_TestMultipleIntegers(A, B) ->
	M=new_message_builder(),
	TTT=initRoot_TestMultipleIntegers(M),
	set_TestMultipleIntegers_testVar1(TTT, A),
	set_TestMultipleIntegers_testVar2(TTT, B),
	to_binary(M).
