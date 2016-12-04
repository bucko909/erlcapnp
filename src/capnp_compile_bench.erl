-module(capnp_compile_bench).

-compile(export_all).

-include_lib("erlcapnp_test1.hrl"). %erlcapnp

-define(RUNS, 10000).
-define(WARMUP, 1000).

ecapnp_teardown({object, {ref, _, _, _, _, _, {builder, Pid}}, _}) ->
	% ecapnp will leak processes if you're not careful!
	unlink(Pid),
	exit(Pid, kill).

% TestBoringInteger

bench_erlcapnp_integer_encode({X}) ->
	erlcapnp_test1:envelope_erlcapnp_TestBoringInteger(#erlcapnp_TestBoringInteger{testVar1=X}).

bench_ecapnp_integer_encode({X}) ->
	{ok, Message} = ecapnp:set_root(test1_capnp:'TestBoringInteger'()),
	ecapnp:set(testVar1, X, Message),
	Value = ecapnp_message:write(Message),
	ecapnp_teardown(Message),
	Value.

bench_ecapnp_integer_encode_hotloop() ->
	{setup,
		fun
			() ->
				{ok, Message} = ecapnp:set_root(test1_capnp:'TestBoringInteger'()),
				Message
		end,
		fun ecapnp_teardown/1,
		fun ?MODULE:bench_ecapnp_integer_encode_hotloop_/2
	}.

bench_ecapnp_integer_encode_hotloop_(Message, {X}) ->
	ecapnp:set(testVar1, X, Message),
	ecapnp_message:write(Message).

% TestMultipleIntegers

bench_erlcapnp_multiple_integer_encode({X1, X2, X3, X4, X5, X6, X7}) ->
	erlcapnp_test1:envelope_erlcapnp_TestMultipleIntegers(#erlcapnp_TestMultipleIntegers{
				testVar1=X1,
				testVar2=X2,
				testVar3=X3,
				testVar4=X4,
				testVar5=X5,
				testVar6=X6,
				testVar7=X7
	}).

bench_ecapnp_multiple_integer_encode({X1, X2, X3, X4, X5, X6, X7}) ->
	{ok, Message} = ecapnp:set_root(test1_capnp:'TestMultipleIntegers'()),
	ecapnp:set(testVar1, X1, Message),
	ecapnp:set(testVar2, X2, Message),
	ecapnp:set(testVar3, X3, Message),
	ecapnp:set(testVar4, X4, Message),
	ecapnp:set(testVar5, X5, Message),
	ecapnp:set(testVar6, X6, Message),
	ecapnp:set(testVar7, X7, Message),
	Value = ecapnp_message:write(Message),
	ecapnp_teardown(Message),
	Value.

bench_ecapnp_multiple_integer_encode_hotloop() ->
	{setup,
		fun
			() ->
				{ok, Message} = ecapnp:set_root(test1_capnp:'TestMultipleIntegers'()),
				Message
		end,
		fun ecapnp_teardown/1,
		fun ?MODULE:bench_ecapnp_multiple_integer_encode_hotloop_/2
	}.

bench_ecapnp_multiple_integer_encode_hotloop_(Message, {X1, X2, X3, X4, X5, X6, X7}) ->
	ecapnp:set(testVar1, X1, Message),
	ecapnp:set(testVar2, X2, Message),
	ecapnp:set(testVar3, X3, Message),
	ecapnp:set(testVar4, X4, Message),
	ecapnp:set(testVar5, X5, Message),
	ecapnp:set(testVar6, X6, Message),
	ecapnp:set(testVar7, X7, Message),
	ecapnp_message:write(Message).



bench_erlcapnp_multiple_integer_decode(Data) ->
	{#erlcapnp_TestMultipleIntegers{
			testVar1=X1,
			testVar2=X2,
			testVar3=X3,
			testVar4=X4,
			testVar5=X5,
			testVar6=X6,
			testVar7=X7
	}, <<>>} = erlcapnp_test1:decode_erlcapnp_TestMultipleIntegers(Data),
	{X1, X2, X3, X4, X5, X6, X7}.

bench_ecapnp_multiple_integer_decode(Data) ->
	{ok, Parts, <<>>} = ecapnp_message:read(Data),
	{ok, Message} = ecapnp:get_root(test1_capnp:'TestMultipleIntegers'(), Parts),
	X1 = ecapnp:get(testVar1, Message),
	X2 = ecapnp:get(testVar2, Message),
	X3 = ecapnp:get(testVar3, Message),
	X4 = ecapnp:get(testVar4, Message),
	X5 = ecapnp:get(testVar5, Message),
	X6 = ecapnp:get(testVar6, Message),
	X7 = ecapnp:get(testVar7, Message),
	% No need to teardown a reader?
	{X1, X2, X3, X4, X5, X6, X7}.

bench_ecapnp_multiple_integer_decode_singlefield(Data) ->
	{ok, Parts, <<>>} = ecapnp_message:read(Data),
	{ok, Message} = ecapnp:get_root(test1_capnp:'TestMultipleIntegers'(), Parts),
	ecapnp:get(testVar1, Message).

% TestTextType

bench_erlcapnp_text_encode({Text, Data}) ->
	erlcapnp_test1:envelope_erlcapnp_TestTextType(#erlcapnp_TestTextType{testVar1=Text, testVar2=Data}).

bench_ecapnp_text_encode({Text, Data}) ->
	{ok, Message} = ecapnp:set_root(test1_capnp:'TestTextType'()),
	ecapnp:set(testVar1, Text, Message),
	ecapnp:set(testVar2, Data, Message),
	Value = ecapnp_message:write(Message),
	ecapnp_teardown(Message),
	Value.

% TestGroupInUnion

bench_erlcapnp_union_group_encode({Tag, RawTagVal, Tag2, TagVal2}) ->
	TagVal = case Tag of
		unionVar1 ->
			{Raw1, Raw2} = RawTagVal,
			#erlcapnp_TestGroupInUnion_unionVar1{testVar1=Raw1, testVar2=Raw2};
		_ ->
			RawTagVal
	end,
	erlcapnp_test1:envelope_erlcapnp_TestGroupInUnion(#erlcapnp_TestGroupInUnion{''={Tag, TagVal}, union2={Tag2, TagVal2}}).

bench_ecapnp_union_group_encode({Tag, RawTagVal, Tag2, TagVal2}) ->
	{ok, Message} = ecapnp:set_root(test1_capnp:'TestGroupInUnion'()),
	case Tag of
		unionVar1 ->
			{Raw1, Raw2} = RawTagVal,
			ecapnp_set:union({unionVar1, [{testVar1, Raw1}, {testVar2, Raw2}]}, Message);
		_ ->
			ecapnp:set({Tag, RawTagVal}, Message)
	end,
	ecapnp:set(union2, {Tag2, TagVal2}, Message),
	Value = ecapnp_message:write(Message),
	ecapnp_teardown(Message),
	Value.







bench_run(FunD, Input) when is_function(FunD, 0) ->
	{setup, Setup, Teardown, Fun} = FunD(),
	Value = Setup(),
	Return = bench2(Fun, Value, Input),
	Teardown(Value),
	Return;
bench_run(Fun, Input) when is_function(Fun, 1) ->
	bench(Fun, Input).

bench(Fun, Input) ->
	bench(?WARMUP, Fun, Input),
	{Time, _} = timer:tc(fun () -> bench(?RUNS, Fun, Input) end),
	output_time(Fun, Time, ?RUNS).

bench(0, _Fun, _Input) ->
	ok;
bench(N, Fun, Input) ->
	Fun(Input),
	bench(N - 1, Fun, Input).

bench2(Fun, Input1, Input2) ->
	{Time, _} = timer:tc(fun () -> bench2(?RUNS, Fun, Input1, Input2) end),
	output_time(Fun, Time, ?RUNS).

bench2(0, _Fun, _Input1, _Input2) ->
	ok;
bench2(N, Fun, Input1, Input2) ->
	Fun(Input1, Input2),
	bench2(N - 1, Fun, Input1, Input2).

output_time(Fun, Time, Runs) ->
	io:format("Time for ~p: ~ps (~pus * ~p)~n", [erlang:fun_info(Fun, name), Time / 1.0e6, Time/Runs, Runs]).

bench() ->
	bench_run(fun ?MODULE:bench_erlcapnp_integer_encode/1, {-1}),
	bench_run(fun ?MODULE:bench_ecapnp_integer_encode/1, {-1}),
	bench_run(fun ?MODULE:bench_ecapnp_integer_encode_hotloop/0, {-1}),
	bench_run(fun ?MODULE:bench_erlcapnp_multiple_integer_encode/1, {1, 2, 3, 4, 5, 6, 7}),
	bench_run(fun ?MODULE:bench_ecapnp_multiple_integer_encode/1, {1, 2, 3, 4, 5, 6, 7}),
	bench_run(fun ?MODULE:bench_ecapnp_multiple_integer_encode_hotloop/0, {1, 2, 3, 4, 5, 6, 7}),
	bench_run(fun ?MODULE:bench_erlcapnp_multiple_integer_decode/1, <<0,0,0,0,5,0,0,0,0,0,0,0,4,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,3,0,4,5,6,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0>>),
	bench_run(fun ?MODULE:bench_ecapnp_multiple_integer_decode/1, <<0,0,0,0,5,0,0,0,0,0,0,0,4,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,3,0,4,5,6,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0>>),
	bench_run(fun ?MODULE:bench_ecapnp_multiple_integer_decode_singlefield/1, <<0,0,0,0,5,0,0,0,0,0,0,0,4,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,3,0,4,5,6,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0>>),
	bench_run(fun ?MODULE:bench_erlcapnp_text_encode/1, {"Hello", "Goodbye"}),
	bench_run(fun ?MODULE:bench_erlcapnp_text_encode/1, {<<"Hello">>, <<"Goodbye">>}),
	bench_run(fun ?MODULE:bench_erlcapnp_text_encode/1, {"Hello Goodbye", "Goodbye Hello"}),
	bench_run(fun ?MODULE:bench_erlcapnp_text_encode/1, {<<"Hello Goodbye">>, <<"Goodbye Hello">>}),
	%bench_run(fun ?MODULE:bench_ecapnp_text_encode/1, {"Hello", "Goodbye"}),
	bench_run(fun ?MODULE:bench_ecapnp_text_encode/1, {<<"Hello">>, <<"Goodbye">>}),
	bench_run(fun ?MODULE:bench_ecapnp_text_encode/1, {<<"Hello Goodbye">>, <<"Goodbye Hello">>}),
	bench_run(fun ?MODULE:bench_erlcapnp_union_group_encode/1, {unionVar1, {1, 2}, testVar2, [4, 5]}),
	bench_run(fun ?MODULE:bench_erlcapnp_union_group_encode/1, {unionVar2, 1, testVar2, [4, 5]}),
	bench_run(fun ?MODULE:bench_erlcapnp_union_group_encode/1, {unionVar2, 1, testVar1, 3}),
	bench_run(fun ?MODULE:bench_ecapnp_union_group_encode/1, {unionVar2, 1, testVar2, [4, 5]}),
	bench_run(fun ?MODULE:bench_ecapnp_union_group_encode/1, {unionVar2, 1, testVar1, 3}).
