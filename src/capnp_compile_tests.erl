-module(capnp_compile_tests).

-compile(export_all).

fname() -> "data/tests/test1.capnp".

test() ->
	do_test(
		{'TestLessBoringPointer', 4, {'TestBoringPointer', {'TestBoringInteger', 7600}}, {'TestMultipleIntegers', 1, 2, 3, 4, 5, 6, 7}},
		<<"(testVar1 = (testVar1 = (testVar1 = 7600)), testVar2 = 4, testVar3 = (testVar1 = 1, testVar2 = 2, testVar3 = 3, testVar4 = 4, testVar5 = 5, testVar6 = 6, testVar7 = 7))">>
	),
	do_test(
		{'TestTextType', "FOOOOOO", "BAAARRRRRRRRR"},
		<<"(testVar1 = \"FOOOOOO\", testVar2 = \"BAAARRRRRRRRR\")">>
	),
	do_test(
		{'TestPrimitiveList',
			[true, false, true, true, true, false, false, true, false],
			[1, -2, 3, -4, 5, -6, 7, -8, 9],
			[1, -2, 3, -4, 5, -6, 7, -8, 9],
			[1, -2, 3, -4, 5, -6, 7, -8, 9],
			[1, -2, 3, -4, 5, -6, 7, -8, 9]
		},
		<<"(",
			"testVar1 = [true, false, true, true, true, false, false, true, false], ",
			"testVar2 = [1, -2, 3, -4, 5, -6, 7, -8, 9], ",
			"testVar3 = [1, -2, 3, -4, 5, -6, 7, -8, 9], ",
			"testVar4 = [1, -2, 3, -4, 5, -6, 7, -8, 9], ",
			"testVar5 = [1, -2, 3, -4, 5, -6, 7, -8, 9]",
		")">>
	),
	do_test(
		{'TestCompositeList',
			[{'TestMultipleIntegers', X, X, X, X, X, X, X} || X <- lists:seq(1, 10)],
			[{'TestLessBoringPointer', X, {'TestBoringPointer', {'TestBoringInteger', X}}, {'TestMultipleIntegers', X, X, X, X, X, X, X}} || X <- [1, 2]]},
		<<"(",
			"testVar1 = [",
				(list_to_binary(join([ "(" ++ join([ "testVar" ++ integer_to_list(VN) ++ " = " ++ integer_to_list(N) || VN <- lists:seq(1, 7)], ", ") ++ ")" || N <- lists:seq(1, 10) ], ", ")))/binary,
			"], ",
			"testVar2 = [",
				(list_to_binary(join([
					"(" ++
						"testVar1 = (testVar1 = (testVar1 = " ++ integer_to_list(N) ++ ")), " ++
						"testVar2 = " ++ integer_to_list(N) ++ ", " ++
						"testVar3 = (" ++ join([ "testVar" ++ integer_to_list(VN) ++ " = " ++ integer_to_list(N) || VN <- lists:seq(1, 7)], ", ") ++ ")" ++
					")"
					|| N <- [1, 2]
				], ", ")))/binary,
			"])"
		>>
	),
	do_test(
		{'TestGroup', -2, {'TestGroup.group1', 1, -3}},
		<<"(group1 = (testVar1 = 1, testVar2 = -3), testVar3 = -2)">>
	),
	do_test(
		{'TestGroupInUnion',
			{unionVar1, {'TestGroupInUnion.unionVar1', 7, 8}},
			{testVar1, 1}
		},
		<<"(unionVar1 = (testVar1 = 7, testVar2 = 8), union2 = (testVar1 = 1))">>
	),
	do_test(
		{'TestGroupInUnion',
			{unionVar2, -4},
			{testVar2, [-5, 2, 3]}
		},
		<<"(unionVar2 = -4, union2 = (testVar2 = [-5, 2, 3]))">>
	),
	do_test(
		{'TestGroupInUnion',
			{unionVar3, -4},
			{testVar2, [-2, 0, 2]}
		},
		<<"(unionVar3 = -4, union2 = (testVar2 = [-2, 0, 2]))">>
	),
	io:format("Tests passed.~n").

join([H], _) -> H;
join([H|T], J) -> H ++ J ++ join(T, J);
join([], _) -> "".

do_test(Rec, Expected) ->
	RecName = atom_to_list(element(1, Rec)),
	capnp_compile:load_directly(fname(), capnp_test, list_to_binary(RecName)),
	EncodeFun = list_to_atom("envelope_" ++ RecName),
	case re:run(RecName, ".*:(.*)", [{capture, all_but_first, list}]) of
		{match, [FriendlyName]} ->
			pass;
		nomatch ->
			FriendlyName = RecName
	end,
	DataBin = capnp_test:EncodeFun(Rec),
	Pipe = erlang:open_port(
			{spawn, "capnp decode --short " ++ fname() ++ " " ++ FriendlyName}, [
			use_stdio,
			binary,
			{line, 1000000}
		]),
	erlang:port_command(Pipe, DataBin),
	receive
		{Pipe, {data, {eol, Line}}} ->
			if
				Line == Expected -> ok;
				true -> io:format("Got:~n~10000000p~nFrom:~n~1000000p~n", [Line, DataBin]), exit({bad_output, {Line, DataBin, Expected}})
			end
	after
		1000 ->
			exit(port_timeout)
	end,
	erlang:port_close(Pipe).
