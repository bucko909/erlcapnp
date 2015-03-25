-module(capnp_compile_tests).

-compile(export_all).

fname() -> "data/tests/test1.capnp".

test() ->
	capnp_compile:to_ast(<<"data/tests/test1.capnp:TestCompositeList">>, "data/tests/test1.capnp"),
	do_test(
		{'data/tests/test1.capnp:TestLessBoringPointer', 4, {'data/tests/test1.capnp:TestBoringPointer', {'data/tests/test1.capnp:TestBoringInteger', 7600}}, {'data/tests/test1.capnp:TestMultipleIntegers', 1, 2, 3, 4, 5, 6, 7}},
		<<"(testVar1 = (testVar1 = (testVar1 = 7600)), testVar2 = 4, testVar3 = (testVar1 = 1, testVar2 = 2, testVar3 = 3, testVar4 = 4, testVar5 = 5, testVar6 = 6, testVar7 = 7))">>
	).

do_test(Rec, Expected) ->
	RecName = atom_to_list(element(1, Rec)),
	capnp_compile:to_ast(list_to_binary(RecName), fname()),
	EncodeFun = list_to_atom("envelope_" ++ RecName),
	FriendlyName = "TestLessBoringPointer",
	DataBin = capnp_test:EncodeFun(Rec),
	Pipe = erlang:open_port(
			{spawn, "capnp decode --short " ++ fname() ++ " " ++ FriendlyName}, [
			use_stdio,
			binary,
			line
		]),
	erlang:port_command(Pipe, DataBin),
	receive
		{Pipe, {data, {eol, Line}}} ->
			if
				Line == Expected -> ok;
				true -> exit({bad_output, {Line, Expected}})
			end
	after
		1000 ->
			exit(port_timeout)
	end,
	erlang:port_close(Pipe).
