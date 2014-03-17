%% @author bucko
%% @doc @todo Add description to test_capnp.


-module(test_capnp).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0]).

test() ->
	Schema = capnp_bootstrap:load_raw_schema("/home/bucko/eclipse-workspace/capnp/data/tests/test1.raw"),
	<<0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,76,0,0,0,0,0,0,0>> = capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestBoringInteger">>, 76}, Schema)),
	<<0,0,0,0,5,0,0,0,0,0,0,0,4,0,0,0,3,0,0,0,0,0,0,0,6,0,0,0,2,0,8,255,246,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0>> = capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestMultipleIntegers">>, 3, 6, 2, 8, -1, -10, 17}, Schema)),
	<<0,0,0,0,3,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,76,0,0,0,0,0,0,0>> = capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestBoringPointer">>, {<<"test1.capnp:TestBoringInteger">>, 76}}, Schema)),
	<<0,0,0,0,10,0,0,0,0,0,0,0,1,0,2,0,255,254,0,0,0,0,0,0,4,0,0,0,0,0,1,0,8,0,0,0,4,0,0,0,0,0,0,0,1,0,0,0,76,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,6,0,0,0,2,0,8,255,246,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0>> = capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestLessBoringPointer">>, {<<"test1.capnp:TestBoringPointer">>, {<<"test1.capnp:TestBoringInteger">>, 76}}, -257, {<<"test1.capnp:TestMultipleIntegers">>, 3, 6, 2, 8, -1, -10, 17}}, Schema)),
	<<0,0,0,0,5,0,0,0,0,0,0,0,0,0,2,0,5,0,0,0,42,0,0,0,5,0,0,0,26,0,0,0,70,111,111,33,0,0,0,0,102,111,111,0,0,0,0,0>> = capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestTextType">>, <<"Foo!">>, <<"foo">>}, Schema)),
	capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestPrimitiveList">>, [true, false, true], [1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11]}, Schema)).

%% ====================================================================
%% Internal functions
%% ====================================================================


