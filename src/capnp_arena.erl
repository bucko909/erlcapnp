%% @author bucko
%% @doc @todo Add description to capnp_arena.


-module(capnp_arena).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-record(arena, {
		left_length=0 :: integer(),
		left=0 :: any(),
		right_length=0 :: integer(),
		right=0 :: any()
	}).

new() ->
	#arena{}.

%allocate(Arena#arena{left_length=LL, right_length=RL, right=Right}, Size) ->
%	Arena#arena{right_length=RL+Size, right=#arena{left_length=RL, left=Right, right_length=Size}}.
	

%% ====================================================================
%% Internal functions
%% ====================================================================


