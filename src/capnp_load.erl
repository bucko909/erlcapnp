-module(capnp_load).

-export([
		load_raw/2
	]).

-include_lib("capnp_schema.hrl").
-include_lib("capnp.hrl").

make_objdict(#'CodeGeneratorRequest'{nodes=Nodes}) ->
	ById = dict:from_list([{Id, Node} || Node=#'Node'{id=Id} <- Nodes ]),
	NameToId = dict:from_list([{Name, Id} || #'Node'{id=Id, displayName=Name} <- Nodes ]),
	#capnp_context{
		by_id = ById,
		name_to_id = NameToId
	}.

massage_names(Schema=#'CodeGeneratorRequest'{nodes=Nodes}, Filename, Prefix) ->
	MassagedNodes = [ X#'Node'{displayName=massage_name(Name, Filename, Prefix)} || X=#'Node'{displayName=Name} <- Nodes ],
	Schema#'CodeGeneratorRequest'{nodes=MassagedNodes}.

massage_name(Name, Filename, Prefix) ->
	String = binary_to_list(Name),
	case {String == Filename, lists:prefix(Filename, String)} of
		{false, true} ->
			list_to_binary(fix_periods(Prefix ++ lists:sublist(String, length(Filename) + 2, length(String) - length(Filename) - 1)));
		_ ->
			list_to_binary(fix_periods(Prefix ++ String))
	end.

fix_periods(String) ->
	lists:map(fun ($.) -> $_; (X) -> X end, String).

load_raw(Filename, Prefix) ->
	{ok, Raw} = file:read_file(Filename),
	Base = filename:rootname(filename:basename(Filename)),
	{Schema, <<>>} = capnp_schema:'decode_CodeGeneratorRequest'(Raw),
	Renamed = massage_names(Schema, Base ++ ".capnp", Prefix),
	make_objdict(Renamed).
