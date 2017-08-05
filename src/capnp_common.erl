-module(capnp_common).

-include_lib("capnp.hrl").
-include_lib("capnp_schema.hrl").

-export([
		encoder_name/2,
		decoder_name/2,
		full_decoder_name/2,
		record_name/2,
		envelope_fun_name/2,

		field_name/1,

		append/2,
		to_list/1,
		to_atom/1,
		make_atom/2,

		cmp_ignore_line/2
	]).

-import(capnp_schema_wrangle, [
		node_name/2
	]).

encoder_name(TypeId, Schema) ->
	{TypeName, _, _} = node_name(TypeId, Schema),
	to_atom(append("encode_", TypeName)).

decoder_name(TypeId, Schema) ->
	{TypeName, _, _} = node_name(TypeId, Schema),
	to_atom(append("internal_decode_", TypeName)).

full_decoder_name(TypeId, Schema) ->
	{TypeName, _, _} = node_name(TypeId, Schema),
	to_atom(append("decode_", TypeName)).

record_name(TypeId, Schema) ->
	{TypeName, _, _} = node_name(TypeId, Schema),
	to_atom(TypeName).

envelope_fun_name(TypeId, Schema) ->
	{TypeName, _, _} = node_name(TypeId, Schema),
	to_atom(append("envelope_", TypeName)).

field_name(#field_info{name=FieldName}) ->
	to_atom(FieldName).


append(A, B) ->
	to_list(A) ++ to_list(B).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_binary(A) -> binary_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A);
to_list(A) when is_list(A) -> A.

to_atom(Val) ->
	list_to_atom(to_list(Val)).

make_atom(Line, Name) ->
	{atom, Line, to_atom(Name)}.

cmp_ignore_line(A, B) ->
	setelement(2, A, 0) =< setelement(2, B, 0).
