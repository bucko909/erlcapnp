-module(capnp_records).

-include_lib("capnp.hrl").

-export([
		generate_record_def/4,
		sort/1
	]).

-import(capnp_common, [
		encoder_name/2,
		decoder_name/2,
		full_decoder_name/2,
		record_name/2,
		envelope_fun_name/2,

		field_name/1,

		append/2,
		to_atom/1,
		make_atom/2
	]).

-import(capnp_schema_wrangle, [
		node_name/2,

		schema_lookup/2,
		find_fields/2,
		find_record_fields/2,
		find_notag_fields/2,
		find_notag_groups/2,
		find_notag_data_fields/2,
		find_notag_pointer_fields/2,
		find_tag_fields/2,
		find_anon_union/2,
		discriminant_field/2,
		is_union/2,
		is_group/2,

		is_native_type/1,
		is_nonvoid_native_type/1,
		is_pointer_type/1,
		is_group_type/1
	]).

generate_record_def(Line, TypeId, RecordFields, Schema) ->
	RecordName = record_name(TypeId, Schema),
	{attribute, Line, record, {RecordName, [{typed_record_field, {record_field, Line, {atom, Line, field_name(Info)}}, field_type(Line, TypeId, Info, Schema)} || Info=#field_info{} <- RecordFields]}}.

field_type(Line, _RecordTypeId, #field_info{type=Type=#native_type{type=integer, width=Bits}}, _Schema) ->
	RawRange = 1 bsl Bits,
	{Min, Max} = case is_signed(Type) of
		true ->
			{-(RawRange bsr 1), (RawRange bsr 1) - 1};
		false ->
			{0, RawRange - 1}
	end,
	{type, Line, range, [{integer, Line, Min}, {integer, Line, Max}]};
field_type(Line, _RecordTypeId, #field_info{type=#native_type{type=float}}, _Schema) ->
	{type, Line, float, []};
field_type(Line, _RecordTypeId, #field_info{type=#native_type{type=boolean}}, _Schema) ->
	{type, Line, union, [{atom, Line, true}, {atom, Line, false}]};
field_type(Line, _RecordTypeId, #field_info{type=#native_type{type=void}}, _Schema) ->
	{atom, Line, undefined};
field_type(Line, _RecordTypeId, #field_info{type=#native_type{type=enum, extra=EnumerantNames}}, _Schema) ->
	{type, Line, union, [ {atom, Line, to_atom(Name)} || Name <- EnumerantNames ] };
field_type(Line, _RecordTypeId, #field_info{type=#ptr_type{type=text_or_data}}, _Schema) ->
	or_undefined(Line, {type, Line, union, [{type, Line, iodata, []}]});
field_type(Line, RecordTypeId, #field_info{type=#ptr_type{type=struct, extra={TypeName, _DataLen, _PtrLen}}}, Schema) ->
	case capnp_schema_wrangle:name_to_type_id(TypeName, Schema) of
		RecordTypeId ->
			% Avoid mutual recursion.
			{type, Line, any, []};
		TypeId ->
			case is_union(TypeId, Schema) of
				true ->
					field_type(Line, TypeId, #field_info{type=#group_type{type_id=TypeId}}, Schema);
				false ->
					{type, Line, any, []}
					%or_undefined(Line, {type, Line, record, [make_atom(Line, TypeName)]})
			end
	end;
field_type(Line, _RecordTypeId, #field_info{type=#ptr_type{type=list, extra={primitive, #native_type{type=boolean}}}}, _Schema) ->
	or_preformat(Line, or_undefined(Line, {type, Line, list, [{type, Line, union, [{atom, Line, true}, {atom, Line, false}]}]}));
field_type(Line, RecordTypeId, #field_info{type=#ptr_type{type=list, extra={primitive, Inner}}}, Schema) ->
	or_preformat(Line, or_undefined(Line, {type, Line, list, [field_type(Line, RecordTypeId, #field_info{type=Inner}, Schema)]}));
field_type(Line, _RecordTypeId, #field_info{type=#ptr_type{type=list, extra=#ptr_type{type=text_or_data}}}, _Schema) ->
	or_preformat(Line, or_undefined(Line, {type, Line, list, [or_undefined(Line, {type, Line, iodata, []})]}));
field_type(Line, _RecordTypeId, #field_info{type=#ptr_type{type=list, extra=#ptr_type{type=list, extra=#ptr_type{type=text_or_data}}}}, _Schema) ->
	or_preformat(Line, or_undefined(Line, {type, Line, list, [or_undefined(Line, {type, Line, list, [or_undefined(Line, {type, Line, iodata, []})]})]}));
field_type(Line, _RecordTypeId, #field_info{type=#ptr_type{type=list, extra={struct, #ptr_type{type=struct, extra={_TypeName, _DataLen, _PtrLen}}}}}, _Schema) ->
	% Recursive call is or_undefined, which is not allowed here!
	{type, Line, any, []};
	%or_undefined(Line, {type, Line, list, [{type, Line, record, [make_atom(Line, TypeName)]}]});
field_type(Line, RecordTypeId, #field_info{type=#group_type{type_id=TypeId}}, Schema) ->
	case is_union(TypeId, Schema) of
		true ->
			UnionFields = find_tag_fields(TypeId, Schema),
			{type, Line, union, [ {type, Line, tuple, [make_atom(Line, Name), field_type(Line, RecordTypeId, Field, Schema)]} || Field=#field_info{name=Name} <- UnionFields ]};
		false ->
			{type, Line, any, []}
			%{type, Line, record, [make_atom(Line, record_name(TypeId, Schema))]}
	end;
field_type(Line, _RecordTypeId, #field_info{type=#ptr_type{type=unknown}, default=undefined}, _Schema) ->
	{atom, Line, undefined}.

sort(Records) ->
	shuffle_forms(lists:sort(fun capnp_common:cmp_ignore_line/2, Records)).

shuffle_forms(Forms) ->
	{New, _, []} = shuffle_forms(Forms, [], [], []),
	New.

shuffle_forms([A={attribute, _, T, {N, AV}}|Rest], Seen, Buffer, Acc) when T =:= record; T =:= type ->
	case [ bad || D <- type_depends(AV), not lists:member(D, Seen) ] of
		[] ->
			{New, NewSeen, NewBuffer} = shuffle_forms(Buffer, [{T, N}|Seen], [], []),
			shuffle_forms(Rest, NewSeen, NewBuffer, Acc ++ [A|New]);
		_ ->
			shuffle_forms(Rest, Seen, Buffer ++ [A], Acc)
	end;
shuffle_forms([], Seen, Buffer, Acc) ->
	{Acc, Seen, Buffer}.


type_depends(L) when is_list(L) ->
	lists:append(lists:map(fun type_depends/1, L));
type_depends({integer, _, _}) ->
	[];
type_depends({atom, _, _}) ->
	[];
type_depends({remote_type, _, _}) ->
	[];
type_depends({type, _, record, [{atom, _, N}]}) ->
	[{record, N}];
type_depends({type, _, _, Deps}) ->
	lists:append(lists:map(fun type_depends/1, Deps));
type_depends({typed_record_field, _Field, T}) ->
	type_depends(T);
type_depends({record_field, _, _}) ->
	[];
type_depends({user_type, _, T, Deps}) ->
	[{type, T} | type_depends(Deps)].


or_undefined(Line, Type) ->
	{type, Line, union, [make_atom(Line, undefined), Type]}.

or_preformat(Line, Type) ->
	{type, Line, union, [{remote_type,8,[{atom,8,capnp},{atom,8,capnp_preformat},[]]}, Type]}.

is_signed(#native_type{binary_options=Opts}) ->
	lists:member(signed, Opts).
