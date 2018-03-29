%% @author bucko
%% @doc @todo Add description to capnp_compile.


-module(capnp_compile).

-compile({parse_transform, uberpt}).

-include_lib("capnp.hrl").
-include_lib("capnp_raw.hrl").
-include_lib("capnp_schema.hrl").

-export([
		to_ast/1,
		load_directly/3,

		self_contained_source/3,
		source_with_include/4,
		header_only/3,

		output_source_with_include/3,
		output_source_with_include/4,
		output_header/2,
		output_header/3
	]).

-import(capnp_common, [
		encoder_name/2,
		decoder_name/2,
		full_decoder_name/2,
		record_name/2,
		envelope_fun_name/2,

		field_name/1,

		append/2,
		to_list/1,
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
		flatten_notag_fields/2,
		discriminant_field/2,
		is_union/2,
		is_group/2,

		is_native_type/1,
		is_nonvoid_native_type/1,
		is_pointer_type/1,
		is_group_type/1
	]).

% Formatters.
self_contained_source(SchemaFile, ModuleName, Prefix) ->
	capnp_format:self_contained_source(
		to_ast(capnp_load:load_raw(SchemaFile, Prefix)),
		ModuleName).

source_with_include(SchemaFile, ModuleName, Path, Prefix) ->
	capnp_format:source_with_include(
		to_ast(capnp_load:load_raw(SchemaFile, Prefix)),
		ModuleName,
		Path).

header_only(SchemaFile, ModuleName, Prefix) ->
	capnp_format:header_only(
		to_ast(capnp_load:load_raw(SchemaFile, Prefix)),
		ModuleName).

% Outputters
output_source_with_include(SchemaFile, ModuleName, Path) ->
	output_source_with_include(SchemaFile, ModuleName, Path, "").
output_source_with_include(SchemaFile, ModuleName, Path, Prefix) ->
	io:format("~s", [source_with_include(SchemaFile, ModuleName, Path, Prefix)]).

output_header(SchemaFile, ModuleName) ->
	output_header(SchemaFile, ModuleName, "").
output_header(SchemaFile, ModuleName, Prefix) ->
	io:format("~s", [header_only(SchemaFile, ModuleName, Prefix)]).


% Convenience method. Might be useful for testing.
load_directly(SchemaFile, ModuleName, Prefix) ->
	% We go via source to make sure we didn't generate anything kooky.
	Source = self_contained_source(SchemaFile, ModuleName, Prefix),
	{ok, Tokens, _} = erl_scan:string(Source),
	Forms = split_forms(Tokens, []),
	{ok, ModuleName, BinData, []} = compile:forms(Forms, [debug_info, return]),
	code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", BinData).

split_forms([Dot={dot,_}|Rest], Acc) ->
	{ok, Form} = erl_parse:parse_form(lists:reverse([Dot|Acc])),
	[Form | split_forms(Rest, [])];
split_forms([Other|Rest], Acc) ->
	split_forms(Rest, [Other|Acc]);
split_forms([], []) ->
	[].

% Main compiler entry point.
to_ast(Schema=#capnp_context{by_id=ById}) ->
	Tasks = [ {generate_name, Name} || {_, #'Node'{''={struct, _}, displayName=Name}} <- dict:to_list(ById) ],
	to_ast(
		[
			generate_massage_bool_list,
			generate_decode_envelope_fun,
			generate_follow_struct_pointer
			| Tasks
		],
		sets:new(),
		[],
		[],
		Schema
	).


to_ast([], _Done, Recs, Funs, _Schema) ->
	{
		capnp_records:sort(Recs),
		lists:sort(fun capnp_common:cmp_ignore_line/2, Funs)
	};
to_ast([Job|Rest], Done, Recs, Funs, Schema) ->
	case sets:is_element(Job, Done) of
		true ->
			to_ast(Rest, Done, Recs, Funs, Schema);
		false ->
			{NewRecs, NewFuns, NewJobs} = do_job(Job, Schema),
			to_ast(NewJobs ++ Rest, sets:add_element(Job, Done), NewRecs ++ Recs, NewFuns ++ Funs, Schema)
	end.

do_job({generate_name, TypeName}, Schema) ->
	TypeId = dict:fetch(TypeName, Schema#capnp_context.name_to_id),
	{[], [], [{generate, TypeId}]};
do_job({generate, TypeId}, Schema) ->
	generate_basic(TypeId, Schema);
do_job({generate_record, TypeId}, Schema) ->
	Line = 0,
	SortedFields = find_record_fields(TypeId, Schema),
	RecDef = capnp_records:generate_record_def(Line, TypeId, SortedFields, Schema),
	{[RecDef], [], []};
do_job({generate_decode, TypeId}, Schema) ->
	Line = 0,
	SortedDataFields = find_notag_data_fields(TypeId, Schema),
	SortedPtrFields = find_notag_pointer_fields(TypeId, Schema),
	Groups = find_anon_union(TypeId, Schema) ++ find_notag_groups(TypeId, Schema),
	FunDef = generate_decode_fun(Line, TypeId, SortedDataFields, SortedPtrFields, Groups, Schema),
	EnvFunDef = generate_full_decoder_fun(Line, TypeId, Schema),
	{[], [FunDef, EnvFunDef], []};
do_job({generate_encode, TypeId}, Schema) ->
	Line = 0,
	FunDef = generate_encode_fun(Line, TypeId, Schema),
	{[], [FunDef], []};
do_job({generate_union_encode, TypeId}, Schema) ->
	Line = 0,
	UnionFields = find_tag_fields(TypeId, Schema),
	FunDef = generate_union_encode_fun(Line, TypeId, UnionFields, Schema),
	{[], [FunDef], []};
do_job({generate_envelope, TypeId}, Schema) ->
	Line = 0,
	FunDef = generate_envelope_fun(Line, TypeId, Schema),
	{[], [FunDef], []};
do_job({generate_text, TextType}, _Schema) ->
	% Needed when we have a list of text.
	generate_text(TextType);
do_job(generate_decode_far_pointer, _Schema) ->
	generate_decode_far_pointer();
do_job(generate_follow_struct_pointer, _Schema) ->
	generate_follow_struct_pointer();
do_job(generate_massage_bool_list, _Schema) ->
	generate_massage_bool_list();
do_job({generate_follow_text_pointer, Type}, _Schema) ->
	generate_follow_text_pointer(Type);
do_job(generate_follow_struct_list_pointer, _Schema) ->
	generate_follow_struct_list_pointer();
do_job({generate_follow_primitive_list_pointer, Type}, _Schema) ->
	Line = 0,
	generate_follow_primitive_list_pointer(Line, Type);
do_job(generate_decode_envelope_fun, _Schema) ->
	generate_decode_envelope_fun().


generate_basic(TypeId, Schema) ->
	IsGroup = is_group(TypeId, Schema),
	UnionFields = find_tag_fields(TypeId, Schema),
	NotUnionFields = find_notag_fields(TypeId, Schema),

	% For any group/union fields in our struct, we generate an extra encode
	% function. We then recombine all of the generated struct data to make
	% the final result. It's a bit ugly, but it makes for some nice-ish code
	% separation -- we can switch on the group inside the group encoder.

	% Later, these might be called by the struct encoder directly to retrieve
	% its data, but it means that the struct encoder needs a better
	% understanding of what fields are meant to come from where.

	EncodersNeeded = if
		UnionFields =:= [] ->
			[
				{generate_record, TypeId},
				{generate_decode, TypeId},
				{generate_encode, TypeId}
			];
		NotUnionFields /= [] ->
			[
				% Generate both the anon union version /and/ the plain version.
				{generate_record, TypeId},
				{generate_decode, TypeId},
				{generate_encode, TypeId},
				{generate_union_encode, {anonunion, TypeId}},
				{generate_decode, {anonunion, TypeId}}
			];
		true ->
			[
				% We are just an anonymous union. We don't need a record.
				{generate_union_encode, TypeId},
				{generate_decode, TypeId}
			]
	end,

	EnvelopesNeeded = if
		not IsGroup ->
			[ {generate_envelope, TypeId} ];
		true ->
			[]
	end,

	ExtraStructs = [ {generate_name, TypeName} || #field_info{type=#ptr_type{type=struct, extra={TypeName, _, _}}} <- find_fields(TypeId, Schema) ],
	ExtraStructsInLists = [ {generate_name, TypeName} || #field_info{type=#ptr_type{type=list, extra={struct, #ptr_type{type=struct, extra={TypeName, _, _}}}}} <- find_fields(TypeId, Schema) ],
	ExtraGroups = [ {generate, GroupTypeId} || #field_info{type=#group_type{type_id=GroupTypeId}} <- find_fields(TypeId, Schema) ],
	ExtraTextList = lists:append([ [{generate_text, TextType}, {generate_follow_text_pointer, TextType}] || #field_info{type=#ptr_type{type=list, extra={text, TextType}}} <- find_fields(TypeId, Schema) ]),
	ExtraText = [ {generate_follow_text_pointer, Type} || #field_info{type=#ptr_type{type=text_or_data, extra=Type}} <- find_fields(TypeId, Schema) ],
	ExtraFollowStruct = [ generate_follow_struct_pointer || #field_info{type=#ptr_type{type=struct}} <- find_fields(TypeId, Schema) ],
	ExtraFollowStructList = [ generate_follow_struct_list_pointer || #field_info{type=#ptr_type{type=list, extra={struct, _}}} <- find_fields(TypeId, Schema) ],
	ExtraFollowNativeLists = [ {generate_follow_primitive_list_pointer, N} || #field_info{type=#ptr_type{type=list, extra={primitive, N=#native_type{}}}} <- find_fields(TypeId, Schema) ],

	Jobs = EncodersNeeded ++ EnvelopesNeeded ++ ExtraStructs ++ ExtraStructsInLists ++ ExtraGroups ++ ExtraTextList ++ ExtraText ++ ExtraFollowStruct ++ ExtraFollowStructList ++ ExtraFollowNativeLists,

	{ [], [], Jobs }.

% This points all pointers in the data segment at somewhat junk, but
% data decoders never need this information!
message_ref(#field_info{type=#group_type{}}) ->
	ast(MessageRef);
message_ref(#field_info{type=#native_type{}}) ->
	undefined;
message_ref(#field_info{type=#ptr_type{}, offset=Offset}) ->
	WordOffset = {integer, 0, (Offset bsr 6)},
	ast(MessageRef#message_ref{current_offset=MessageRef#message_ref.current_offset + quote(WordOffset)}).

generate_decode_fun(Line, TypeId, SortedDataFields, SortedPtrFields, Groups, Schema) ->
	% Should be decode_<Name>(PrimitiveData, PointerData, CompleteMessage) -> #<Name>{}
	% or decode_<Name>(PrimitiveData, PointerData, CompleteMessage) -> {UnionTag, UnionData}
	{_, DWords, PWords} = node_name(TypeId, Schema),
	DBits = {integer, Line, DWords * 64},
	PBits = {integer, Line, PWords * 64},
	Decoder = case is_union(TypeId, Schema) of
		false ->
			% This is the usual case. The type is to be treated as a usual struct.
			DataMatcher1 = {bin, Line, generate_data_binary(0, SortedDataFields, decode, DWords)},
			PointerMatcher1 = {bin, Line, generate_ptr_binary(0, SortedPtrFields, decode, PWords)},

			% These case statements just mask out variables that might be
			% needed to decode certain fields if we have no such fields, to
			% prevent some compile warnings.
			case Groups of
				[] ->
					DataMatcher = DataMatcher1,
					PointerMatcher = PointerMatcher1;
				_ ->
					DataMatcher = ast(Data = quote(DataMatcher1)),
					PointerMatcher = ast(Pointers = quote(PointerMatcher1))
			end,
			case Groups ++ SortedPtrFields of
				[] ->
					MessageRefVar = ast(_MessageRef);
				_ ->
					MessageRefVar = ast(MessageRef)
			end,
			{record, Line, record_name(TypeId, Schema),
				[{record_field, Line, make_atom(Line, FieldName), decoder(Type, Default, var_p(Line, "Var", FieldName), Line, message_ref(Info), Schema)} || Info=#field_info{name=FieldName, default=Default, type=Type} <- SortedDataFields ++ SortedPtrFields ++ Groups ]
			};
		true ->
			% The type is to be treated as a union. We need to find the
			% discriminant and then decode based on that.
			UnionFields = find_tag_fields(TypeId, Schema),
			Sorted = lists:sort(fun (#field_info{discriminant=X}, #field_info{discriminant=Y}) -> X < Y end, UnionFields),
			Expected = lists:seq(0, length(Sorted)-1),
			{Expected, Expected} = {Expected, [ X || #field_info{discriminant=X} <- Sorted ]},

			#field_info{offset=Offset} = discriminant_field(TypeId, Schema),
			DiscriminantSkip = {integer, Line, Offset},
			DataRest = {integer, Line, DWords * 64 - Offset - 16},
			DataMatcher1 = ast(<<_:(quote(DiscriminantSkip)), Discriminant:16/little-unsigned-integer, _:(quote(DataRest))>>),
			PointerMatcher1 = ast(<<_:(quote(PBits))>>),

			case lists:any(fun capnp_schema_wrangle:is_group_type/1, UnionFields) orelse lists:any(fun capnp_schema_wrangle:is_nonvoid_native_type/1, UnionFields) of
				true ->
					DataMatcher = ast(Data = quote(DataMatcher1));
				false ->
					DataMatcher = DataMatcher1
			end,
			case lists:any(fun capnp_schema_wrangle:is_group_type/1, UnionFields) orelse lists:any(fun capnp_schema_wrangle:is_pointer_type/1, UnionFields) of
				true ->
					MessageRefVar = ast(MessageRef),
					PointerMatcher = ast(Pointers = quote(PointerMatcher1));
				false ->
					MessageRefVar = ast(_MessageRef),
					PointerMatcher = PointerMatcher1
			end,
			DecoderClauses = [ {clause, Line, [{integer, Line, Discriminant}], [], generate_union_decoder(Line, Field, Schema)} || Field=#field_info{discriminant=Discriminant} <- Sorted ],
			{'case', Line, ast(Discriminant), DecoderClauses}
	end,
	Name = decoder_name(TypeId, Schema),
	ast_function(
		quote(Name),
		fun
			(quote(DataMatcher), quote(PointerMatcher), quote(MessageRefVar)) ->
				quote(Decoder);
			(Data, Pointers, MessageRef=#message_ref{}) ->
				DataPadLength = quote({integer, Line, DWords * 64}) - bit_size(Data),
				if
					DataPadLength > 0 ->
						PaddedData = <<Data/binary, 0:DataPadLength>>;
					DataPadLength =:= 0 ->
						PaddedData = Data;
					DataPadLength < 0 ->
						<<PaddedData:(quote(DBits))/bitstring, _/bitstring>> = Data
				end,
				PointerPadLength = quote({integer, Line, PWords * 64}) - bit_size(Pointers),
				if
					PointerPadLength > 0 ->
						PaddedPointers = <<Pointers/binary, 0:PointerPadLength>>;
					PointerPadLength =:= 0 ->
						PaddedPointers = Pointers;
					PointerPadLength < 0 ->
						<<PaddedPointers:(quote(PBits))/bitstring, _/bitstring>> = Pointers
				end,
				(quote({atom, Line, Name}))(PaddedData, PaddedPointers, MessageRef)
		end
	).

generate_union_decoder(Line, #field_info{name=Name, default=undefined, type=#group_type{type_id=TypeId}}, Schema) ->
	Decoder = make_atom(Line, decoder_name(TypeId, Schema)),
	[
		ast({quote(make_atom(Line, Name)), (quote(Decoder))(Data, Pointers, MessageRef)})
	];
generate_union_decoder(Line, #field_info{name=Name, type=#native_type{width=0}}, _Schema) ->
	% A bit ugly, but prevents unused Var warnings.
	[
		ast({quote(make_atom(Line, Name)), undefined})
	];
generate_union_decoder(Line, Field=#field_info{name=Name, default=Default, type=Type}, Schema) ->
	Var = ast(Var),
	[
		generate_union_matcher(Line, Field, Schema),
		ast({quote(make_atom(Line, Name)), quote(decoder(Type, Default, Var, Line, message_ref(Field), Schema))})
	].

generate_union_matcher(Line, #field_info{offset=Offset, type=#native_type{width=Width, binary_options=BinaryOptions}}, _Schema) ->
	Skip = {integer, Line, Offset},
	Junk = junkterm(Line, decode),
	Matcher = {bin, Line, [{bin_element, Line, Junk, Skip, default}, {bin_element, Line, ast(Var), {integer, Line, Width}, BinaryOptions}, {bin_element, Line, Junk, default, [bitstring]}]},
	ast(quote(Matcher) = Data);
generate_union_matcher(Line, #field_info{offset=Offset, type=#ptr_type{}}, _Schema) ->
	Skip = {integer, Line, Offset},
	ast(<<_:(quote(Skip)), Var:64/little-unsigned-integer, _/bitstring>> = Pointers).

generate_full_decoder_fun(Line, TypeId, Schema) ->
	% Should be function decode_<Name>(<<>>, StartOffset, CompleteMessage) -> #<Name>{}
	% We just take the binary for now and break if we get a pointer type.
	Decoder = {'fun', Line, {function, to_atom(decoder_name(TypeId, Schema)), 3}},
	ast_function(
		quote(full_decoder_name(TypeId, Schema)),
		fun (Data) ->
			{MessageRef, Ptr, Dregs} = decode_envelope(Data),
			Decoded = follow_struct_pointer(quote(Decoder), Ptr, MessageRef),
			{Decoded, Dregs}
		end
	).

generate_decode_text_fun(TextType) ->
	case TextType of
		text ->
			Name = internal_decode_text,
			Follow = ast(follow_text_pointer);
		data ->
			Name = internal_decode_data,
			Follow = ast(follow_data_pointer)
	end,
	FunDef = ast_function(
		quote(Name),
		fun
			(_, <<Var:64/little-unsigned-integer, _/binary>>, MessageRef) ->
				(quote(Follow))(Var, MessageRef);
			(_, <<>>, _MessageRef) ->
				undefined
		end
	),
	{ [], [FunDef], [] }.

generate_decode_envelope_fun() ->
	[RecDef, FunDef] = ast_decode_envelope(),
	{ [RecDef], [FunDef], [] }.

-ast_forms_function(#{name => ast_decode_envelope}).
	-record(message_ref, {current_offset, current_segment, segments}).
	decode_envelope(<<RawSegCount:32/little-unsigned-integer, Rest/binary>>) ->
		SegLengthLength = ((((RawSegCount + 1) bsr 1) bsl 1) + 1) bsl 2,
		<<SegLengthData:SegLengthLength/binary, SegData/binary>> = Rest,
		SegLengths = [ X bsl 3 || <<X:32/little-unsigned-integer>> <= SegLengthData, X > 0 ],
		{SegsR, Dregs} = lists:foldl(fun (Length, {SplitSegs, Data}) -> <<Seg:Length/binary, Remain/binary>> = Data, {[Seg|SplitSegs], Remain} end, {[], SegData}, SegLengths),
		Segs = lists:reverse(SegsR),
		<<Ptr:64/little-unsigned-integer, _/binary>> = hd(Segs),
		{#message_ref{current_offset=0, current_segment=hd(Segs), segments=list_to_tuple(Segs)}, Ptr, Dregs}.
-end_ast_forms_function([]).

generate_massage_bool_list() ->
	{ [], ast_massage_bool_list(), [generate_decode_far_pointer] }.

-ast_forms_function(#{name => ast_massage_bool_list}).
	massage_bool_list(List) ->
		try lists:split(8, List) of
			{First, Last} ->
				lists:reverse(First) ++ massage_bool_list(Last)
		catch
			error:badarg ->
				lists:reverse(List ++ lists:duplicate(-length(List) band 7, 0))
		end.
-end_ast_forms_function([]).

generate_follow_struct_pointer() ->
	{ [], ast_follow_struct_pointer(), [generate_decode_far_pointer] }.

-ast_forms_function(#{name => ast_follow_struct_pointer}).
	follow_struct_pointer(_DecodeFun, 0, _MessageRef) ->
		undefined;
	follow_struct_pointer(DecodeFun, PointerInt, MessageRef) when PointerInt band 3 == 0 ->
		PointerOffset = case PointerInt band (1 bsl 31) of
			0 -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) + 1;
			_ -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) - (1 bsl 30) + 1
		end,
		NewOffset = MessageRef#message_ref.current_offset + PointerOffset,
		DWords = (PointerInt bsr 32) band (1 bsl 16 - 1),
		PWords = (PointerInt bsr 48) band (1 bsl 16 - 1),
		NewMessageRef = MessageRef#message_ref{current_offset=NewOffset + DWords}, % Point at start of pointer area.
		SkipBits = NewOffset bsl 6,
		DBits = DWords bsl 6,
		PBits = PWords bsl 6,
		<<_:SkipBits, Data:DBits/bitstring, Pointers:PBits/bitstring, _/binary>> = MessageRef#message_ref.current_segment,
		DecodeFun(Data, Pointers, NewMessageRef);
	follow_struct_pointer(DecodeFun, PointerInt, MessageRef=#message_ref{}) when PointerInt band 3 == 2 ->
		{NewPointerInt, NewMessageRef} = decode_far_pointer(PointerInt, MessageRef),
		follow_struct_pointer(DecodeFun, NewPointerInt, NewMessageRef).
-end_ast_forms_function([]).

generate_decode_far_pointer() ->
	{ [], ast_decode_far_pointer(), [] }.

-ast_forms_function(#{name => ast_decode_far_pointer}).
	decode_far_pointer(PointerInt, MessageRef=#message_ref{segments=Segments}) when PointerInt band 3 == 2 ->
		% Far pointer
		PointerOffset = ((PointerInt bsr 3) band (1 bsl 29 - 1)),
		SkipBits = PointerOffset bsl 6,
		Segment = element((PointerInt bsr 32) + 1, Segments),
		<<_:SkipBits, LandingPadInt:64/little-unsigned-integer, _/bitstring>> = Segment,
		case PointerInt band 4 of
			0 ->
				NewPointerInt = LandingPadInt,
				NewMessageRef = MessageRef#message_ref{current_segment=Segment, current_offset=PointerOffset};
			1 ->
				2 = LandingPadInt band 7, % Sanity check (far pointer, one byte)
				SecondPointerOffset = ((LandingPadInt bsr 3) band (1 bsl 29 - 1)),
				SecondSegment = element((LandingPadInt bsr 32) + 1, Segments),
				<<_:SkipBits, 0:64, NewPointerInt:64/little-unsigned-integer, _/bitstring>> = Segment,
				0 = ((NewPointerInt bsr 2) band (1 bsl 30 - 1)), % Sanity check (offset=0)
				% Note that the 0-offset is relative to the end of the pointer at current_offset.
				% So we need to subtract 1 from SecondPointerOffset to get the decode to work correctly.
				NewMessageRef = MessageRef#message_ref{current_segment=SecondSegment, current_offset=SecondPointerOffset-1}
		end,
		{NewPointerInt, NewMessageRef}.
-end_ast_forms_function([]).

generate_follow_text_pointer(text) ->
	{ [], ast_follow_text_pointer(), [{generate_follow_text_pointer, common}] };
generate_follow_text_pointer(data) ->
	{ [], ast_follow_data_pointer(), [{generate_follow_text_pointer, common}] };
generate_follow_text_pointer(common) ->
	{ [], ast_follow_text_or_data_pointer(), [generate_decode_far_pointer] }.

-ast_forms_function(#{name => ast_follow_text_pointer}).
follow_text_pointer(PointerInt, MessageRef) ->
	follow_text_or_data_pointer(PointerInt, MessageRef, 1).
-end_ast_forms_function([]).

-ast_forms_function(#{name => ast_follow_data_pointer}).
follow_data_pointer(PointerInt, MessageRef) ->
	follow_text_or_data_pointer(PointerInt, MessageRef, 0).
-end_ast_forms_function([]).

-ast_forms_function(#{name => ast_follow_text_or_data_pointer}).
	follow_text_or_data_pointer(0, _MessageRef, _Trail) ->
		undefined;
	follow_text_or_data_pointer(PointerInt, MessageRef, Trail) when PointerInt band 3 =:= 1 andalso (PointerInt bsr 32) band 7 =:= 2 ->
		PointerOffset = case PointerInt band (1 bsl 31) of
			0 -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) + 1;
			_ -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) - (1 bsl 30) + 1
		end,
		Offset = MessageRef#message_ref.current_offset + PointerOffset,
		SkipBits = Offset bsl 6,
		Length = (PointerInt bsr 35) - Trail,
		MessageBits = Length bsl 3,
		<<_:SkipBits, ListData:MessageBits/bitstring, _/bitstring>> = MessageRef#message_ref.current_segment,
		ListData;
	follow_text_or_data_pointer(PointerInt, MessageRef=#message_ref{}, Trail) when PointerInt band 3 == 2 ->
		{NewPointerInt, NewMessageRef} = decode_far_pointer(PointerInt, MessageRef),
		follow_text_or_data_pointer(NewPointerInt, NewMessageRef, Trail).
-end_ast_forms_function([]).

generate_follow_struct_list_pointer() ->
	{ [], ast_follow_struct_list_pointer(), [generate_decode_far_pointer] }.

-ast_forms_function(#{name => ast_follow_struct_list_pointer}).
	follow_tagged_struct_list_pointer(_DecodeFun, 0, _MessageRef) ->
		undefined;
	follow_tagged_struct_list_pointer(DecodeFun, PointerInt, MessageRef) when PointerInt band 3 == 1 ->
		PointerOffset = case PointerInt band (1 bsl 31) of
			0 -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) + 1;
			_ -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) - (1 bsl 30) + 1
		end,
		NewOffset = MessageRef#message_ref.current_offset + PointerOffset,
		SkipBits = NewOffset bsl 6,
		<<_:SkipBits, Tag:64/little-unsigned-integer, _/binary>> = MessageRef#message_ref.current_segment,
		Length = ((Tag bsr 2) band (1 bsl 30 - 1)),
		DWords = (Tag bsr 32) band (1 bsl 16 - 1),
		PWords = (Tag bsr 48) band (1 bsl 16 - 1),
		decode_struct_list(DecodeFun, Length, DWords, PWords, MessageRef#message_ref{current_offset=NewOffset+1});
	follow_tagged_struct_list_pointer(DecodeFun, PointerInt, MessageRef=#message_ref{}) when PointerInt band 3 == 2 ->
		{NewPointerInt, NewMessageRef} = decode_far_pointer(PointerInt, MessageRef),
		follow_tagged_struct_list_pointer(DecodeFun, NewPointerInt, NewMessageRef).

	decode_struct_list(DecodeFun, Length, DWords, PWords, MessageRef) ->
		Offset = MessageRef#message_ref.current_offset,
		SkipBits = Offset * 64,
		<<_:SkipBits, Rest/binary>> = MessageRef#message_ref.current_segment,
		Words = DWords + PWords,
		DBits = DWords * 64,
		PBits = PWords * 64,
		{_, ListR} = lists:foldl(
			fun
				(N, {OldRest, Acc}) ->
					<<ThisData:DBits/bitstring, ThisPointers:PBits/bitstring, NewRest/binary>> = OldRest,
					New = DecodeFun(ThisData, ThisPointers, MessageRef#message_ref{current_offset=Offset+DWords+Words*N}),
					{NewRest, [New|Acc]}
			end,
			{Rest, []},
			lists:seq(0, Length-1)
		),
		lists:reverse(ListR).
-end_ast_forms_function([]).

generate_follow_primitive_list_pointer(_Line, #native_type{type=void}) ->
	FunDef = ast_function(
		quote(follow_void_list_pointer),
		fun
			(0, _MessageRef) ->
				undefined;
			(PointerInt, MessageRef) when PointerInt band 3 =:= 1 andalso (PointerInt bsr 32) band 7 =:= 0 ->
				Length = PointerInt bsr 35,
				[ undefined || _ <- lists:seq(1, Length) ]
		end
	),
	{ [], [FunDef], [] };
generate_follow_primitive_list_pointer(_Line, #native_type{type=boolean}) ->
	FunDef = ast_function(
		quote(follow_bool_list_pointer),
		fun
			(0, _MessageRef) ->
				undefined;
			(PointerInt, MessageRef) when PointerInt band 3 =:= 1 andalso (PointerInt bsr 32) band 7 =:= 1 ->
				PointerOffset = case PointerInt band (1 bsl 31) of
					0 -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) + 1;
					_ -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) - (1 bsl 30) + 1
				end,
				Offset = MessageRef#message_ref.current_offset + PointerOffset,
				SkipBits = Offset * 64,
				Length = PointerInt bsr 35,
				WholeParts = ((Length + 7) bsr 3) bsl 3,
				<<_:SkipBits, ListData:WholeParts/bitstring, _/bitstring>> = MessageRef#message_ref.current_segment,
				lists:sublist(lists:append([ lists:reverse([case Bit of 0 -> false; 1 -> true end || <<Bit:1>> <= Byte ]) || <<Byte:8/bitstring>> <= ListData ]), Length)
		end
	),
	{ [], [FunDef], [] };
generate_follow_primitive_list_pointer(Line, #native_type{type=Type, name=Name, width=Width, binary_options=BinaryOpts, list_tag=Tag, extra=Extra}) ->
	Match = {b_generate, Line, {bin, Line, [{bin_element, Line, ast(X), {integer, Line, Width}, BinaryOpts}]}, ast(ListData)},
	Decode = case Type of
		integer ->
			ast(X);
		float ->
			ast(X);
		enum ->
			Tuple = {tuple, Line, [ make_atom(Line, X) || X <- Extra ]},
			ast(element(X+1, quote(Tuple)))
	end,
	FunDef = ast_function(
		quote(to_atom(append(follow_, append(Name, "_list_pointer")))),
		fun
			(0, _MessageRef) ->
				undefined;
			(PointerInt, MessageRef) when PointerInt band 3 =:= 1 andalso (PointerInt bsr 32) band 7 =:= quote({integer, Line, Tag}) ->
				PointerOffset = case PointerInt band (1 bsl 31) of
					0 -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) + 1;
					_ -> ((PointerInt bsr 2) band (1 bsl 30 - 1)) - (1 bsl 30) + 1
				end,
				Offset = MessageRef#message_ref.current_offset + PointerOffset,
				SkipBits = Offset * 64,
				Length = PointerInt bsr 35,
				MessageBits = Length * quote({integer, Line, Width}),
				<<_:SkipBits, ListData:MessageBits/bitstring, _/bitstring>> = MessageRef#message_ref.current_segment,
				[ quote(Decode) || quote(Match) ]
		end
	),
	{ [], [FunDef], [] }.

generate_encode_fun(Line, TypeId, Schema) ->
	% We're going to encode by encoding each type as close to its contents as possible.
	% So a #a{b=#c{}} looks like [ enc(a), enc(b) ].
	% This doesn't quite work for lists, as the complete list must be inlined first.
	% So PtrOffsetWordsFromEnd0 will be 0 for structs, or the distance to the end of the list for lists.
	%
	% function encode_<Type>(#<Type>{}, PtrOffsetWordsFromEnd0) ->
	%     {DataLen0, PtrLen0} = (constant)
	%     ExtraLen0 = 0,
	%
	%     {DataLen1, PtrLen1, ExtraLen1, Data1, Extra1} = encode_<Type1>(<InputData1>, 0),
	%     PtrOffset1 = PtrOffsetWordsFromEnd0 + (PtrLen0 - 1), % Distance /from/ end plus distance /to/ end
	%     Ptr1 = struct_ptr(PtrOffset1, DataLen1, PtrLen1),
	%     PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0 + DataLen1 + PtrLen1 + ExtraLen1,
	%
	%     {DataLen2, PtrLen2, ExtraLen2, Data2, Extra2} = encode_<Type2>(<InputData2>, 0),
	%     PtrOffset2 = PtrOffsetWordsFromEnd1 + (PtrLen0 - 2),
	%     Ptr2 = struct_ptr(PtrOffset2, DataLen2, PtrLen2),
	%     PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1 + DataLen2 + PtrLen2 + ExtraLen2,
	%
	%     MyPtrs = << X:64/unsigned-little-integer || X <- [Ptr1, Ptr2, ...] >>,
	%     {DataLen, PtrLen, PtrOffsetWordsFromEndN - PtrOffsetWordsFromEnd0, [MyData, MyPtrs], [Data1, Extra1, Data2, Extra2, ...]}.
	%
	SortedFields = flatten_notag_fields(TypeId, Schema),
	SortedDataFields = lists:filter(fun capnp_schema_wrangle:is_native_type/1, SortedFields),
	SortedPtrFields = lists:filter(fun capnp_schema_wrangle:is_pointer_type/1, SortedFields),
	Unions = lists:filter(capnp_schema_wrangle:is_union_type(Schema), find_anon_union(TypeId, Schema) ++ find_notag_groups(TypeId, Schema)),
	EncodeBody = encode_function_body(Line, TypeId, Unions, SortedDataFields, SortedPtrFields, Schema),

	Matcher = matcher(TypeId, "Var", Line, Schema),

	ast_function(
		quote(encoder_name(TypeId, Schema)),
		fun
			(quote(Matcher), PtrOffsetWordsFromEnd0) ->
				quote_block(EncodeBody);
			(undefined, _PtrOffsetWordsFromEnd0) ->
				{0, 0, 0, [], []};
			({ZeroOffsetPtrInt, MainLen, ExtraLen, MainData, ExtraData}, 0)
					when is_integer(ZeroOffsetPtrInt), is_integer(MainLen), is_integer(ExtraLen) ->
				% Note that we can't precompute list elements because their encodings might require pointer re-offsetting.
				% Hence we must check PtrOffsetWordsFromEnd0 == 0.
				{ZeroOffsetPtrInt, MainLen, ExtraLen, MainData, ExtraData}
		end).

generate_text(TextType) ->
	case TextType of
		text ->
			DataLenAst = ast(iolist_size(List) + 1),
			DataAst = ast([List, <<0:8, 0:(-DataLen band 7 * 8)/unsigned-little-integer>>]);
		data ->
			DataLenAst = ast(iolist_size(List)),
			DataAst = ast([List, <<0:(-DataLen band 7 * 8)/unsigned-little-integer>>])
	end,
	Line = 0,
	% This is a pointer to a struct with 1 pointer, which is what the caller /believes/ we are.
	% We pass it in just so that struct pointers are consistently generated.
	FakePointerInt = {integer, Line, struct_pointer_header(0, 1)},

	Func = ast_function(quote(to_atom(append(encode_, TextType))),
		fun
			(undefined, _Offset) ->
				{quote(FakePointerInt), 1, 0, [<<0:64>>], []};
			(List, Offset) ->
				DataLen = quote(DataLenAst),
				Data = quote(DataAst),
				Ptr = 1 bor (Offset bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
				{quote(FakePointerInt), 1, DataLen + 7 bsr 3, <<Ptr:64/unsigned-little-integer>>, Data}
		end),
	{[], [DecodeFunc], []} = generate_decode_text_fun(TextType),
	{[], [Func, DecodeFunc], []}.

encode_function_body(Line, TypeId, Unions, SortedDataFields, SortedPtrFields, Schema) ->
	#'Node'{
		''={
			struct,
			#'Node_struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = schema_lookup(TypeId, Schema),
	{NoGroupEncodeBody, NoGroupExtraLen, NoGroupBodyData, NoGroupExtraData} = encode_function_body_inline(Line, TypeId, SortedDataFields, SortedPtrFields, Schema),
	ZeroOffsetPtrInt = {integer, Line, struct_pointer_header(DWords, PWords)},
	StructSizeInt = {integer, Line, DWords + PWords},
	if
		Unions =:= [] ->
			% Easy case.
			NoGroupEncodeBody ++ [ {tuple, Line, [ZeroOffsetPtrInt, StructSizeInt, NoGroupExtraLen, NoGroupBodyData, NoGroupExtraData]} ];
		true ->
			NoGroupBodyDataInt = {var, Line, 'NoGroupBodyDataAsInt'},
			BodyLengthInt = {integer, Line, (PWords+DWords)*64},
			ToIntBody = [{match, Line, {bin, Line, [{bin_element, Line, NoGroupBodyDataInt, BodyLengthInt, [integer]}]}, NoGroupBodyData}],
			{FullEncodeBody, ExtraLen, SumBodyData, ExtraData} = group_encoder(NoGroupEncodeBody ++ ToIntBody, NoGroupExtraLen, NoGroupBodyDataInt, NoGroupExtraData, Unions, Line, BodyLengthInt, Schema),
			BodyData = {bin, Line, [{bin_element, Line, SumBodyData, BodyLengthInt, [integer]}]},
			FullEncodeBody ++ [ {tuple, Line, [ZeroOffsetPtrInt, StructSizeInt, ExtraLen, BodyData, ExtraData]} ]
	end.

generate_union_encode_fun(Line, TypeId, UnionFields, Schema) ->
	EncodeBody = union_encode_function_body(Line, TypeId, UnionFields, Schema),
	case lists:any(fun capnp_schema_wrangle:is_pointer_type/1, UnionFields) orelse lists:any(fun capnp_schema_wrangle:is_group_type/1, UnionFields) of
		true -> PtrOffsetWordsFromEnd0Var = ast(PtrOffsetWordsFromEnd0);
		false -> PtrOffsetWordsFromEnd0Var = ast(_PtrOffsetWordsFromEnd0)
	end,

	ast_function(
		quote(encoder_name(TypeId, Schema)),
		fun ({VarDiscriminant, Var}, quote(PtrOffsetWordsFromEnd0Var)) -> quote_block(EncodeBody) end
	).

union_encode_function_body(Line, TypeId, UnionFields, Schema) ->
	Sorted = lists:sort(fun (#field_info{discriminant=X}, #field_info{discriminant=Y}) -> X < Y end, UnionFields),
	Expected = lists:seq(0, length(Sorted)-1),
	{Expected, Expected} = {Expected, [ X || #field_info{discriminant=X} <- Sorted ]},

	DiscriminantField = discriminant_field(TypeId, Schema),

	EncoderClauses = [ {clause, Line, [make_atom(Line, Name)], [], generate_union_encoder(Line, DiscriminantField, Field, TypeId, Schema)} || Field=#field_info{name=Name} <- Sorted ],
	[{'case', Line, {var, Line, 'VarDiscriminant'}, EncoderClauses}].

generate_union_encoder(Line, DiscriminantFieldRaw, Field=#field_info{discriminant=Discriminant, type=#native_type{}}, TypeId, Schema) ->
	#'Node'{
		''={
			struct,
			#'Node_struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = schema_lookup(TypeId, Schema),
	DiscriminantField = DiscriminantFieldRaw#field_info{override={integer, Line, Discriminant}},
	[{tuple, Line, [
				{integer, Line, struct_pointer_header(DWords, PWords)},
				{integer, Line, DWords+PWords},
				{integer, Line, 0},
				{bin, Line, generate_data_binary(0, lists:sort([DiscriminantField, Field#field_info{name={override, 'Var'}}]), encode, DWords) ++ generate_ptr_binary(0, [], encode, PWords)},
				{nil, Line}
	]}];
generate_union_encoder(Line, DiscriminantFieldRaw, Field=#field_info{discriminant=Discriminant, type=Type=#ptr_type{}, offset=Offset}, TypeId, Schema) ->
	#'Node'{
		''={
			struct,
			#'Node_struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = schema_lookup(TypeId, Schema),
	DiscriminantField = DiscriminantFieldRaw#field_info{override={integer, Line, Discriminant}},
	ast_encode_ptr({1, Offset bsr 6}, PWords, Type, <<>>, Line) ++
	[{tuple, Line, [
				{integer, Line, struct_pointer_header(DWords, PWords)},
				{integer, Line, DWords+PWords},
				{op, Line, '-', {var, Line, 'PtrOffsetWordsFromEnd1'}, {var, Line, 'PtrOffsetWordsFromEnd0'}},
				{bin, Line, generate_data_binary(0, [DiscriminantField], encode, DWords) ++ generate_ptr_binary(0, [Field#field_info{name= <<>>}], encode, PWords)},
				to_list(Line, [{var, Line, 'Data1'}, {var, Line, 'Extra1'}])
	]}];
generate_union_encoder(Line, #field_info{offset=Offset}, #field_info{discriminant=Discriminant, type=#group_type{type_id=GroupTypeId}}, TypeId, Schema) ->
	#'Node'{
		''={
			struct,
			#'Node_struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = schema_lookup(TypeId, Schema),
	ast_group_in_union_(
		{in, [{atom, Line, encoder_name(GroupTypeId, Schema)}, {integer, Line, Discriminant}, {integer, Line, Offset}, {integer, Line, (DWords+PWords)*64}]},
		{out, []},
		{temp_suffix, ""}
	).

% We're not too careful about variable clashes here, since each call to this is made in an independent block.
-ast_fragment2([]).
ast_group_in_union_(
		{in, [EncodeFun, Discriminant, DiscriminantOffset, BitLength]},
		{out, []},
		{temp, [ZeroOffsetPtrInt, MainLen]}
		) ->
	{ZeroOffsetPtrInt, MainLen, ExtraLen, <<DataInt:BitLength/little-unsigned-integer>>, ExtraData} = EncodeFun(Var, PtrOffsetWordsFromEnd0),
	{ZeroOffsetPtrInt, MainLen, ExtraLen, <<(DataInt bor (Discriminant bsl DiscriminantOffset)):BitLength/little-unsigned-integer>>, ExtraData}.


% Combining directly to a binary as we do here is faster than using 'bsl' on the values as integers, by about a factor of 3 on a 6 element list (0.13 micros vs 0.38 micros).
% The difference becomes larger with more elements.
encode_function_body_inline(Line, TypeId, SortedDataFields, SortedPtrFields, Schema) ->
	{_, DWords, PWords} = node_name(TypeId, Schema),
	DataMaker = generate_data_binary(0, SortedDataFields, encode, DWords),
	PtrMaker = generate_ptr_binary(0, SortedPtrFields, encode, PWords),
	EncodePointers = lists:append([ begin undefined = Default, ast_encode_ptr({N, Offset bsr 6}, PWords, Type, FieldName, Line) end || {N, #field_info{offset=Offset, default=Default, name=FieldName, type=Type=#ptr_type{}}} <- lists:zip(lists:seq(1, length(SortedPtrFields)), SortedPtrFields) ]),
	{
		EncodePointers,
		{op, Line, '-', var_p(Line, "PtrOffsetWordsFromEnd", length(SortedPtrFields)), {var, Line, 'PtrOffsetWordsFromEnd0'}}, % Extra len that we added
		{bin, Line, DataMaker ++ PtrMaker},
		to_list(Line, lists:append([ [ var_p(Line, "Data", N), var_p(Line, "Extra", N) ] || N <- lists:seq(1, length(SortedPtrFields)) ])) % Extra data that we added
	}.


% This is the part of the encoder body that's inserted after we've encoded the pointers and such.
% It boils down to "call the encoders for each group, then convert everything to big integers and add them together".
% This is somewhat faster than generating the individual binary parts and flattening them,
% though slower than simply spitting out the binary parts in an iolist.
% The time to bor two binaries of length 256 bits is about 0.3 micros for my test script. 1024 bits takes about a micro per bor.
% For comparison, the loop framework was consuming about 0.01 micros per test.
%
% 'bor' has a slight (probably not even statistically significant) speed advantage over '+', and is equivalent since the parts are supposed to be independent.
%
% The iolist solution, while faster as we're ultimately going to return an io_list anyway, is complicated by bit patterns, since:
% - Erlang shuffles the bits relative to our ideal little-endian order (we can fix this just by mangling the offsets), and
% - bitstrings aren't iolists, so we'd still need to add code to combine the bitstrings into binaries before we return.
group_encoder(InitialBody, InitialExtraLen, InitialBodyData, InitialExtraData, [], _Line, _BodyLengthInt, _Schema) ->
	{InitialBody, InitialExtraLen, InitialBodyData, InitialExtraData};
group_encoder(InitialBody, InitialExtraLen, InitialBodyDataInt, InitialExtraData, [#field_info{name=FieldName, type=#group_type{type_id=TypeId}}|T], Line, BodyLengthInt, Schema) ->
	MatchVar = var_p(Line, "Var", FieldName),
	NewExtraLen = var_p(Line, "ExtraDataLen", FieldName),
	NewBodyData = var_p(Line, "BodyData", FieldName),
	NewExtraData = var_p(Line, "ExtraData", FieldName),
	GroupEncodeFun = {atom, Line, encoder_name(TypeId, Schema)},
	ExtraBody = ast_encode_group_(
		{in, [MatchVar, GroupEncodeFun, InitialExtraLen]},
		{out, [NewExtraLen, NewBodyData, NewExtraData]},
		{temp_suffix, binary_to_list(FieldName)}
	),
	NewBodyDataInt = var_p(Line, "BodyDataAsIntFrom", FieldName),
	MeldBody = [{match, Line, {bin, Line, [{bin_element, Line, NewBodyDataInt, BodyLengthInt, [integer]}]}, NewBodyData}],
	group_encoder(InitialBody ++ ExtraBody ++ MeldBody, {op, Line, '+', InitialExtraLen, NewExtraLen}, {op, Line, 'bor', InitialBodyDataInt, NewBodyDataInt}, {cons, Line, InitialExtraData, NewExtraData}, T, Line, BodyLengthInt, Schema).

-ast_fragment2([]).
ast_encode_group_(
		{in, [MatchVar, GroupEncodeFun, InitialExtraLen]},
		{out, [NewExtraLen, NewBodyData, NewExtraData]},
		{temp, [_ZeroOffsetPtrInt, _NewBodyLen]} % These are the same as the full struct, since a group can't encode to null.
	) ->
	{_ZeroOffsetPtrInt, _NewBodyLen, NewExtraLen, NewBodyData, NewExtraData} = GroupEncodeFun(MatchVar, InitialExtraLen).


generate_envelope_fun(Line, TypeId, Schema) ->
	EncodeFun = {atom, Line, encoder_name(TypeId, Schema)},
	ast_function(
		quote(envelope_fun_name(TypeId, Schema)),
		fun
			(Input) ->
				{ZeroOffsetPtrInt, MainDataLen, ExtraDataLen, MainData, ExtraData} = (quote(EncodeFun))(Input, 0),
				list_to_binary([
						<<
							% Segment envelope
							0:32/unsigned-little-integer, % Segcount - 1. Always 0 for us.
							(1+MainDataLen+ExtraDataLen):32/unsigned-little-integer, % Seglen = 1 (our pointer) + (all encoded data)

							% Pointer to first struct. It's actually zero-offset, so we can just use the canonical pointer returned from the encoder.
							ZeroOffsetPtrInt:64/unsigned-little-integer % Number of data words in the struct.
						>>,

						% Now the actual data. This may be an io_list, so we can't just /binary it in.
						MainData,
						ExtraData
				])
		end
	).

ast_encode_ptr_common({PtrVarNum, PtrOffset}, PtrLen0, AstFun, ExtraInputParams, VarName, Line) ->
	%case VarName of <<"textualKey">> -> io:format("textualKey: ~p~n", [{N, PtrLen0, AstFun, ExtraInputParams, VarName, Line}]); _ -> ok end,
	[OldPtrOffsetWordsFromEndVar, NewPtrOffsetWordsFromEndVar] = [ var_p(Line, "PtrOffsetWordsFromEnd", OldOrNew) || OldOrNew <- [PtrVarNum-1, PtrVarNum] ],
	OffsetToEndInt = {integer, Line, PtrLen0 - PtrOffset - 1},
	MatchVar = var_p(Line, "Var", VarName),
	CommonIn = [OldPtrOffsetWordsFromEndVar, OffsetToEndInt, MatchVar],

	PtrVar = var_p(Line, "Ptr", VarName),
	[DataVar, ExtraVar] = [ var_p(Line, VN, PtrVarNum) || VN <- ["Data", "Extra"] ],
	CommonOut = [NewPtrOffsetWordsFromEndVar, PtrVar, DataVar, ExtraVar],

	AstFun(
		{in, CommonIn ++ ExtraInputParams},
		{out, CommonOut},
		{temp_suffix, temp_suffix(VarName)}
	).

temp_suffix({override, _Name}) ->
	"Temp";
temp_suffix(Name) ->
	binary_to_list(Name).

struct_pointer_header(DWords, PWords) ->
	% Tag is zero.
	0 + (DWords bsl 32) + (PWords bsl 48).

% This is just a variable aligning function which passes through to ast_encode_ptr_
ast_encode_ptr(N, PtrLen0, #ptr_type{type=struct, extra={TypeName, _DataLen, _PtrLen}}, VarName, Line) ->
	EncodeFun = make_atom(Line, append("encode_", TypeName)),
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_struct_/3, [EncodeFun], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=unknown}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_anyPointer_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=text_or_data, extra=text}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_text_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=text_or_data, extra=data}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_data_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={primitive, #native_type{type=boolean}}}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_bool_list_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={primitive, Type}}, VarName, Line) ->
	#native_type{list_tag=WidthType, type=Class, width=Width, binary_options=BinType} = Type,
	WidthTypeInt = {integer, Line, WidthType},
	WidthInt = {integer, Line, Width},
	% I can't << X || ... >> is invalid syntax, and << <<X>> || ... >> doesn't let me specify encoding types, so I
	% have to write the whole comprehension by hand! Woe!
	MatchVar = var_p(Line, "Var", VarName),
	Default = case Class of
		float -> 0.0;
		integer -> 0;
		boolean -> false;
		enum -> 0;
		void -> undefined
	end,
	EncodedX = {bc, Line, {bin, Line, [{bin_element, Line, encoder(Type, Default, {var, Line, 'X'}, Line), WidthInt, BinType}]}, [{generate,199,{var,Line,'X'}, MatchVar}]},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_primitive_list_/3, [WidthInt, WidthTypeInt, EncodedX], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={struct, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}}}, VarName, Line) ->
	EncodeFun = make_atom(Line, append("encode_", TypeName)),
	StructSizePreformatted = {integer, Line, struct_pointer_header(DataLen, PtrLen)},
	StructLen = {integer, Line, DataLen + PtrLen},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_struct_list_/3, [EncodeFun, StructSizePreformatted, StructLen], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={text, TextType}}, VarName, Line) ->
	% This is a bit of a hack; we encode a list of text fields as a list of structs which have the first field being text.
	% They should really be anyPointer, I think, which doesn't need a header tag word.
	EncodeFun = make_atom(Line, append("encode_", TextType)),
	StructSizePreformatted = {integer, Line, struct_pointer_header(0, 1)},
	StructLen = {integer, Line, 1},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_struct_list_/3, [EncodeFun, StructSizePreformatted, StructLen], VarName, Line).

% This are the fragment that are inserted for each pointer.
% All of them should have the same 'out' params:
% They should assign PtrOffsetWordsFromEnd{N} to include the length of all of the stuff they're encoding, plus PtrOffsetWordsFromEnd{N-1}.
% They should assign Ptr{VarName} to a valid 64-bit integer to encode into the pointer.
% They should assign Data{N} to their personal data.
% They should assign Extra{N} to any extra data they are adding.
% The distinction between Data{N} and Extra{N} is important for list contents; Data should contain /just/ the thing that would be put into the list; Extra /just/ the thing that isn't.
-ast_fragment2([]).
ast_encode_struct_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, EncodeFun]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [ZeroOffsetPtrInt, MainLen, ExtraLen]}
	) ->
	{ZeroOffsetPtrInt, MainLen, ExtraLen, MainData, ExtraData} = EncodeFun(ValueToEncode, 0),
	PointerAsInt = case ZeroOffsetPtrInt of 0 -> 0; _ -> ((OldOffsetFromEnd + OffsetToEnd) bsl 2) + ZeroOffsetPtrInt end,
	NewOffsetFromEnd = OldOffsetFromEnd + MainLen + ExtraLen.

-ast_fragment2([]).
ast_encode_text_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen]}
	) ->
	if
		is_list(ValueToEncode); is_binary(ValueToEncode) ->
			ExtraData = <<>>,
			DataLen = iolist_size(ValueToEncode)+1,
			% We need to add a null terminator.
			% Then we need to add (-L band 7) bytes of padding for alignment.
			MainData = [ValueToEncode, <<0:8, 0:((-DataLen band 7)*8)/unsigned-little-integer>>],
			PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
			NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 7) bsr 3);
		ValueToEncode =:= undefined ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd
	end.

-ast_fragment2([]).
ast_encode_data_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen]}
	) ->
	if
		is_list(ValueToEncode); is_binary(ValueToEncode) ->
			ExtraData = <<>>,
			DataLen = iolist_size(ValueToEncode),
			% Then we need to add (-L band 7) bytes of padding for alignment.
			MainData = [ValueToEncode, <<0:((-DataLen band 7)*8)/unsigned-little-integer>>],
			PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
			NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 7) bsr 3);
		ValueToEncode =:= undefined ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd
	end.

-ast_fragment2([]).
ast_encode_anyPointer_(
		{in, [OldOffsetFromEnd, _OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, []}
	) ->
	if
		ValueToEncode =:= undefined ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd
	end.

-ast_fragment2([]).
ast_encode_primitive_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, Width, WidthType, EncodedX]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen, MainLen, ExtraLen, PointerAsInt1]}
	) ->
	case ValueToEncode of
		_ when is_list(ValueToEncode) ->
			ExtraData = <<>>,
			DataLen = length(ValueToEncode),
			% We need to add (-L*W band 63) bytes of padding for alignment.
			MainData = [EncodedX, <<0:(-DataLen*Width band 63)/unsigned-little-integer>>],
			PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (WidthType bsl 32) bor (DataLen bsl 35),
			NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen * Width + 63) bsr 6);
		{0, 0, 0, _, _} ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd;
		{PointerAsInt1, MainLen, ExtraLen, MainData, ExtraData} ->
			% Match vars are included above
			PointerAsInt = PointerAsInt1 bor ((OffsetToEnd + OldOffsetFromEnd) bsl 2),
			NewOffsetFromEnd = OldOffsetFromEnd + MainLen + ExtraLen;
		undefined ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd
	end.

-ast_fragment2([]).
ast_encode_bool_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen, DataFixed, MainLen, ExtraLen, PointerAsInt1]}
	) ->
	case ValueToEncode of
		_ when is_list(ValueToEncode) ->
			ExtraData = <<>>,
			DataLen = length(ValueToEncode),
			% Because Erlang will reverse blocks of 8 bools, we pre-reverse them here.
			DataFixed = massage_bool_list([ if Y =:= true -> 1; true -> 0 end || Y <- ValueToEncode ]),
			% We need to add (-L band 63) bytes of padding for alignment.
			MainData = << (<< <<X:1>> || X <- DataFixed >>)/bitstring, 0:(-length(DataFixed) band 63)/unsigned-little-integer>>,
			PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (1 bsl 32) bor (DataLen bsl 35),
			NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 63) bsr 6);
		{0, 0, 0, _, _} ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd;
		{PointerAsInt1, MainLen, ExtraLen, MainData, ExtraData} ->
			% Match vars are included above
			PointerAsInt = PointerAsInt1 bor ((OffsetToEnd + OldOffsetFromEnd) bsl 2),
			NewOffsetFromEnd = OldOffsetFromEnd + MainLen + ExtraLen;
		undefined ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd
	end.

-ast_fragment2([]).
ast_encode_struct_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, EncodeFun, StructSizePreformatted, StructLen]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen, FinalOffset, MainLen, ExtraLen, PointerAsInt1]}
	) ->
	case ValueToEncode of
		_ when is_list(ValueToEncode) ->
			DataLen = length(ValueToEncode),
			% We need to add (-L band 7) bytes of padding for alignment.
			{FinalOffset, MainData, ExtraData} = lists:foldl(fun
					(Element, {Offset, DataAcc, ExtraAcc}) ->
						% Call encode_Name once for each struct element
						{StructSizePreformatted, StructLen, ExtraLen, ThisBody, ThisExtra} = EncodeFun(Element, Offset - StructLen), % The function wants an offset from the /end/ of this struct
						% Must remember to account for the fact that next element starts StructLen further along.
						{ExtraLen + Offset - StructLen, [DataAcc, ThisBody], [ExtraAcc | ThisExtra]}
				end, {
					DataLen * StructLen, % This is the offset from the start of the first embedded struct, to the first word that will be after all embedded structs.
					[<< ((DataLen bsl 2) + StructSizePreformatted):64/unsigned-little-integer >>], % Struct lists start with a tag word.
					[] % Extra accumulator starts empty!
				}, ValueToEncode),
			FinalOffset = round(iolist_size(ExtraData) / 8),
			PointerAsInt = 1 bor ((OffsetToEnd + OldOffsetFromEnd) bsl 2) bor (7 bsl 32) bor ((DataLen * StructLen) bsl 35),
			NewOffsetFromEnd = OldOffsetFromEnd
						+ 1 % tag word
						+ DataLen * StructLen % list contents
						+ FinalOffset; % extra data length.
		{0, 0, 0, _, _} ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd;
		{PointerAsInt1, MainLen, ExtraLen, MainData, ExtraData} ->
			% Match vars are included above
			PointerAsInt = PointerAsInt1 bor ((OffsetToEnd + OldOffsetFromEnd) bsl 2),
			NewOffsetFromEnd = OldOffsetFromEnd + MainLen + ExtraLen;
		undefined ->
			ExtraData = <<>>,
			MainData = [],
			PointerAsInt = 0,
			NewOffsetFromEnd = OldOffsetFromEnd
	end.

to_list(Line, List) ->
	lists:foldr(fun (Item, SoFar) -> {cons, Line, Item, SoFar} end, {nil, Line}, List).

var_p(Line, _Prepend, {override, Name}) ->
	{var, Line, Name};
var_p(Line, Prepend, Value) ->
	{var, Line, to_atom(append(Prepend, Value))}.

% Need to be careful to gather bit-size elements /backwards/ inside each byte.
% Ignore for now.
% Also support only ints for now.
generate_data_binary(DesiredOffset, [#field_info{override=Override, default=Default, offset=DesiredOffset, type=Type=#native_type{width=Size, binary_options=BinType}, name=Name}|Rest], Direction, DWords) ->
	% Match an integer.
	Line = 0, % TODO
	RawVar = if Override =:= undefined -> var_p(Line, "Var", Name); true -> Override end,
	Var = case Direction of
		encode ->
			encoder(Type, Default, RawVar, Line);
		decode ->
			RawVar
	end,
	[{bin_element, Line, Var, {integer, Line, Size}, BinType}|generate_data_binary(DesiredOffset+Size, Rest, Direction, DWords)];
generate_data_binary(CurrentOffset, [#field_info{type=#native_type{width=0}}|Rest], Direction, DWords) ->
	% Just skip over void entries. They always have offset 0 and mess everything else up...
	generate_data_binary(CurrentOffset, Rest, Direction, DWords);
generate_data_binary(CurrentOffset, Rest=[#field_info{offset=DesiredOffset}|_], Direction, DWords) ->
	% Generate filler junk.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, DesiredOffset-CurrentOffset}, [integer]}|generate_data_binary(DesiredOffset, Rest, Direction, DWords)];
generate_data_binary(CurrentOffset, [], _Direction, DWords) when CurrentOffset == DWords * 64 ->
	[];
generate_data_binary(CurrentOffset, [], Direction, DWords) ->
	% Generate terminal junk.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, 64*DWords-CurrentOffset}, [integer]}].

generate_ptr_binary(DesiredOffset, [#field_info{override=undefined, offset=DesiredOffset, type=#ptr_type{}, name=Name}|Rest], Direction, PWords) ->
	% Match an integer.
	Line = 0, % TODO
	Var = case Direction of
		encode ->
			var_p(Line, "Ptr", Name);
		decode ->
			var_p(Line, "Var", Name)
	end,
	[{bin_element, Line, Var, {integer, Line, 64}, [little,unsigned,integer]}|generate_ptr_binary(DesiredOffset+64, Rest, Direction, PWords)];
generate_ptr_binary(CurrentOffset, Rest=[#field_info{offset=DesiredOffset}|_], Direction, PWords) ->
	% Generate filler junk. We only get this in unions.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, DesiredOffset-CurrentOffset}, [integer]}|generate_ptr_binary(DesiredOffset, Rest, Direction, PWords)];
generate_ptr_binary(CurrentOffset, [], _Direction, PWords) when CurrentOffset == PWords * 64 ->
	[];
generate_ptr_binary(CurrentOffset, [], Direction, PWords) ->
	% Generate terminal junk.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, 64*PWords-CurrentOffset}, [integer]}].

junkterm(Line, decode) ->
	{var, Line, '_'};
junkterm(Line, encode) ->
	{integer, Line, 0}.

% The code we generate to construct data to put into a binary.
encoder(#native_type{type=void}, undefined, Var, _Line) ->
	ast(case quote(Var) of undefined -> 0 end);
encoder(#native_type{type=integer}, 0, Var, _Line) ->
	Var;
encoder(#native_type{type=integer}, Default, Var, Line) ->
	ast(quote(Var) bxor quote({integer, Line, Default}));
encoder(#native_type{type=float}, 0.0, Var, _Line) ->
	Var;
encoder(#native_type{type=boolean}, false, Var, _Line) ->
	ast(case quote(Var) of false -> 0; true -> 1 end);
encoder(#native_type{type=boolean}, true, Var, _Line) ->
	ast(case quote(Var) of false -> 1; true -> 0 end);
encoder(#native_type{type=enum, extra=Enumerants}, Default, Var, Line) ->
	{Numbered, _Len} = lists:mapfoldl(fun (Elt, N) -> {{N bxor Default, Elt}, N+1} end, 0, Enumerants),
	% case Var of V1 -> 0; ... end
	{'case', Line, Var, [{clause, Line, [{atom, Line, Name}], [], [{integer, Line, Number}]} || {Number, Name} <- Numbered ]};
encoder(#ptr_type{}, undefined, Var, _Line) ->
	Var.

% The code we generate after matching a binary to put data into a record.
decoder(#native_type{type=void}, _Default, _Var, _Line, _MessageRef, _Schema) ->
	ast(undefined);
decoder(#native_type{type=integer}, 0, Var, _Line, _MessageRef, _Schema) ->
	% Special case for neater code.
	Var;
decoder(#native_type{type=integer}, Default, Var, Line, _MessageRef, _Schema) ->
	ast(quote(Var) bxor quote({integer, Line, Default}));
decoder(#native_type{type=float}, 0.0, Var, _Line, _MessageRef, _Schema) ->
	% TODO nonzero defaults. They are ugly!
	Var;
decoder(#native_type{type=boolean}, Default, Var, Line, _MessageRef, _Schema) ->
	ZeroValue = {atom, Line, Default},
	OneValue = {atom, Line, not Default},
	ast(case quote(Var) of 0 -> quote(ZeroValue); 1 -> quote(OneValue) end);
decoder(#native_type{type=enum, extra=Enumerants}, 0, Var, Line, _MessageRef, _Schema) ->
	Tuple = {tuple, Line, [{atom, Line, Name} || Name <- Enumerants ]},
	ast(element(quote(Var) + 1, quote(Tuple)));
decoder(#native_type{type=enum, extra=Enumerants}, Default, Var, Line, _MessageRef, _Schema) ->
	Tuple = {tuple, Line, [{atom, Line, Name} || Name <- Enumerants ]},
	ast(element((quote(Var) bxor quote({integer, Line, Default})) + 1, quote(Tuple)));
decoder(#ptr_type{type=struct, extra={TypeName, _, _}}, undefined, Var, Line, MessageRef, _Schema) ->
	Decoder = {'fun', Line, {function, to_atom(append("internal_decode_", TypeName)), 3}},
	ast(follow_struct_pointer(quote(Decoder), quote(Var), quote(MessageRef)));
decoder(#ptr_type{type=list, extra={struct, #ptr_type{type=struct, extra={TypeName, _, _}}}}, undefined, Var, Line, MessageRef, _Schema) ->
	Decoder = {'fun', Line, {function, to_atom(append("internal_decode_", TypeName)), 3}},
	ast(follow_tagged_struct_list_pointer(quote(Decoder), quote(Var), quote(MessageRef)));
decoder(#ptr_type{type=list, extra={primitive, #native_type{name=Name}}}, undefined, Var, Line, MessageRef, _Schema) ->
	Decoder = make_atom(Line, append("follow_", append(Name, "_list_pointer"))),
	ast((quote(Decoder))(quote(Var), quote(MessageRef)));
decoder(#ptr_type{type=list, extra={text, TextType}}, undefined, Var, Line, MessageRef, _Schema) ->
	DecoderName = to_atom(append("internal_decode_", TextType)),
	Decoder = {'fun', Line, {function, DecoderName, 3}},
	ast(follow_tagged_struct_list_pointer(quote(Decoder), quote(Var), quote(MessageRef)));
decoder(#ptr_type{type=text_or_data, extra=text}, undefined, Var, _Line, MessageRef, _Schema) ->
	ast(follow_text_pointer(quote(Var), quote(MessageRef)));
decoder(#ptr_type{type=text_or_data, extra=data}, undefined, Var, _Line, MessageRef, _Schema) ->
	ast(follow_data_pointer(quote(Var), quote(MessageRef)));
decoder(#group_type{type_id=TypeId}, undefined, _Var, Line, MessageRef, Schema) ->
	Decoder = make_atom(Line, decoder_name(TypeId, Schema)),
	ast((quote(Decoder))(Data, Pointers, quote(MessageRef)));
decoder(#ptr_type{}, _Default, _Var, _Line, _MessageRef, _Schema) ->
	ast(undefined). % not implemented

matcher(TypeId, Prefix, Line, Schema) ->
	SortedDataFields = find_notag_data_fields(TypeId, Schema),
	SortedPtrFields = find_notag_pointer_fields(TypeId, Schema),
	Groups = find_anon_union(TypeId, Schema) ++ find_notag_groups(TypeId, Schema),
	{record, Line, record_name(TypeId, Schema),
		[{record_field, Line, make_atom(Line, FieldName), field_matcher(Field, Prefix, Line, Schema)}
			|| Field=#field_info{name=FieldName} <- SortedDataFields ++ SortedPtrFields ++ Groups ]
	}.

field_matcher(Field=#field_info{type=#group_type{type_id=TypeId}, name=FieldName}, Prefix, Line, Schema) ->
	case is_union(TypeId, Schema) of
		true ->
			var_p(Line, Prefix, FieldName);
		false ->
			% Plain group
			matcher(TypeId, Prefix ++ binary_to_list(FieldName), Line, Schema)
	end;
field_matcher(#field_info{name=FieldName}, Prefix, Line, _Schema) ->
	var_p(Line, Prefix, FieldName).
