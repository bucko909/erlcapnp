%% @author bucko
%% @doc @todo Add description to capnp_compile.


-module(capnp_compile).

-include_lib("capnp.hrl").
-include_lib("capnp_raw.hrl").
-include_lib("capnp_bootstrap.hrl").

-export([
		to_ast/2
	]).

-record(field_info, {offset, type, name, default, discriminant}).
-record(native_type, {type, width, extra, binary_options, list_tag}).
-record(ptr_type, {type, extra}).
-record(group_type, {type_id}).

-compile({parse_transform, uberpt}).

% TODO:
%  void is not rendered. This is probably OK as we can basically delete void fields.
%  no lists
%  no structs
%  no unions
%  no groups

% Compiled reader functions take as arguments a #deref_ptr{} and a #envelope{} for pointer lookups.
% Compiled writer functions take data as argument, and return an io_list() which is locally consistent.
to_ast(Name, SchemaFile) when is_list(SchemaFile) ->
	Schema = capnp_bootstrap:load_raw_schema(SchemaFile),
	to_ast(Name, Schema);
to_ast(Name, Schema) ->
	to_ast([Name], sets:new(), [], [], Schema).

to_ast([], _Done, Recs, Funs, _Schema) ->
	Line = 0,
	Forms = [{attribute,1,file,{"capnp_test.erl",1}},{attribute,Line,module,capnp_test},{attribute,Line,compile,[export_all]}] ++ Recs ++ massage_bool_list() ++ Funs ++ [{eof,Line}],
	io:format("~p~n", [Forms]),
	io:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms), [{paper, 200}, {ribbon, 200}])]),
	{ok, capnp_test, BinData, []} = compile:forms(Forms, [debug_info, return]),
	code:load_binary(capnp_test, "capnp_test.beam", BinData);
to_ast([Name|Rest], Done, Recs, Funs, Schema) ->
	case sets:is_element(Name, Done) of
		true ->
			to_ast(Rest, Done, Recs, Funs, Schema);
		false ->
			{NewRec, NewFuns, NewStructNames} = to_ast_one(Name, Schema),
			to_ast(NewStructNames ++ Rest, sets:add_element(Name, Done), [NewRec|Recs], NewFuns ++ Funs, Schema)
	end.

to_ast_one(Name, Schema) ->
	TypeId = dict:fetch(Name, Schema#capnp_context.name_to_id),
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords,
				fields=Fields,
				discriminantCount=_DiscriminantCount % 0 if there is no anonymous union.
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	% Start by finding the bit offsets of each field, so that we can order them.
	AllFields = [ field_info(Field, Schema) || Field <- Fields ],
	io:format("~p~n", [AllFields]),
	{_Groups, Slots} = lists:partition(fun (#field_info{type=#group_type{}}) -> true; (_) -> false end, AllFields),
	{PtrFields, DataFields} = lists:partition(fun (#field_info{type=#native_type{}}) -> false; (_) -> true end, Slots),
	% We like these sorted by offset.
	SortedDataFields = lists:sort(DataFields),
	SortedPtrFields = lists:sort(PtrFields),

	Line = 0, % TODO
	DataMatcher = generate_data_binary(0, SortedDataFields, decode),
	PtrMatcher = generate_ptr_binary(0, SortedPtrFields, decode),

	RecordName = list_to_atom(binary_to_list(Name)),
	RecDef = {attribute, Line, record, {RecordName, [{record_field, Line, {atom, Line, list_to_atom(binary_to_list(FieldName))}} || #field_info{name=FieldName} <- SortedDataFields ++ SortedPtrFields]}},
	% function decode_<Name>(<<>>, StartOffset, CompleteMessage) -> #<Name>{}
	DecodeFunDef = {function, Line, list_to_atom("decode_" ++ binary_to_list(Name)), 1,
		[{clause, Line,
				[{bin, Line, DataMatcher ++ PtrMatcher}],
				[],
				[{record,Line,RecordName,
						[{record_field, Line, {atom, Line, list_to_atom(binary_to_list(FieldName))}, decoder(Type, var_p(Line, "Var", FieldName), Line)} || #field_info{name=FieldName, type=Type} <- SortedDataFields ++ SortedPtrFields ]
					}]
			}]},
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
	EncodeBody = encode_function_body(SortedDataFields, SortedPtrFields, Line),
	EncodeFunDef = {function, Line, list_to_atom("encode_" ++ binary_to_list(Name)), 2,
		[{clause, Line,
				[
					{record,Line,RecordName,
						[{record_field, Line, {atom, Line, list_to_atom(binary_to_list(FieldName))}, var_p(Line, "Var", FieldName)} || #field_info{name=FieldName} <- SortedDataFields ++ SortedPtrFields ]
					},
					{var, Line, 'PtrOffsetWordsFromEnd0'}
				],
				[],
				EncodeBody
			}]},
	EnvelopeFunDef = {function, Line, list_to_atom("envelope_" ++ binary_to_list(Name)), 1,
		[{clause, Line,
				[
					{var, Line, 'Input'}
				],
				[],
				ast_envelope(DWords, PWords, Name, Line)
			}]},
	ExtraTypes1 = [ TypeName || #field_info{type=#ptr_type{type=struct, extra={TypeName, _, _}}} <- SortedPtrFields ],
	ExtraTypes2 = [ TypeName || #field_info{type=#ptr_type{type=list, extra={struct, #ptr_type{type=struct, extra={TypeName, _, _}}}}} <- SortedPtrFields ],
	{RecDef, [EncodeFunDef, DecodeFunDef, EnvelopeFunDef], ExtraTypes1 ++ ExtraTypes2}.

encode_function_body(SortedDataFields, SortedPtrFields, Line) ->
	DataMaker = generate_data_binary(0, SortedDataFields, encode),
	PtrMaker = generate_ptr_binary(0, SortedPtrFields, encode),
	EncodePointers = lists:append([ ast_encode_ptr(N, length(SortedPtrFields), Type, FieldName, Line) || {N, #field_info{name=FieldName, type=Type=#ptr_type{}}} <- lists:zip(lists:seq(1, length(SortedPtrFields)), SortedPtrFields) ]),
	EncodePointers ++ [
		{tuple, Line, [
				{op, Line, '-', var_p(Line, "PtrOffsetWordsFromEnd", length(SortedPtrFields)), {var, Line, 'PtrOffsetWordsFromEnd0'}},
				{bin, Line, DataMaker ++ PtrMaker},
				to_list(Line, lists:append([ [ var_p(Line, "Data", N), var_p(Line, "Extra", N) ] || N <- lists:seq(1, length(SortedPtrFields)) ]))
		]}
	].

to_list(Line, List) ->
	lists:foldr(fun (Item, SoFar) -> {cons, Line, Item, SoFar} end, {nil, Line}, List).

ast_envelope(DWords, PWords, Name, Line) ->
	EncodeFun = {atom, Line, list_to_atom("encode_" ++ binary_to_list(Name))},
	ast_envelope_({integer, Line, DWords}, {integer, Line, PWords}, {integer, Line, 1+DWords+PWords}, {var, Line, 'Input'}, EncodeFun).

-ast_fragment([]).
ast_envelope_(DWordsInt, PWordsInt, CommonLenInt, InputVar, EncodeFun) ->
	{ExtraDataLen, MainData, ExtraData} = EncodeFun(InputVar, 0),
	list_to_binary([
			<<
				% Segment envelope
				0:32/unsigned-little-integer, % Segcount - 1. Always 0 for us.
				(CommonLenInt+ExtraDataLen):32/unsigned-little-integer, % Seglen = 1 + DWords + PWords + ExtraLen

				% Pointer to first struct
				0:32/unsigned-little-integer, % Offset of data, starting from end of this pointer, after the struct-type tag. Always 0 for us.
				DWordsInt:16/unsigned-little-integer, % Number of data words in the struct.
				PWordsInt:16/unsigned-little-integer % Number of pointer words in the struct.
			>>,

			% Now the actual data. This may be an io_list, so we can't just /binary it in.
			MainData,
			ExtraData
	]).

ast_encode_ptr_common(N, PtrLen0, AstFun, ExtraInputParams, VarName, Line) ->
	[OldPtrOffsetWordsFromEndVar, NewPtrOffsetWordsFromEndVar] = [ var_p(Line, "PtrOffsetWordsFromEnd", OldOrNew) || OldOrNew <- [N-1, N] ],
	OffsetToEndInt = {integer, Line, PtrLen0 - N},
	MatchVar = var_p(Line, "Var", VarName),
	CommonIn = [OldPtrOffsetWordsFromEndVar, OffsetToEndInt, MatchVar],

	PtrVar = var_p(Line, "Ptr", VarName),
	[DataVar, ExtraVar] = [ var_p(Line, VN, N) || VN <- ["Data", "Extra"] ],
	CommonOut = [NewPtrOffsetWordsFromEndVar, PtrVar, DataVar, ExtraVar],

	AstFun(
		{in, CommonIn ++ ExtraInputParams},
		{out, CommonOut},
		{temp_suffix, binary_to_list(VarName)}
	).

% This is just a variable aligning function which passes through to ast_encode_ptr_
ast_encode_ptr(N, PtrLen0, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}, VarName, Line) ->
	EncodeFun = {atom, Line, list_to_atom("encode_" ++ binary_to_list(TypeName))},
	StructHeaderNumbers = {integer, Line, (DataLen bsl 32) + (PtrLen bsl 48)},
	StructLen = {integer, Line, DataLen + PtrLen},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_struct_/3, [EncodeFun, StructHeaderNumbers, StructLen], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=text_or_data, extra=text}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_text_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=text_or_data, extra=data}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_data_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={primitive, bool}}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_bool_list_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={primitive, Type}}, VarName, Line) ->
	#native_type{list_tag=WidthType, width=Width, binary_options=BinType} = Type,
	WidthTypeInt = {integer, Line, WidthType},
	WidthInt = {integer, Line, Width},
	% I can't << X || ... >> is invalid syntax, and << <<X>> || ... >> doesn't let me specify encoding types, so I
	% have to write the whole comprehension by hand! Woe!
	MatchVar = var_p(Line, "Var", VarName),
	EncodedX = {bc, Line, {bin, Line, [{bin_element, Line, encoder(Type, {var, Line, 'X'}, Line), WidthInt, BinType}]}, [{generate,199,{var,Line,'X'}, MatchVar}]},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_primitive_list_/3, [WidthInt, WidthTypeInt, EncodedX], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={struct, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}}}, VarName, Line) ->
	EncodeFun = {atom, Line, list_to_atom("encode_" ++ binary_to_list(TypeName))},
	StructSizePreformatted = {integer, Line, (DataLen bsl 32) + (PtrLen bsl 48)},
	StructLen = {integer, Line, DataLen + PtrLen},
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
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, EncodeFun, StructSizePreformatted, StructLen]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [ExtraLen]}
	) ->
	{ExtraLen, MainData, ExtraData} = EncodeFun(ValueToEncode, 0),
	PointerAsInt = ((OldOffsetFromEnd + OffsetToEnd) bsl 2) + StructSizePreformatted,
	NewOffsetFromEnd = OldOffsetFromEnd + ExtraLen + StructLen.

-ast_fragment2([]).
ast_encode_text_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen]}
	) ->
	ExtraData = <<>>,
	DataLen = iolist_size(ValueToEncode)+1,
	% We need to add a null terminator.
	% Then we need to add (-L band 7) bytes of padding for alignment.
	MainData = [ValueToEncode, <<0:8, 0:((-DataLen band 7)*8)/unsigned-little-integer>>],
	PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 7) bsr 3).

-ast_fragment2([]).
ast_encode_data_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen]}
	) ->
	ExtraData = <<>>,
	DataLen = iolist_size(ValueToEncode),
	% Then we need to add (-L band 7) bytes of padding for alignment.
	MainData = [ValueToEncode, <<0:((-DataLen band 7)*8)/unsigned-little-integer>>],
	PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 7) bsr 3).

-ast_fragment2([]).
ast_encode_primitive_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, Width, WidthType, EncodedX]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen]}
	) ->
	ExtraData = <<>>,
	DataLen = length(ValueToEncode),
	% We need to add (-L*W band 63) bytes of padding for alignment.
	MainData = [EncodedX, <<0:(-DataLen*Width band 63)/unsigned-little-integer>>],
	PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (WidthType bsl 32) bor (DataLen bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen * Width + 63) bsr 6).

-ast_fragment2([]).
ast_encode_bool_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen, DataFixed]}
	) ->
	ExtraData = <<>>,
	DataLen = length(ValueToEncode),
	% Because Erlang will reverse blocks of 8 bools, we pre-reverse them here.
	DataFixed = massage_bool_list([ if Y =:= true -> 1; true -> 0 end || Y <- ValueToEncode ]),
	% We need to add (-L band 63) bytes of padding for alignment.
	MainData = [ << <<X:1>> || X <- DataFixed >>, <<0:(-length(DataFixed) band 63)/unsigned-little-integer>>],
	PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (1 bsl 32) bor (DataLen bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 63) bsr 6).

-ast_fragment2([]).
ast_encode_struct_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, EncodeFun, StructSizePreformatted, StructLen]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen, FinalOffset]}
	) ->
	DataLen = length(ValueToEncode),
	% We need to add (-L band 7) bytes of padding for alignment.
	{FinalOffset, MainData, ExtraData} = lists:foldl(fun
			(Element, {Offset, DataAcc, ExtraAcc}) ->
				% Call encode_Name once for each struct element
				{ExtraLen, ThisData, ThisExtra} = EncodeFun(Element, Offset - StructLen), % The function wants an offset from the /end/ of this struct
				% Must remember to account for the fact that next element starts StructLen further along.
				{ExtraLen + Offset - StructLen, [DataAcc|ThisData], [ExtraAcc|ThisExtra]}
		end, {
			DataLen * StructLen, % This is the offset from the start of the first embedded struct, to the first word that will be after all embedded structs.
			[<< ((DataLen bsl 2) + StructSizePreformatted):64/unsigned-little-integer >>], % Struct lists start with a tag word.
			[] % Extra accumulator starts empty!
		}, ValueToEncode),
	FinalOffset = round(iolist_size(ExtraData) / 8),
	DataLen = round(iolist_size(MainData) / 8 / StructLen),
	PointerAsInt = 1 bor ((OffsetToEnd + OldOffsetFromEnd) bsl 2) bor (7 bsl 32) bor ((DataLen * StructLen) bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd
				+ 1 % tag word
				+ DataLen * StructLen % list contents
				+ FinalOffset. % extra data length.

massage_bool_list() ->
	Var = {var, 0, 'List'},
	[{function, 0, massage_bool_list, 1,
		[{clause, 0,
				[
					Var
				],
				[],
				ast_massage_bool_list_(Var)
			}]}].

-ast_fragment([]).
ast_massage_bool_list_(List) ->
	try lists:split(8, List) of
		{First, Last} ->
			lists:reverse(First) ++ massage_bool_list(Last)
	catch
		error:badarg ->
			lists:reverse(List ++ lists:duplicate(-length(List) band 7, 0))
	end.

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_binary(A) -> binary_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A);
to_list(A) when is_list(A) -> A.

var_p(Line, Prepend, Value) ->
	{var, Line, list_to_atom(to_list(Prepend) ++ to_list(Value))}.

% Need to be careful to gather bit-size elements /backwards/ inside each byte.
% Ignore for now.
% Also support only ints for now.
generate_data_binary(DesiredOffset, [#field_info{offset=DesiredOffset, type=Type=#native_type{width=Size, binary_options=BinType}, name=Name}|Rest], Direction) ->
	% Match an integer.
	Line = 0, % TODO
	RawVar = var_p(Line, "Var", Name),
	Var = case Direction of
		encode ->
			encoder(Type, RawVar, Line);
		decode ->
			RawVar
	end,
	[{bin_element, Line, Var, {integer, Line, Size}, BinType}|generate_data_binary(DesiredOffset+Size, Rest, Direction)];
generate_data_binary(CurrentOffset, Rest=[#field_info{offset=DesiredOffset}|_], Direction) ->
	% Generate filler junk.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, DesiredOffset-CurrentOffset}, [integer]}|generate_data_binary(DesiredOffset, Rest, Direction)];
generate_data_binary(CurrentOffset, [], _Direction) when CurrentOffset rem 64 == 0 ->
	[];
generate_data_binary(CurrentOffset, [], Direction) ->
	% Generate terminal junk.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, 64-(CurrentOffset rem 64)}, [integer]}].

% We shouldn't have gaps in this case, but we sanity check that.
generate_ptr_binary(DesiredOffset, [#field_info{offset=DesiredOffset, type=#ptr_type{}, name=Name}|Rest], Direction) ->
	% Match an integer.
	Line = 0, % TODO
	Var = case Direction of
		encode ->
			var_p(Line, "Ptr", Name);
		decode ->
			var_p(Line, "Var", Name)
	end,
	[{bin_element, Line, Var, {integer, Line, 64}, [little,unsigned,integer]}|generate_ptr_binary(DesiredOffset+64, Rest, Direction)];
generate_ptr_binary(_CurrentOffset, [], _Direction) ->
	[].

junkterm(Line, decode) ->
	{var, Line, '_'};
junkterm(Line, encode) ->
	{integer, Line, 0}.

% The code we generate to construct data to put into a binary.
encoder(#native_type{type=integer}, Var, _Line) ->
	Var;
encoder(#native_type{type=float}, Var, _Line) ->
	Var;
encoder(#native_type{type=boolean}, Var, Line) ->
	{'if',Line,[{clause,Line,[],[[Var]],[{integer,Line,1}]},{clause,Line,[],[[{atom,Line,true}]],[{integer,Line,0}]}]};
encoder(#native_type{type=enum, extra=Enumerants}, Var, Line) ->
	{Numbered, _Len} = lists:mapfoldl(fun (Elt, N) -> {{N, Elt}, N+1} end, 0, Enumerants),
	% case Var of V1 -> 0; ... end
	{'case', Line, Var, [{clause, Line, [{atom, Line, Name}], [], [{integer, Line, Number}]} || {Number, Name} <- Numbered ]};
encoder(#ptr_type{}, Var, _Line) ->
	Var.

% The code we generate after matching a binary to put data into a record.
decoder(#native_type{type=integer}, Var, _Line) ->
	Var;
decoder(#native_type{type=float}, Var, _Line) ->
	Var;
decoder(#native_type{type=boolean}, Var, Line) ->
	{'if',Line,[{clause,Line,[],[[{op,Line,'=:=',Var,{integer,Line,1}}]],[{atom,Line,true}]},{clause,Line,[],[[{atom,Line,true}]],[{atom,Line,false}]}]};
decoder(#native_type{type=enum, extra=Enumerants}, Var, Line) ->
	Tuple = {tuple, Line, [{atom, Line, Name} || Name <- Enumerants ]},
	% erlang:element(Var+1, {V1, ...})
	{call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,element}},[{op,Line,'+',Var,{integer,14,1}},Tuple]};
decoder(#ptr_type{}, Var, _Line) ->
	Var.

field_info(#'capnp::namespace::Field'{
		discriminantValue=DiscriminantValue,
		name=Name,
		''={{0,slot},
			#'capnp::namespace::Field::::slot'{
				offset=N,
				defaultValue=#'capnp::namespace::Value'{''={{_,TypeClass},DefaultValue}},
				type=Type=#'capnp::namespace::Type'{''={{_,TypeClass},_TypeDescription}}
			}
		}
	}, Schema) ->
	{Size, Info} = type_info(Type, Schema),
	#field_info{offset=Size*N, type=Info, name=Name, discriminant=if DiscriminantValue =:= 65535 -> undefined; true -> DiscriminantValue end, default=DefaultValue};
field_info(#'capnp::namespace::Field'{
		discriminantValue=DiscriminantValue,
		name=Name,
		''={{1,group},
			#'capnp::namespace::Field::::group'{
				typeId=TypeId
			}
		}
	}, _Schema) ->
	% Groups and unions.
	#field_info{offset=undefined, type=#group_type{type_id=TypeId}, name=Name, discriminant=if DiscriminantValue =:= 65535 -> undefined; true -> DiscriminantValue end}.

type_info(#'capnp::namespace::Type'{''={{_,TypeClass},TypeDescription}}, Schema) ->
	type_info(TypeClass, TypeDescription, Schema).

% Pointer types (composite/list)
type_info(TextType, void, _Schema) when TextType =:= text; TextType =:= data ->
	{64, #ptr_type{type=text_or_data, extra=TextType}};
type_info(anyPointer, void, _Schema) ->
	{64, #ptr_type{type=unknown}}; % Not really possible
type_info(struct, #'capnp::namespace::Type::::struct'{typeId=TypeId}, Schema) when is_integer(TypeId) ->
	{TypeName, DataLen, PtrLen} = node_name(TypeId, Schema),
	{64, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}};
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,enum},#'capnp::namespace::Type::::enum'{typeId=TypeId}}}}, Schema) ->
	% List of enums.
	EnumerantNames = enumerant_names(TypeId, Schema),
	{64, #ptr_type{type=list, extra={primitive, #native_type{type=enum, extra=EnumerantNames, width=16, binary_options=[little,unsigned,integer], list_tag=3}}}};
type_info(list, #'capnp::namespace::Type'{''={{_,TextType},void}}, _Schema) when TextType =:= text; TextType =:= data ->
	% List of text types; this is a list-of-lists.
	erlang:error({not_implemented, list, TextType}); % TODO
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,bool},void}}}, _Schema) ->
	% List of bools. While this /could/ encode a list of 1-bit ints, erlang makes it hard by reversing our bits.
	% So we need to special case it!
	{64, #ptr_type{type=list, extra={primitive, bool}}};
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,PrimitiveType},void}}}, _Schema) ->
	% List of any normal primitive type.
	{64, #ptr_type{type=list, extra={primitive, builtin_info(PrimitiveType)}}};
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,PtrType},_LTypeDescription}}}, _Schema) when PtrType =:= list; PtrType =:= text; PtrType =:= data ->
	% List of list, or list-of-(text or data) -- all three are lists of lists of lists.
	erlang:error({not_implemented, list, list}); % TODO
type_info(list, #'capnp::namespace::Type::::list'{elementType=InnerType=#'capnp::namespace::Type'{''={{_,struct},_}}}, Schema) ->
	% List of structs.
	% These will be encoded in-line.
	{64, TypeInfo} = type_info(InnerType, Schema),
	{64, #ptr_type{type=list, extra={struct, TypeInfo}}};
type_info(list, #'capnp::namespace::Type'{''={{_,anyPointer},void}}, _Schema) ->
	erlang:error({not_implemented, list, anyPointer}); % TODO
type_info(list, #'capnp::namespace::Type'{''={{_,interface},_LTypeId}}, _Schema) ->
	erlang:error({not_implemented, list, interface}); % TODO
% TODO union variables.
% TODO group variables.
% TODO decoders for pointers.
% Data types
type_info(enum, #'capnp::namespace::Type::::enum'{typeId=TypeId}, Schema) when is_integer(TypeId) ->
	EnumerantNames = enumerant_names(TypeId, Schema),
	{16, #native_type{type=enum, extra=EnumerantNames, width=16, binary_options=[little,unsigned,integer], list_tag=3}};
type_info(TypeClass, void, _Schema) ->
	Info1 = #native_type{width=Size1} = builtin_info(TypeClass),
	{Size1, Info1};
% Catchall
type_info(TypeClass, TypeDescription, _Schema) ->
	io:format("Unknown: ~p~n", [{TypeClass, TypeDescription}]),
	{64, #ptr_type{type=unknown}}.

enumerant_names(TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{2, enum},
			#'capnp::namespace::Node::::enum'{
				enumerants=Enumerants
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	[ list_to_atom(binary_to_list(EName)) || #'capnp::namespace::Enumerant'{name=EName} <- Enumerants ].

node_name(TypeId, Schema) ->
	#'capnp::namespace::Node'{
		displayName=Name,
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	{Name, DWords, PWords}.

% {BroadType, Bits, BinaryType}
builtin_info(int64) -> #native_type{type=integer, width=64, binary_options=[little, signed, integer], list_tag=5};
builtin_info(int32) -> #native_type{type=integer, width=32, binary_options=[little, signed, integer], list_tag=4};
builtin_info(int16) -> #native_type{type=integer, width=16, binary_options=[little, signed, integer], list_tag=3};
builtin_info(int8) -> #native_type{type=integer, width=8, binary_options=[little, signed, integer], list_tag=2};
builtin_info(uint64) -> #native_type{type=integer, width=64, binary_options=[little, unsigned, integer], list_tag=5};
builtin_info(uint32) -> #native_type{type=integer, width=32, binary_options=[little, unsigned, integer], list_tag=4};
builtin_info(uint16) -> #native_type{type=integer, width=16, binary_options=[little, unsigned, integer], list_tag=3};
builtin_info(uint8) -> #native_type{type=integer, width=8, binary_options=[little, unsigned, integer], list_tag=8};
builtin_info(float32) -> #native_type{type=float, width=32, binary_options=[float], list_tag=4};
builtin_info(float64) -> #native_type{type=float, width=64, binary_options=[float], list_tag=5};
builtin_info(bool) -> #native_type{type=boolean, width=1, binary_options=[integer], list_tag=1};
builtin_info(void) -> #native_type{type=void, width=0, binary_options=[integer], list_tag=0}.

% TODO lists of lists.
