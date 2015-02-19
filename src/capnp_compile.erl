%% @author bucko
%% @doc @todo Add description to capnp_compile.


-module(capnp_compile).

-include_lib("capnp.hrl").
-include_lib("capnp_raw.hrl").
-include_lib("capnp_bootstrap.hrl").

-export([
		to_ast/2
	]).

-record(field_info, {offset, type, name, default}).
-record(native_type, {type, width, extra, binary_options}).
-record(ptr_type, {type, extra}).

-compile({parse_transform, uberpt}).

% TODO:
%  void is not rendered. This is probably OK as we can basically delete void fields.
%  no lists
%  no structs
%  no unions
%  no groups

% Compiled reader functions take as arguments a #deref_ptr{} and a #envelope{} for pointer lookups.
% Compiled writer functions take data as argument, and return an io_list() which is locally consistent.
to_ast(Name, Schema) ->
	to_ast([Name], sets:new(), [], [], Schema).

to_ast([], _Done, Recs, Funs, _Schema) ->
	Line = 0,
	Forms = [{attribute,1,file,{"capnp_test.erl",1}},{attribute,Line,module,capnp_test},{attribute,Line,compile,[export_all]}] ++ Recs ++ Funs ++ [{eof,Line}],
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
				fields=Fields
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	% Start by finding the bit offsets of each field, so that we can order them.
	AllFields = [ field_info(Field, Schema) || Field <- Fields ],
	io:format("~p~n", [AllFields]),
	{PtrFields, DataFields} = lists:partition(fun (#field_info{type=#native_type{}}) -> false; (_) -> true end, AllFields),
	% We like these sorted.
	SortedDataFields = lists:sort(DataFields),
	SortedPtrFields = lists:sort(PtrFields),

	Line = 0, % TODO
	DataMatcher = generate_data_binary(0, SortedDataFields, decode),
	DataMaker = generate_data_binary(0, SortedDataFields, encode),
	PtrMatcher = generate_ptr_binary(0, SortedPtrFields, decode),
	PtrMaker = generate_ptr_binary(0, SortedPtrFields, encode),

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
	EncodePointers = lists:append([ ast_encode_ptr(N, length(PtrFields), Type, FieldName, Line) || {N, #field_info{name=FieldName, type=Type=#ptr_type{}}} <- lists:zip(lists:seq(1, length(SortedPtrFields)), SortedPtrFields) ]),
	EncodeFunDef = {function, Line, list_to_atom("encode_" ++ binary_to_list(Name)), 2,
		[{clause, Line,
				[
					{record,Line,RecordName,
						[{record_field, Line, {atom, Line, list_to_atom(binary_to_list(FieldName))}, var_p(Line, "Var", FieldName)} || #field_info{name=FieldName} <- SortedDataFields ++ SortedPtrFields ]
					},
					{var, Line, 'PtrOffsetWordsFromEnd0'}
				],
				[],
				EncodePointers ++ [
					{tuple, Line, [
							{op, Line, '-', var_p(Line, "PtrOffsetWordsFromEnd", length(PtrFields)), {var, Line, 'PtrOffsetWordsFromEnd0'}},
							{bin, Line, DataMaker ++ PtrMaker},
							to_list(Line, lists:append([ [ var_p(Line, "Data", N), var_p(Line, "Extra", N) ] || N <- lists:seq(1, length(PtrFields)) ]))
					]}
				]
			}]},
	EnvelopeFunDef = {function, Line, list_to_atom("envelope_" ++ binary_to_list(Name)), 1,
		[{clause, Line,
				[
					{var, Line, 'Input'}
				],
				[],
				ast_envelope(DWords, PWords, Name, Line)
			}]},
	ExtraTypes = [ TypeName || #field_info{type=#ptr_type{type=struct, extra={TypeName, _, _}}} <- SortedPtrFields ],
	{RecDef, [EncodeFunDef, DecodeFunDef, EnvelopeFunDef], ExtraTypes}.

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


% This is just a variable aligning function which passes through to ast_encode_ptr_
ast_encode_ptr(N, PtrLen0, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}, VarName, Line) ->
	[ExtraLenVar, DataVar, ExtraVar] = [ var_p(Line, VN, N) || VN <- ["ExtraLen", "Data", "Extra"] ],
	EncodeFun = {atom, Line, list_to_atom("encode_" ++ binary_to_list(TypeName))},
	MatchVar = var_p(Line, "Var", VarName),
	PtrVar = var_p(Line, "Ptr", VarName),
	StructHeaderNumbers = {integer, Line, (DataLen bsl 32) + (PtrLen bsl 48)},
	StructLen = {integer, Line, DataLen + PtrLen},
	PtrOffset = {op, Line, '+', var_p(Line, "PtrOffsetWordsFromEnd", N-1), {integer, Line, PtrLen0 - N}},
	[OldPtrOffsetWordsFromEndVar, NewPtrOffsetWordsFromEndVar] = [ var_p(Line, "PtrOffsetWordsFromEnd", OldOrNew) || OldOrNew <- [N-1, N] ],
	ast_encode_struct_(ExtraLenVar, DataVar, ExtraVar, EncodeFun, MatchVar, PtrVar, PtrOffset, StructHeaderNumbers, OldPtrOffsetWordsFromEndVar, NewPtrOffsetWordsFromEndVar, StructLen).

% This is the fragment that's inserted for each pointer.
-ast_fragment([]).
ast_encode_struct_(ExtraLenVar, DataVar, ExtraVar, EncodeFun, MatchVar, PtrVar, PtrOffset, StructHeaderNumbers, OldOffsetWordsFromEndVar, NewPtrOffsetWordsFromEndVar, StructLen) ->
	{ExtraLenVar, DataVar, ExtraVar} = EncodeFun(MatchVar, 0),
	PtrVar = (PtrOffset bsl 2) + StructHeaderNumbers,
	NewPtrOffsetWordsFromEndVar = OldOffsetWordsFromEndVar + ExtraLenVar + StructLen.

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
		name=Name,
		''={{0,slot},
			#'capnp::namespace::Field::::slot'{
				offset=N,
				defaultValue=#'capnp::namespace::Value'{''={{_,TypeClass},DefaultValue}},
				type=#'capnp::namespace::Type'{''={{_,TypeClass},TypeDescription}}
			}
		}   
	}, Schema) ->
	{Offset, Info} = case {TypeClass, TypeDescription} of
		% Data types
		{text, void} ->
			{N * 64, #ptr_type{type=unknown}}; % TODO
		{data, void} ->
			{N * 64, #ptr_type{type=unknown}}; % TODO
		{anyPointer, void} ->
			{N * 64, #ptr_type{type=unknown}}; % Not really possible
		{_, void} ->
			Info1 = #native_type{width=Size1} = builtin_info(TypeClass),
			{N * Size1, Info1};
		{enum, #'capnp::namespace::Type::::enum'{typeId=TypeId}} when is_integer(TypeId) ->
			EnumerantNames = enumerant_names(TypeId, Schema),
			{N * 16, #native_type{type=enum, extra=EnumerantNames, width=16, binary_options=[little,unsigned,integer]}};
		% Pointer types (composite/list)
		{struct, #'capnp::namespace::Type::::struct'{typeId=TypeId}} when is_integer(TypeId) ->
			{TypeName, DataLen, PtrLen} = node_name(TypeId, Schema),
			{N * 64, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}};
		_ ->
			{N * 64, #ptr_type{type=unknown}} % TODO
	end,
	#field_info{offset=Offset, type=Info, name=Name, default=DefaultValue}.

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
builtin_info(int64) -> #native_type{type=integer, width=64, binary_options=[little, signed, integer]};
builtin_info(int32) -> #native_type{type=integer, width=32, binary_options=[little, signed, integer]};
builtin_info(int16) -> #native_type{type=integer, width=16, binary_options=[little, signed, integer]};
builtin_info(int8) -> #native_type{type=integer, width=8, binary_options=[little, signed, integer]};
builtin_info(uint64) -> #native_type{type=integer, width=64, binary_options=[little, unsigned, integer]};
builtin_info(uint32) -> #native_type{type=integer, width=32, binary_options=[little, unsigned, integer]};
builtin_info(uint16) -> #native_type{type=integer, width=16, binary_options=[little, unsigned, integer]};
builtin_info(uint8) -> #native_type{type=integer, width=8, binary_options=[little, unsigned, integer]};
builtin_info(float32) -> #native_type{type=float, width=32, binary_options=[float]};
builtin_info(float64) -> #native_type{type=float, width=64, binary_options=[float]};
builtin_info(bool) -> #native_type{type=boolean, width=1, binary_options=[integer]};
builtin_info(void) -> #native_type{type=void, width=0, binary_options=[integer]}.

% Convert a record into a segment, starting with the pointer to the binary.
to_bytes(Rec, Schema) ->
	Name = element(1, Rec),
	TypeId = dict:fetch(Name, Schema#capnp_context.name_to_id),
	{DWords, PWords, Raw, _RawSize, Extra, _FinalOffset} = to_bytes(Schema, TypeId, Rec, 0),
	list_to_binary([<<(struct_pointer(0, DWords, PWords)):?UInt64>>, Raw | Extra]).

% OffsetAfterHere is num words /after/ the pointer that the data segment starts. It will therefore often be zero.
struct_pointer(OffsetAfterHere, DWords, PWords) ->
	(OffsetAfterHere bsl 2) bor (DWords bsl 32) bor (PWords bsl 48).

% SizeTag is 0-index into bit sizes: {0, 1, 8, 16, 32, 64, 64(Pointer), 64-ish(Composite)}
% ElementCount is /word/ count in Composite case; we actually just special case this below.
plain_list_pointer(OffsetAfterHere, SizeTag, ElementCount) ->
	1 bor (OffsetAfterHere bsl 2) bor (SizeTag bsl 32) bor (ElementCount bsl 35).

% First elt is a pointer. Second is a list tag which looks a bit like a struct pointer; it should go on the start of the list elements.
composite_list_pointer(OffsetAfterHere, DWords, PWords, ElementCount) ->
	{
		1 bor (OffsetAfterHere bsl 2) bor (7 bsl 32) bor ((ElementCount*(DWords+PWords)) bsl 35),
		struct_pointer(ElementCount, DWords, PWords) % It's a bit ugly to call struct_pointer here, but it /does/ do the right thing.
	}.

% Convert a record into a byte stream. Each sub-structure will be placed immediately after this one in left-to-right order.
% Returns the data/pointer words in our structure, an unflattened io_list of the encoded structure and the total encoded length (in words).
to_bytes(Schema, TypeId, Obj) ->
	to_bytes(Schema, TypeId, Obj, 0).
to_bytes(Schema, TypeId, Obj, POffset) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords,
				fields=Fields
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	{DataSeg, PointerSeg, ExtraData, ExtraDataLength} = encode_parts(Fields, tl(tuple_to_list(Obj)), list_to_tuple(lists:duplicate(DWords, 0)), list_to_tuple(lists:duplicate(PWords, 0)), POffset, [], Schema),
	Data = [flatten_seg(DataSeg)| flatten_seg(PointerSeg)],
	DataLength = DWords + PWords,
	{DWords, PWords, Data, DataLength, ExtraData, ExtraDataLength - POffset}.

encode_parts([
		#'capnp::namespace::Field'{
			''={{0,slot},
				#'capnp::namespace::Field::::slot'{
					offset=N,
					defaultValue=#'capnp::namespace::Value'{''={{_,TypeClass},DefaultValue}},
					type=#'capnp::namespace::Type'{''={{_,TypeClass},TypeDescription}}
				}
			}   
		}
		|RestFields], [Value|RestValues], DataSeg, PointerSeg, DataLength, Data, Schema) ->
	{NewDataSeg, NewPointerSeg, ExtraDataLength, ExtraData} = encode_field(TypeClass, TypeDescription, DefaultValue, N, Value, DataSeg, PointerSeg, DataLength, Schema),
	encode_parts(RestFields, RestValues, NewDataSeg, NewPointerSeg, DataLength + ExtraDataLength, [Data|ExtraData], Schema);
encode_parts(_, [], DataSeg, PointerSeg, ExtraDataLength, ExtraData, _Schema) ->
	% Offset is total data length of everything /extra/ we've put in.
	{DataSeg, PointerSeg, ExtraData, ExtraDataLength}.

% We actually don't need to care about the DefaultValue except for primitive fields.
% For composite fields, the default is either a null pointer, or a valid encoding of the entire structure as if it were set manually.
% We could save space here by potentially verifying that the default /is/ the default value, and encoding as a null pointer.
encode_field(TypeClass, TypeDescription, DefaultValue, N, Value, DataSeg, PointerSeg, ExtraDataLength, Schema) ->
	case {TypeClass, TypeDescription} of
		{anyPointer, void} ->
			erlang:error(not_implemented);
		{TextType, void} when TextType =:= text; TextType =:= data ->
			{Binary, ByteSize} = encode_text(TextType, Value),
			Pointer = plain_list_pointer(ExtraDataLength + (tuple_size(PointerSeg) - (N + 1)), 2, ByteSize),
			PadLength = -ByteSize band 7,
			Pad = << <<0:8>> || _ <- lists:seq(1, PadLength) >>,
			WordSize = (ByteSize + PadLength) bsr 3,
			NewPointerSeg = insert(N, PointerSeg, Pointer),
			{DataSeg, NewPointerSeg, WordSize, [Binary|Pad]};
		{_, void} ->
			{Shifts, Encoded} = encode(TypeClass, Value, DefaultValue, N),
			NewDataSeg = insert((N bsl (Shifts - 6)), DataSeg, Encoded),
			{NewDataSeg, PointerSeg, 0, []};
		{struct, #'capnp::namespace::Type::::struct'{typeId=TypeId}} when is_integer(TypeId)-> % TODO are these working fine in bootstrap_capnp? They're a :group.
			{DWords, PWords, Data, DataWords, NewExtraData, FinalOffset} = to_bytes(Schema, TypeId, Value, 0),
			% We're going to jam the new data on the end of the accumulator, so we must add the length of every structure we've added so far.
			% We also need to include the length of every pointer /after/ this one. Not that the first pointer is N=0.
			Pointer = struct_pointer(ExtraDataLength + (tuple_size(PointerSeg) - (N + 1)), DWords, PWords),
			NewPointerSeg = insert(N, PointerSeg, Pointer),
			{DataSeg, NewPointerSeg, DataWords + FinalOffset, [Data|NewExtraData]};
		{list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,PtrType},LTypeDescription}}}} when PtrType =:= list; PtrType =:= text; PtrType =:= data ->
			% Start the encode from the end of the list. Append the data, and prepend the pointers.
			% This means that the first pointer is always just a zero pointer.
			% The second must skip one list element, and all of the data that element caused.
			% Etc.
			FoldFun = fun (V, {I, Pointers, Data, DataLength}) ->
					{{}, {Pointer}, NewDataLength, NewData} = encode_field(PtrType, LTypeDescription, _Default=0, _N=0, V, _DataSeg={}, _PointerSeg={0}, DataLength+I, Schema),
					{I+1, [<<Pointer:?UInt64>>|Pointers], [Data|NewData], NewDataLength + DataLength}
			end,
			{ListLength, Pointers, Data, DataLength} = lists:foldr(FoldFun, {0, [], [], 0}, Value),
			Pointer = plain_list_pointer(ExtraDataLength + (tuple_size(PointerSeg) - (N + 1)), 6, ListLength),
			NewPointerSeg = insert(N, PointerSeg, Pointer),
			{DataSeg, NewPointerSeg, ListLength + DataLength, [Pointers, Data]};
		{list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,struct},#'capnp::namespace::Type::::struct'{typeId=LTypeId}}}}} ->
			case struct_bit_size(LTypeId, Schema) of
				{composite, DWords, PWords} ->
					FoldFun = fun (V, {L, I, Data, DataLength}) ->
							{DWords1, PWords1, NewData, _NewDataLength, NewExtraData, NewExtraDataLength} = to_bytes(Schema, LTypeId, V, L+DataLength),
							io:format("~p~n", [{NewExtraData, NewExtraDataLength}]),
							{DWords, PWords} = {DWords1, PWords1},
							{L + DWords1 + PWords1, I+1, [NewData,Data|NewExtraData], NewExtraDataLength + DataLength}
					end,
					{DataLength, ListLength, Data, NewExtraDataLength} = lists:foldr(FoldFun, {0, 0, [], 0}, Value),
					io:format("~p~n", [{DataLength, ListLength, Data, NewExtraDataLength}]),
					{Pointer, Header} = composite_list_pointer(ExtraDataLength + (tuple_size(PointerSeg) - (N + 1)), DWords, PWords, ListLength),
					NewPointerSeg = insert(N, PointerSeg, Pointer),
					{DataSeg, NewPointerSeg, DataLength + NewExtraDataLength + 1, [<<Header:?UInt64>>|Data]};
				{pointer, PTypeClass, PTypeDescription, PDefaultValue} ->
					% Pointer only. Actually can encode this exactly like composite in theory; we just don't need a header.
					% Only difference between this and list-of-list case is the types and the unpacking of the struct, which must be {TypeName, Value} as it's only one pointer.
					FoldFun = fun ({_, V}, {I, Pointers, Data, DataLength}) ->
							{{}, {Pointer}, NewDataLength, NewData} = encode_field(PTypeClass, PTypeDescription, PDefaultValue, _N=0, V, _DataSeg={}, _PointerSeg={0}, DataLength+I, Schema),
							{I+1, [<<Pointer:?UInt64>>|Pointers], [Data|NewData], NewDataLength + DataLength}
					end,
					{ListLength, Pointers, Data, DataLength} = lists:foldr(FoldFun, {0, [], [], 0}, Value),
					Pointer = plain_list_pointer(ExtraDataLength + (tuple_size(PointerSeg) - (N + 1)), 6, ListLength),
					NewPointerSeg = insert(N, PointerSeg, Pointer),
					{DataSeg, NewPointerSeg, ListLength + DataLength, [Pointers, Data]};
				{SizeTag, BitSize, Fields} ->
					BurnData = fun (Rec) ->
							Values = tl(tuple_to_list(Rec)),
							{{Data}, {}, _, 0} = encode_parts(Fields, Values, {0}, {}, 0, [], Schema),
							<<Data:BitSize/little-integer>>
					end,
					Length = length(Value),
					PadLength = -(Length * BitSize) band 63,
					Pad = <<0:PadLength>>,
					ListData = [lists:map(BurnData, Value)|Pad],
					ListWords = (Length * BitSize + PadLength) bsr 6,
					Pointer = plain_list_pointer(ExtraDataLength + (tuple_size(PointerSeg) - (N + 1)), SizeTag, Length),
					NewPointerSeg = insert(N, PointerSeg, Pointer),
					{DataSeg, NewPointerSeg, ListWords, ListData}
			end;
		{list, #'capnp::namespace::Type'{''={{_,anyPointer},void}}} ->
			% TODO Encode as pointers?!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,interface},_LTypeId}}} ->
			% TODO Encode as ???!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,enum},_LTypeId}}} ->
			% TODO Encode as ints!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,IntOrVoidType},void}}} ->
			% TODO Encode as ints!
			erlang:error({not_implemented, list, IntOrVoidType})
			% TODO nested constructs (ie. groups)
			% TODO unions (discriminantValue/discriminantOffset)
	end.

encode_text(text, T) when is_binary(T) ->
	{[T, 0], byte_size(T) + 1};
encode_text(data, T) when is_binary(T) ->
	{T, byte_size(T)}.

struct_bit_size(TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords,
				fields=Fields,
				discriminantCount=DCount,
				discriminantOffset=DOffset
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	if
		DWords + PWords > 1 ->
			{composite, DWords, PWords};
		PWords =:= 1 ->
			[#'capnp::namespace::Field'{
					''={{0,slot},
						#'capnp::namespace::Field::::slot'{
							offset=0,
							defaultValue=#'capnp::namespace::Value'{''={{_,TypeClass},DefaultValue}},
							type=#'capnp::namespace::Type'{''={{_,TypeClass},TypeDescription}} % No pointers, so should be fine here!
						}
					}
				}] = Fields,
			{pointer, TypeClass, TypeDescription, DefaultValue};
		true ->
			% Hard part; there are no pointers but we don't know the data length!
			% We simply need the highest extent of any struct part.
			GetExtent = fun (#'capnp::namespace::Field'{
						''={{0,slot},
							#'capnp::namespace::Field::::slot'{
								offset=N,
								defaultValue=#'capnp::namespace::Value'{''={{_,TypeClass},_}},
								type=#'capnp::namespace::Type'{''={{_,TypeClass},void}} % No pointers, so should be fine here!
							}
						}
					}) -> (1 bsl isize(TypeClass)) * (N + 1) end,
			DiscExtent = if DCount > 0 -> 16 + DOffset; true -> 0 end,
			BitExtent = lists:max([DiscExtent|lists:map(GetExtent, Fields)]),
			if
				BitExtent =:= 0 ->
					{0, 0, []};
				BitExtent =:= 1 ->
					{1, 1, Fields};
				BitExtent =< 8 ->
					{2, 8, Fields};
				BitExtent =< 16 ->
					{3, 16, Fields};
				BitExtent =< 32 ->
					{4, 32, Fields};
				true -> % Since DWords + PWords =< 1, we can't be >64.
					{5, 64, Fields}
			end
	end.

% S is in "integer generations"; we're going to use it to to work out how much to bsl.
encode(Type, Value, Default, Offset) ->
	ValueToWrite = encode(Type, Value, Default),
	Shifts = isize(Type),
	% Multiply Offset by the value size, and the result with 63.
	% This gives the offset within this word that the value will appear at.
	% Now shift the encoded value by that amount to align it to where we'd
	% hope it gets written.
	{Shifts, ValueToWrite bsl ((Offset bsl Shifts) band 63)}.

% This step is somewhat complicated by erlang's lack of love for little endian data.
% Basically, booleans end up in the wrong part of their byte.
% Still need a lot of experiment to find the fastest way of writing this code. 
encode(void, _, _) ->
	0;
encode(int8, N, V) ->
	encode_integer(1 bsl 7, N, V);
encode(int16, N, V) ->
	encode_integer(1 bsl 15, N, V);
encode(int32, N, V) ->
	encode_integer(1 bsl 31, N, V);
encode(int64, N, V) ->
	encode_integer(1 bsl 63, N, V);
encode(bool, N, V) ->
	encode_uinteger(1, N, V);
encode(uint8, N, V) ->
	encode_uinteger(1 bsl 8, N, V);
encode(uint16, N, V) ->
	encode_uinteger(1 bsl 16, N, V);
encode(uint32, N, V) ->
	encode_uinteger(1 bsl 32, N, V);
encode(uint64, N, V) ->
	encode_uinteger(1 bsl 64, N, V);
encode(_, _, _) ->
	0.

% How many times do we bsl 1 to get the size in bits?
isize(void) ->
	-1;
isize(bool) ->
	0;
isize(uint8) ->
	3;
isize(int8) ->
	3;
isize(uint16) ->
	4;
isize(int16) ->
	4;
isize(uint32) ->
	5;
isize(int32) ->
	5;
isize(uint64) ->
	6;
isize(int64) ->
	6.

encode_integer(Max, Value, Default) when is_integer(Value), Value < 0, Value >= -Max ->
	(Value+Max*2) bxor Default;
encode_integer(Max, Value, Default) when is_integer(Value), Value >= 0, Value < Max ->
	Value bxor Default.

encode_uinteger(Max, Value, Default) when is_integer(Value), Value >= 0, Value < Max ->
	Value bxor Default.

insert(Offset, DataSeg, Value) ->
	% TODO setelement is sad
	setelement(Offset+1, DataSeg, element(Offset+1, DataSeg) bor Value).

flatten_seg(L) ->
	<< <<A:?UInt64>> || A <- tuple_to_list(L) >>.
