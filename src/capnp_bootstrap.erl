-module(capnp_bootstrap).

% Manual implementation of capnp schema to get us a bootstrapped erlang record schema.

% Wrong assumptions:
% - All defaults are empty list or 0.
% - All data is provided (can't load older schemas).

-compile([export_all]).

-include_lib("capnp_raw.hrl").
-include_lib("capnp_bootstrap.hrl").
-include_lib("capnp.hrl").

decode_list(_, null_pointer) ->
	[];
decode_list(F, #list{data=Data}) ->
	lists:map(F, Data).

decode_text(L) ->
	case decode_list(fun id/1, L) of
		[] ->
			[];
		Decoded ->
			list_to_binary(lists:reverse(tl(lists:reverse(Decoded))))
	end.

id(X) -> X.

'decode_capnp::namespace::Node'(#struct{data=Data= <<
		Id:?UInt64,
		DisplayNamePrefixLength:32/unsigned-little-integer,
		UnionTag:16/little-integer,
		_RestData1:2/binary,
		ScopeId:64/unsigned-little-integer,
		_RestData2/binary
	>>, pointers=Pointers=[DisplayName, NestedNodes, Annotations|_RestPointers]}) ->
	#'capnp::namespace::Node'{
		id=Id,
		displayNamePrefixLength=DisplayNamePrefixLength,
		scopeId=ScopeId,
		displayName=decode_text(DisplayName),
		nestedNodes=decode_list(fun 'decode_capnp::namespace::Node::NestedNode'/1, NestedNodes),
		annotations=decode_list(fun id/1, Annotations),
		''='decode_capnp::namespace::Node::'(UnionTag, Data, Pointers)
	}.

'decode_capnp::namespace::Node::'(UnionTag, Data, Pointers) ->
	Decoders = {
		fun 'decode_capnp::namespace::Node::::file'/2,
		fun 'decode_capnp::namespace::Node::::struct'/2,
		fun 'decode_capnp::namespace::Node::::enum'/2,
		fun 'decode_capnp::namespace::Node::::interface'/2,
		fun 'decode_capnp::namespace::Node::::const'/2,
		fun 'decode_capnp::namespace::Node::::annotation'/2
	},
	(element(UnionTag+1, Decoders))(Data, Pointers).

'decode_capnp::namespace::Node::::file'(_, _) ->
	{{0, file}, void}.
'decode_capnp::namespace::Node::::struct'(<<
		_:112/bitstring,
		DataWordCount:16/unsigned-little-integer,
		_:64/bitstring,
		PointerCount:16/unsigned-little-integer,
		PreferredListEncoding:16/unsigned-little-integer,
		IsGroup:1/unsigned-little-integer,
		_:15/bitstring,
		DiscriminantCount:16/unsigned-little-integer,
		DiscriminantOffset:32/unsigned-little-integer,
		_/binary
	>>, [_, _, _, Fields, _]) ->
	{{1, struct}, #'capnp::namespace::Node::::struct'{
			dataWordCount=DataWordCount,
			pointerCount=PointerCount,
			preferredListEncoding=PreferredListEncoding,
			isGroup=IsGroup,
			discriminantCount=DiscriminantCount,
			discriminantOffset=DiscriminantOffset,
			fields=decode_list(fun 'decode_capnp::namespace::Field'/1, Fields)
	}}.
'decode_capnp::namespace::Node::::enum'(_, [_, _, _, Enumerants|_]) ->
	{{2, enum}, #'capnp::namespace::Node::::enum'{
		enumerants=decode_list(fun 'decode_capnp::namespace::Enumerant'/1, Enumerants)
	}}.
'decode_capnp::namespace::Node::::interface'(_, _) ->
	% Not in schema.
	{'decode_capnp::namespace::Node::::interface'}.
'decode_capnp::namespace::Node::::const'(_, [_, _, _, Type, Value]) ->
	#'capnp::namespace::Node::::const'{
		type='decode_capnp::namespace::Type'(Type),
		value='decode_capnp::namespace::Value'(Value)
	}.
'decode_capnp::namespace::Node::::annotation'(_, _) ->
	% Just ignore!
	{'decode_capnp::namespace::Node::::interface'}.

'decode_capnp::namespace::Node::NestedNode'(#struct{data= <<
		Id:?UInt64,
		_/bitstring
	>>, pointers=[Name]}) ->
	#'capnp::namespace::Node::NestedNode'{
		name=decode_text(Name),
		id=Id
	}.

'decode_capnp::namespace::Field'(#struct{data=Data= <<
		CodeOrder:?UInt16,
		DiscriminantValueRaw:?UInt16,
		_:32/bitstring,
		UnionTag1:?UInt16,
		UnionTag2:?UInt16,
		_/binary
	>>, pointers=Pointers=[Name, Annotations|_]}) ->
	#'capnp::namespace::Field'{
		codeOrder=CodeOrder,
		name=decode_text(Name),
		discriminantValue=DiscriminantValueRaw bxor 65535,
		annotations=decode_list(fun id/1, Annotations),
		''='deocde_capnp::namespace::Field::'(UnionTag1, Data, Pointers),
		ordinal='deocde_capnp::namespace::Field::ordinal'(UnionTag2, Data, Pointers)
	}.

'deocde_capnp::namespace::Field::'(UnionTag, Data, Pointers) ->
	Decoders = {
		fun 'deocde_capnp::namespace::Field::::slot'/2,
		fun 'deocde_capnp::namespace::Field::::group'/2
	},
	(element(UnionTag+1, Decoders))(Data, Pointers).

'deocde_capnp::namespace::Field::::slot'(<<
		_:32/bitstring,
		Offset:?UInt32,
		_:64/bitstring,
		HadExplicitDefault:?Bool,
		_/bitstring
	>>, [_, _, Type, DefaultValue]) ->
	{{0, slot}, #'capnp::namespace::Field::::slot'{
		offset=Offset,
		type='decode_capnp::namespace::Type'(Type),
		defaultValue='decode_capnp::namespace::Value'(DefaultValue),
		hadExplicitDefault=HadExplicitDefault
	}}.
'deocde_capnp::namespace::Field::::group'(<<
		_:128/bitstring,
		TypeId:?UInt64,
		_/binary>>, _) ->
	{{1, group}, #'capnp::namespace::Field::::group'{
		typeId=TypeId
	}}.

'deocde_capnp::namespace::Field::ordinal'(UnionTag, Data, Pointers) ->
	Decoders = {
		fun 'deocde_capnp::namespace::Field::ordinal::implicit'/2,
		fun 'deocde_capnp::namespace::Field::ordinal::explicit'/2
	},
	(element(UnionTag+1, Decoders))(Data, Pointers).
'deocde_capnp::namespace::Field::ordinal::implicit'(_, _) ->
	{{0, implicit}, void}.
'deocde_capnp::namespace::Field::ordinal::explicit'(<<_:96/bitstring, Explicit:?UInt16, _/bitstring>>, _) ->
	{{1, explicit}, Explicit}.

'decode_capnp::namespace::Enumerant'(#struct{data= <<
		CodeOrder:?UInt16,
		_/bitstring
	>>, pointers=[Name, Annotations]}) ->
	#'capnp::namespace::Enumerant'{
		name=decode_text(Name),
		codeOrder=CodeOrder,
		annotations=decode_list(fun id/1, Annotations)
	}.

'decode_capnp::namespace::Type'(#struct{data=Data= <<
		UnionTag:?UInt16,
		_/bitstring
	>>, pointers=Pointers}) ->
	#'capnp::namespace::Type'{
		''='decode_capnp::namespace::Type::'(UnionTag, Data, Pointers)
	}.

'decode_capnp::namespace::Type::'(UnionTag, Data, Pointers) ->
	Decoders = {
		fun 'decode_capnp::namespace::Type::void'/2,
		fun 'decode_capnp::namespace::Type::bool'/2,
		fun 'decode_capnp::namespace::Type::int8'/2,
		fun 'decode_capnp::namespace::Type::int16'/2,
		fun 'decode_capnp::namespace::Type::int32'/2,
		fun 'decode_capnp::namespace::Type::int64'/2,
		fun 'decode_capnp::namespace::Type::uint8'/2,
		fun 'decode_capnp::namespace::Type::uint16'/2,
		fun 'decode_capnp::namespace::Type::uint32'/2,
		fun 'decode_capnp::namespace::Type::uint64'/2,
		fun 'decode_capnp::namespace::Type::float32'/2,
		fun 'decode_capnp::namespace::Type::float64'/2,
		fun 'decode_capnp::namespace::Type::text'/2,
		fun 'decode_capnp::namespace::Type::data'/2,
		fun 'decode_capnp::namespace::Type::list'/2,
		fun 'decode_capnp::namespace::Type::enum'/2,
		fun 'decode_capnp::namespace::Type::struct'/2,
		fun 'decode_capnp::namespace::Type::interface'/2,
		fun 'decode_capnp::namespace::Type::anyPointer'/2
	},
	(element(UnionTag+1, Decoders))(Data, Pointers).

'decode_capnp::namespace::Type::void'(_, _) ->
	{{0, void}, void}.
'decode_capnp::namespace::Type::bool'(_, _) ->
	{{1, bool}, void}.
'decode_capnp::namespace::Type::int8'(_, _) ->
	{{2, int8}, void}.
'decode_capnp::namespace::Type::int16'(_, _) ->
	{{3, int16}, void}.
'decode_capnp::namespace::Type::int32'(_, _) ->
	{{4, int32}, void}.
'decode_capnp::namespace::Type::int64'(_, _) ->
	{{5, int64}, void}.
'decode_capnp::namespace::Type::uint8'(_, _) ->
	{{6, uint8}, void}.
'decode_capnp::namespace::Type::uint16'(_, _) ->
	{{7, uint16}, void}.
'decode_capnp::namespace::Type::uint32'(_, _) ->
	{{8, uint32}, void}.
'decode_capnp::namespace::Type::uint64'(_, _) ->
	{{9, uint64}, void}.
'decode_capnp::namespace::Type::float32'(_, _) ->
	{{10, float32}, void}.
'decode_capnp::namespace::Type::float64'(_, _) ->
	{{11, float64}, void}.
'decode_capnp::namespace::Type::text'(_, _) ->
	{{12, text}, void}.
'decode_capnp::namespace::Type::data'(_, _) ->
	{{13, data}, void}.
'decode_capnp::namespace::Type::list'(_, [ElementType]) ->
	{{14, list}, 'decode_capnp::namespace::Type'(ElementType)}.
'decode_capnp::namespace::Type::enum'(<<_:64/bitstring, TypeId:?UInt64, _/bitstring>>, _) ->
	{{15, enum}, TypeId}.
'decode_capnp::namespace::Type::struct'(<<_:64/bitstring, TypeId:?UInt64, _/bitstring>>, _) ->
	{{16, struct}, TypeId}.
'decode_capnp::namespace::Type::interface'(<<_:64/bitstring, TypeId:?UInt64, _/bitstring>>, _) ->
	{{17, interface}, TypeId}.
'decode_capnp::namespace::Type::anyPointer'(_, _) ->
	{{18, anyPointer}, void}.


'decode_capnp::namespace::Value'(#struct{data=Data= <<
		UnionTag:?UInt16,
		_/bitstring
	>>, pointers=Pointers}) ->
	#'capnp::namespace::Value'{
		''='decode_capnp::namespace::Value::'(UnionTag, Data, Pointers)
	}.

'decode_capnp::namespace::Value::'(UnionTag, Data, Pointers) ->
	Decoders = {
		fun 'decode_capnp::namespace::Value::void'/2,
		fun 'decode_capnp::namespace::Value::bool'/2,
		fun 'decode_capnp::namespace::Value::int8'/2,
		fun 'decode_capnp::namespace::Value::int16'/2,
		fun 'decode_capnp::namespace::Value::int32'/2,
		fun 'decode_capnp::namespace::Value::int64'/2,
		fun 'decode_capnp::namespace::Value::uint8'/2,
		fun 'decode_capnp::namespace::Value::uint16'/2,
		fun 'decode_capnp::namespace::Value::uint32'/2,
		fun 'decode_capnp::namespace::Value::uint64'/2,
		fun 'decode_capnp::namespace::Value::float32'/2,
		fun 'decode_capnp::namespace::Value::float64'/2,
		fun 'decode_capnp::namespace::Value::text'/2,
		fun 'decode_capnp::namespace::Value::data'/2,
		fun 'decode_capnp::namespace::Value::list'/2,
		fun 'decode_capnp::namespace::Value::enum'/2,
		fun 'decode_capnp::namespace::Value::struct'/2,
		fun 'decode_capnp::namespace::Value::interface'/2,
		fun 'decode_capnp::namespace::Value::anyPointer'/2
	},
	(element(UnionTag+1, Decoders))(Data, Pointers).

'decode_capnp::namespace::Value::void'(_, _) ->
	{{0, void}, void}.
'decode_capnp::namespace::Value::bool'(<<_:16/bitstring, Value:?Bool, _/bitstring>>, _) ->
	{{1, bool}, Value}.
'decode_capnp::namespace::Value::int8'(<<_:16/bitstring, Value:?Int8, _/bitstring>>, _) ->
	{{2, int8}, Value}.
'decode_capnp::namespace::Value::int16'(<<_:16/bitstring, Value:?Int16, _/bitstring>>, _) ->
	{{3, int16}, Value}.
'decode_capnp::namespace::Value::int32'(<<_:32/bitstring, Value:?Int32, _/bitstring>>, _) ->
	{{4, int32}, Value}.
'decode_capnp::namespace::Value::int64'(<<_:64/bitstring, Value:?Int64, _/bitstring>>, _) ->
	{{5, int64}, Value}.
'decode_capnp::namespace::Value::uint8'(<<_:16/bitstring, Value:?UInt8, _/bitstring>>, _) ->
	{{6, uint8}, Value}.
'decode_capnp::namespace::Value::uint16'(<<_:16/bitstring, Value:?UInt16, _/bitstring>>, _) ->
	{{7, uint16}, Value}.
'decode_capnp::namespace::Value::uint32'(<<_:32/bitstring, Value:?UInt32, _/bitstring>>, _) ->
	{{8, uint32}, Value}.
'decode_capnp::namespace::Value::uint64'(<<_:64/bitstring, Value:?UInt64, _/bitstring>>, _) ->
	{{9, uint64}, Value}.
'decode_capnp::namespace::Value::float32'(<<_:32/bitstring, Value:?Float32, _/bitstring>>, _) ->
	{{10, float32}, Value}.
'decode_capnp::namespace::Value::float64'(<<_:64/bitstring, Value:?Float64, _/bitstring>>, _) ->
	{{11, float64}, Value}.
'decode_capnp::namespace::Value::text'(_, [Pointer]) ->
	{{12, text}, Pointer}.
'decode_capnp::namespace::Value::data'(_, [Pointer]) ->
	{{13, data}, Pointer}.
'decode_capnp::namespace::Value::list'(_, [Pointer]) ->
	{{14, list}, Pointer}.
'decode_capnp::namespace::Value::enum'(<<_:16/bitstring, Value:?UInt16, _/bitstring>>, _) ->
	{{15, enum}, Value}.
'decode_capnp::namespace::Value::struct'(_, [Pointer]) ->
	{{16, struct}, Pointer}.
'decode_capnp::namespace::Value::interface'(_, _) ->
	{{17, interface}, void}.
'decode_capnp::namespace::Value::anyPointer'(_, [Pointer]) ->
	{{18, anyPointer}, Pointer}.


'decode_capnp::namespace::CodeGeneratorRequest'(#struct{pointers=[Nodes, RequestedFiles]}) ->
	#'capnp::namespace::CodeGeneratorRequest'{
		nodes=decode_list(fun 'decode_capnp::namespace::Node'/1, Nodes),
		requestedFiles=decode_list(fun 'decode_capnp::namespace::CodeGeneratorRequest::RequestedFile'/1, RequestedFiles)
	}.

'decode_capnp::namespace::CodeGeneratorRequest::RequestedFile'(S) ->
	S.

make_objdict(#'capnp::namespace::CodeGeneratorRequest'{nodes=Nodes}) ->
	ById = dict:from_list([{Id, Node} || Node=#'capnp::namespace::Node'{id=Id} <- Nodes ]),
	NameToId = dict:from_list([{Name, Id} || #'capnp::namespace::Node'{id=Id, displayName=Name} <- Nodes ]),
	#capnp_context{
		by_id = ById,
		name_to_id = NameToId
	}.

test() ->
	load_raw_schema("/home/bucko/eclipse-workspace/capnp/data/capnp.raw").

load_raw_schema(Filename) ->
	{ok, Data} = file:read_file(Filename),
	Mes = capnp_raw:read_message(Data),
	Raw = capnp_raw:decode_pointer(Mes),
	CGR = 'decode_capnp::namespace::CodeGeneratorRequest'(Raw),
	make_objdict(CGR).