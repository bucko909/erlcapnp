-module(capnp_raw).

-compile([export_all]).

-record(message, {
		segments :: tuple(),
		current_segment :: binary(),
		current_offset :: integer(),
		depth=0
	}).

-record(struct, {
		data :: binary(),
		pointers :: list(term())
	}).

-record(list, {
		data :: list(term())
	}).

read_message(<<SegmentCountM1:32/unsigned-little-integer, Rest/binary>>) ->
	SegmentLengthsLength = (SegmentCountM1+1)*4,
	PaddingLength = case SegmentCountM1 rem 2 == 0 of true -> 0; false -> 4 end,
	<<SegmentLengthsData:SegmentLengthsLength/binary, _Padding:PaddingLength/binary, DataArea/binary>> = Rest,
	SegmentLengths = get_segment_lengths(SegmentCountM1+1, SegmentLengthsData),
	Segments = [FirstSegment|_] = get_segments(SegmentLengths, DataArea),
	#message{
		segments = list_to_tuple(Segments),
		current_segment = FirstSegment,
		current_offset = 0
	}.

get_segment_lengths(0, <<>>) ->
	[];
get_segment_lengths(N, <<SegmentLength:32/unsigned-little-integer, Rest/binary>>) ->
	[SegmentLength*8|get_segment_lengths(N-1, Rest)].

get_segments([], <<>>) ->
	[];
get_segments([Length|RestLengths], RemData) ->
	<<Data:Length/binary, Rest/binary>> = RemData,
	[Data|get_segments(RestLengths, Rest)].

decode_pointer(Message = #message{current_segment=CS, current_offset=Offset}) ->
	OffsetSize = Offset * 8,
	<<_:OffsetSize/binary, X:8/binary, _/binary>> = CS,
	decode_pointer(X, Message#message{});
decode_pointer(_) ->
	excess_depth.

decode_pointers(<<>>, _) ->
	[];
decode_pointers(<<Pointer:8/binary, Rest/binary>>, Message=#message{current_offset=Offset}) ->
	[decode_pointer(Pointer, Message)|decode_pointers(Rest, Message#message{current_offset=Offset+1})].

decode_pointer(<<0:64/little-integer>>, _) ->
	null_pointer;
decode_pointer(P= <<DataOffsetLSB:6/little-integer, 0:2/little-integer, DataOffsetMSB:24/little-integer, DataSize:16/little-integer, PointerSize:16/little-integer>>, Message=#message{current_offset=CurrentOffset}) ->
	DataOffset = DataOffsetLSB + (DataOffsetMSB bsl 6),
	%io:format("Struct pointer: ~p, ~p~n", [P, {CurrentOffset+1+DataOffset, DataSize, PointerSize}]),
	(catch decode_struct(DataSize, PointerSize, Message#message{current_offset=CurrentOffset+1+DataOffset}));
decode_pointer(P= <<DataOffsetLSB:6/little-integer, 1:2/little-integer, DataOffsetMSB:24/little-integer, ListLengthLSB:5/little-integer, ElementSize:3/little-integer, ListLengthMSB:24/little-integer>>, Message=#message{current_offset=CurrentOffset}) ->
	DataOffset = DataOffsetLSB + (DataOffsetMSB bsl 6),
	ListLength = ListLengthLSB + (ListLengthMSB bsl 5),
	%io:format("List pointer: ~p, ~p~n", [P, {{CurrentOffset, DataOffsetLSB, DataOffsetMSB, CurrentOffset+1+DataOffset}, {ListLength, ListLengthLSB, ListLengthMSB}}]),
	(catch decode_list(ElementSize, ListLength, Message#message{current_offset=CurrentOffset+1+DataOffset}));
decode_pointer(P= <<SegmentOffsetLSB:5/little-integer, LandingPadExtra:1/little-integer, 2:2/little-integer, SegmentOffsetMSB:24/little-integer, SegmentNumber:32/little-integer>>, Message) ->
	SegmentOffset = SegmentOffsetLSB + (SegmentOffsetMSB bsl 5),
	%io:format("Far pointer: ~p, ~p~n", [P, {{SegmentOffset, SegmentOffsetLSB, SegmentOffsetMSB}, SegmentNumber, LandingPadExtra}]),
	(catch decode_far_pointer(SegmentOffset, SegmentNumber, LandingPadExtra, Message));
decode_pointer(P= <<0:6/little-integer, 3:2/little-integer, 0:24/little-integer, CapabilityOffset:32/little-integer>>, #message{}) ->
	%io:format("Capability: ~p, ~p~n", [P, CapabilityOffset]),
	{not_implemented_capabilities, CapabilityOffset};
decode_pointer(Junk, _) ->
	%io:format("Junk: ~p~n", [Junk]),
	{junk, Junk}.

decode_struct(DataWords, PointerWords, Message=#message{current_segment=CS, current_offset=Offset}) ->
	OffsetSize = Offset * 8,
	DataSize = DataWords * 8,
	PointerSize = PointerWords * 8,
	<<_:OffsetSize/binary, Data:DataSize/binary, Pointers:PointerSize/binary, _/binary>> = CS,
	%io:format("Data: ~p; Pointers: ~p~n", [Data, Pointers]),
	#struct{
		data=Data,
		pointers=(catch decode_pointers(Pointers, Message#message{current_offset=Offset+DataWords}))
	}.

decode_list(6, Length, Message=#message{current_segment=CS, current_offset=Offset}) ->
	OffsetSize = Offset * 8,
	PointerSize = Length * 8,
	<<_:OffsetSize/binary, Pointers:PointerSize/binary, _/binary>> = CS,
	%io:format("List pointers: ~p~n", [Pointers]),
	#list{
		data=(catch decode_pointers(Pointers, Message#message{current_offset=Offset+1}))
	};
decode_list(7, _WordLength, Message=#message{current_segment=CS, current_offset=Offset}) ->
	OffsetSize = Offset * 8,
	<<_:OffsetSize/binary, CS1/binary>> = CS,
	<<ListLengthLSB:6/little-integer, 0:2/little-integer, ListLengthMSB:24/little-integer, DataWords:16/little-integer, PointerWords:16/little-integer, _/binary>> = CS1,
	ListLength = ListLengthLSB + (ListLengthMSB bsl 6),
	%io:format("Struct list.~n"),
	#list{
		data=[ (catch decode_struct(DataWords, PointerWords, Message#message{current_offset=Offset+1+I*(DataWords+PointerWords)})) || I <- lists:seq(0, ListLength-1) ]
	};
decode_list(Size, Length, #message{current_segment=CS, current_offset=Offset}) ->
	BitSize = element(Size+1, {0, 1, 8, 16, 32, 64}),
	OffsetSize = Offset * 8,
	DataSize = BitSize * Length,
	<<_:OffsetSize/binary, Data:DataSize/bitstring, _/binary>> = CS,
	Elts = (catch decode_list_elements(BitSize, Length, Data)),
	%io:format("Element list: ~p~n", [Elts]),
	#list{data=Elts}.

decode_list_elements(_, 0, <<>>) ->
	[];
decode_list_elements(S, L, Data) ->
	<<Elt:S/little-integer, Rest/binary>> = Data,
	%io:format("decode_list_elements ~p ~p ~p~n", [S, L, Data]),
	[Elt|decode_list_elements(S, L-1, Rest)].

decode_far_pointer(SegmentOffset, SegmentNumber, 0, Message=#message{segments=Segments}) ->
	NewSegment = element(SegmentNumber+1, Segments),
	%io:format("Far pointer ~p (~p)~n", [SegmentOffset, SegmentNumber]),
	(catch decode_pointer(Message#message{current_segment=NewSegment, current_offset=SegmentOffset})).

test() ->
	{ok, Data} = file:read_file("capnp.raw"),
	Mes = read_message(Data),
	decode_pointer(Mes).
