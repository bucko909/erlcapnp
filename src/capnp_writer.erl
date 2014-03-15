%% @author bucko
%% @doc @todo Add description to capnp_writer.


-module(capnp_writer).

-include_lib("capnp.hrl").
-include_lib("capnp_raw.hrl").
-include_lib("capnp_bootstrap.hrl").

-export([
		envelope/1,
		to_bytes/2
	]).

envelope(Bytes) ->
	% 32-bit int, num segs - 1
	% segs * 32 bit int, word-size of segs
	% Then the data!
	% TODO assumption: That we have less than 8 * 2^32 bytes of data and only one segment.
	<<0:?UInt32, (round(erlang:byte_size(Bytes)/8)):?UInt32, Bytes/binary>>.

% Convert a record into a segment, starting with the pointer to the binary.
to_bytes(Rec, Schema) ->
	Name = element(1, Rec),
	TypeId = dict:fetch(Name, Schema#capnp_context.name_to_id),
	{DWords, PWords, Raw, _Size} = to_bytes(Schema, TypeId, Rec),
	list_to_binary([<<(struct_pointer(0, DWords, PWords)):?UInt64>>, Raw]).

% OffsetAfterHere is num words /after/ the pointer that the data segment starts. It will therefore often be zero.
struct_pointer(OffsetAfterHere, DWords, PWords) ->
	(OffsetAfterHere bsl 2) bor (DWords bsl 32) bor (PWords bsl 48).

% Convert a record into a byte stream. Each sub-structure will be placed immediately after this one in left-to-right order.
% Returns the data/pointer words in our structure, an unflattened io_list of the encoded structure and the total encoded length (in words).
to_bytes(Schema, TypeId, Obj) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords,
				fields=Fields
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	{Acc, AccSize} = encode_parts(Fields, tl(tuple_to_list(Obj)), list_to_tuple(lists:duplicate(DWords, 0)), list_to_tuple(lists:duplicate(PWords, 0)), PWords, [], Schema),
	{DWords, PWords, Acc, AccSize+DWords}.

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
		|RestFields], [Value|RestValues], DataSeg, PointerSeg, Offset, AccParts, Schema) ->
	case TypeDescription of
		void ->
			{Shifts, Encoded} = encode(TypeClass, Value, DefaultValue, N),
			NewDataSeg = insert((N bsl (Shifts - 6)), DataSeg, Encoded),
			encode_parts(RestFields, RestValues, NewDataSeg, PointerSeg, Offset, AccParts, Schema);
		TypeId when is_integer(TypeId) ->
			{DWords, PWords, Data, TotalWords} = to_bytes(Schema, TypeId, Value),
			Pointer = struct_pointer(Offset-N-1, DWords, PWords),
			NewPointerSeg = insert(N, PointerSeg, Pointer),
			encode_parts(RestFields, RestValues, DataSeg, NewPointerSeg, Offset + TotalWords, [AccParts,Data], Schema)
		% TODO lists
		% TODO nested constructs (ie. groups)
		% TODO unions (discriminantValue/discriminantOffset)
	end;
encode_parts(_, [], DataSeg, PointerSeg, Offset, AccParts, _Schema) ->
	io:format("encode_parts() end ~p~n", [{DataSeg, PointerSeg}]),
	{[flatten_seg(DataSeg), flatten_seg(PointerSeg), AccParts], Offset}.

% S is in "integer generations"; we're going to use it to to work out how much to bsl.
encode(Type, Value, Default, Offset) ->
	{Shifts, ValueToWrite} = encode(Type, Value, Default),
	% Multiply Offset by the value size, and the result with 63.
	% This gives the offset within this word that the value will appear at.
	% Now shift the encoded value by that amount to align it to where we'd
	% hope it gets written.
	{Shifts, ValueToWrite bsl ((Offset bsl Shifts) band 63)}.

% This step is somewhat complicated by erlang's lack of love for little endian data.
% Basically, booleans end up in the wrong part of their byte.
% Still need a lot of experiment to find the fastest way of writing this code. 
encode(void, _, _) ->
	{0, 0};
encode(int8, N, V) ->
	encode_integer(1 bsl 7, 3, N, V);
encode(int16, N, V) ->
	encode_integer(1 bsl 15, 4, N, V);
encode(int32, N, V) ->
	encode_integer(1 bsl 31, 5, N, V);
encode(int64, N, V) ->
	encode_integer(1 bsl 63, 6, N, V);
encode(bool, N, V) ->
	encode_uinteger(1, 0, N, V);
encode(uint8, N, V) ->
	encode_uinteger(1 bsl 8, 3, N, V);
encode(uint16, N, V) ->
	encode_uinteger(1 bsl 16, 4, N, V);
encode(uint32, N, V) ->
	encode_uinteger(1 bsl 32, 5, N, V);
encode(uint64, N, V) ->
	encode_uinteger(1 bsl 64, 6, N, V);
encode(_, _, _) ->
	{0, 0}.

encode_integer(Max, Size, Value, Default) when is_integer(Value), Value < 0, Value >= -Max ->
	{Size, (Value+Max*2) bxor Default};
encode_integer(Max, Size, Value, Default) when is_integer(Value), Value >= 0, Value < Max ->
	{Size, Value bxor Default}.

encode_uinteger(Max, Size, Value, Default) when is_integer(Value), Value >= 0, Value < Max ->
	{Size, Value bxor Default}.

insert(Offset, DataSeg, Value) ->
	% TODO setelement is sad
	setelement(Offset+1, DataSeg, element(Offset+1, DataSeg) bor Value).

flatten_seg(L) ->
	<< <<A:?UInt64>> || A <- tuple_to_list(L) >>.