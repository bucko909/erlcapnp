%% @author bucko
%% @doc @todo Add description to capnp_writer.


-module(capnp_writer).

-include_lib("capnp.hrl").
-include_lib("capnp_raw.hrl").
-include_lib("capnp_bootstrap.hrl").

-export([
		to_bytes/2
	]).

to_bytes(Rec, Schema) ->
	Name = element(1, Rec),
	TypeId = dict:fetch(Name, Schema#capnp_context.name_to_id),
	{PtrHd, Raw, _Size} = to_bytes(Schema, TypeId, Rec),
	list_to_binary([<<0:?UInt32>>, PtrHd, Raw]).

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
	{Acc, AccSize} = encode_parts(Fields, tl(tuple_to_list(Obj)), DWords * 64, PWords * 64, PWords, [], Schema),
	{<<DWords:?UInt16, PWords:?UInt16>>, Acc, AccSize}.

encode_parts([
			#'capnp::namespace::Field'{
				''={{0,slot},
					#'capnp::namespace::Field::::slot'{
						offset=N,
						defaultValue=#'capnp::namespace::Value'{''={{TypeClass,_},DefaultValue}},
						type=#'capnp::namespace::Type'{''={{TypeClass,_},TypeDescription}}
					}
				}   
			}
		|RestFields], [Value|RestValues], DataSeg, PointerSeg, Offset, AccParts, Schema) ->
	case TypeDescription of
		void ->
			io:format("~p~n", [{ec, Value, type_size(TypeClass), N, DataSeg, encode(TypeClass, Value)}]),
			NewDataSeg = insert((N*type_size(TypeClass)), DataSeg, encode(TypeClass, Value)),
			io:format("~p~n", [{NewDataSeg}]),
			encode_parts(RestFields, RestValues, NewDataSeg, PointerSeg, Offset, AccParts, Schema);
		TypeId when is_integer(TypeId) ->
			{PointerRHS, Data, TotalWords} = to_bytes(Schema, TypeId, Value),
			Pointer = <<((Offset-N) bsr 2 + 0):?UInt32, PointerRHS/binary>>,
			NewPointerSeg = insert(N*64, PointerSeg, Pointer),
			encode_parts(RestFields, RestValues, NewPointerSeg, DataSeg, Offset + TotalWords, [AccParts,Data], Schema)
		% TODO lists
		% TODO nested constructs (ie. groups)
		% TODO unions (discriminantValue/discriminantOffset)
	end;
encode_parts(_, [], DataSeg, PointerSeg, Offset, AccParts, _Schema) ->
	{[flatten_seg(DataSeg), flatten_seg(PointerSeg), AccParts], Offset}.

encode(9, V) ->
	<<V:?UInt64>>.

type_size(9) ->
	64;
type_size(_) ->
	0.

insert(Offset, {LSize, L, R}, BitString) when Offset < LSize ->
	{LSize, insert(Offset, L, BitString), R};
insert(Offset, {LSize, L, R}, BitString) when Offset >= LSize ->
	{LSize, L, insert(Offset - LSize, R, BitString)};
insert(0, N, BitString) when N =:= erlang:bit_size(BitString) ->
	BitString;
insert(Offset, N, BitString) when N - Offset =:= erlang:bit_size(BitString) ->
	{Offset, Offset, BitString};
insert(Offset, N, BitString) when N - Offset > erlang:bit_size(BitString) ->
	BS = erlang:bit_size(BitString),
	{Offset, Offset, {BS, BitString, N - BS - Offset}}.

flatten_seg({_, LHS, RHS}) ->
	[ flatten_seg(LHS), flatten_seg(RHS) ];
flatten_seg(N) when is_integer(N) ->
	<<0:N/unsigned-little-integer>>;
flatten_seg(BitString) when is_bitstring(BitString) ->
	BitString.