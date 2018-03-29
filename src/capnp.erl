-module(capnp).

-export([
		split_message/1
]).

-export_type([
		capnp_preformat/0
]).

-include_lib("capnp.hrl").

split_message(Input = <<SegmentsMinusOne:32/little-unsigned-integer, SegmentsAndDataAndRest/binary>>) ->
	% Segments is padded to 64 bits.
	SegmentsLength = (SegmentsMinusOne + 1) * 4,
	PaddingLength = (SegmentsMinusOne rem 2) * 4,
	case SegmentsAndDataAndRest of
		<<SegmentData:SegmentsLength/binary, _Padding:PaddingLength/binary, DataAndRest/binary>> ->
			DataLength = lists:sum([SegmentWords * 8 || <<SegmentWords:32/little-unsigned-integer>> <= SegmentData]),
			RemainingLength = byte_size(DataAndRest) - DataLength,
			case RemainingLength >= 0 of
				true ->
					% We let the compiler know no copying is needed by re-using Input here.
					TotalLength = 4 + SegmentsLength + PaddingLength + DataLength,
					<<ThisMessage:TotalLength/binary, Rest/binary>> = Input,
					{ok, ThisMessage, Rest};
				false ->
					{incomplete, missing_data, -RemainingLength}
			end;
		_TooShort ->
			{incomplete, missing_segment_lengths, SegmentsLength + PaddingLength - byte_size(SegmentsAndDataAndRest)}
	end;
split_message(Input) when is_binary(Input) ->
	{incomplete, missing_segment_count, 4 - byte_size(Input)}.
