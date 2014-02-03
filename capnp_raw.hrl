-define(Bool, 1/little-unsigned-integer).
-define(Int8, 8/little-signed-integer).
-define(Int16, 16/little-signed-integer).
-define(Int32, 32/little-signed-integer).
-define(Int64, 64/little-signed-integer).
-define(UInt8, 8/little-unsigned-integer).
-define(UInt16, 16/little-unsigned-integer).
-define(UInt32, 32/little-unsigned-integer).
-define(UInt64, 64/little-unsigned-integer).
-define(Float32, 32/float).
-define(Float64, 64/float).
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

