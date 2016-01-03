# erlcapnp

capnproto library in erlang.

Provides ability to generate a .hrl and .erl file for your code, which, with as little code between them and the binary as possible, will let you turn records of the shape #CapnpStruct{field=Value} into valid unpacked capnp messages, and (in future) vice versa. As the only operation on a message right now is to decode it, it does not and cannot support RPC.

The development of this library is largely driven by the needs of my employer, who only needs the encode part, so expect slower development on anything unrelated to this. Use ecapnp if you want something more feature-complete.

Preliminary indications are that the encode speed is more than 10 times as fast as ecapnp (tested 2015-12 on the AddressBook example).

## Types

| capnp | Erlang |
|---|---|
| struct A {} | -record('A', {}). |
| struct B { a @0 :UInt64; } | -record('B', { a :: integer() }). |
| enum EnumType { a @0; b @1; } | -type 'EnumType'() :: a \| b. |
| struct C { a @0 :EnumType; } | -record('C', { a :: 'EnumType'() }). |
| struct D { a @0 :Text; } | -record('D', { a :: binary() }). |
| struct E { a @0 :Data; } | -record('E', { a :: binary() }). |
| struct F { a @0 :A; } | -record('F', { a :: #'A'{} }). |
| struct G { a :group { b @0 :B; c @1 :C; } } | -record('G.a', { a :: #'A'{}, b :: #'B'{} }).<br/>-record('G', { a :: #'G.a'{} }). |
| struct H { union { a @0 :A; b @1 :B; } } | -type 'H'() :: { a, #'A'{} } \| { b, #'B'{} } }. |
| struct I { a @0 :UInt64; union { a @0 :A; b @1 :B; } } | -record('I', { a :: integer(), '' :: H() }). |
| struct J { a @0 :UInt64; b :union { a @0 :A; b @1 :B; } } | -record('J', { a :: integer(), b :: H() }). |

Note that pure-anonymous unions are collapsed into their owning scope, as are single-field groups. This behaviour is still in flux, but will be needed if the code is to work safely over hot reloads when data for an old version, generated for an older schema, is in use. (Introduction of a group wrapping a single integer cannot be identified from the schema ordinals.)

## TODO

Very much WIP!

* Namespacing (by prefixing type names?).
* A prettier compile interface (so that capnpc can be used).
* Decoding of some message types (list(text/data)/far pointers).
* Decoding of structs of unexpected lengths. Currently we will do some quite wrong things here, so please don't canonicalise a message and expect decoding to work. :)
* Defaults. Right now defaults on struct and float valued fields are not supported. (Float defaults other than 0.0 are messy; struct is probably impossible to implement at all if the default is recursive, hence scary.)

## NIF branch

Extremely experimental; uses NIFs to encode exactly one sample message type.

Initial experiments on speed aren't super-encouraging (I do not expect much more than a 2x speedup, if any; it seems like allocating resources is fairly expensive). However, a NIF implementation will be able to support RPC.
