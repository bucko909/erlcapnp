-file("erlcapnp_test1.hrl", 1).

-record(erlcapnp_SimpleShortStruct,{testVar1 :: -128..127,
                                    testVar2 :: -32768..32767}).

-record(erlcapnp_TestBoringInteger,{testVar1 :: 0..18446744073709551615}).

-record(erlcapnp_TestBoringPointer,{testVar1 :: any()}).

-record(erlcapnp_TestCompositeList,{testVar1 :: any(),testVar2 :: any()}).

-record(erlcapnp_TestDefaults,{testVar1 ::
                                   -9223372036854775808..
                                   9223372036854775807,
                               testVar2 :: any(),
                               testVar3 :: any(),
                               testVar4 ::
                                   capnp:capnp_preformat() |
                                   undefined |
                                   [-9223372036854775808..
                                    9223372036854775807]}).

-record(erlcapnp_TestEnum,{testVar1 :: testEnum1 | testEnum2 | testEnum3}).

-record(erlcapnp_TestEnumList,{testVar1 ::
                                   capnp:capnp_preformat() |
                                   undefined |
                                   [testEnum1 | testEnum2 | testEnum3]}).

-record(erlcapnp_TestGroup,{testVar3 :: -2147483648..2147483647,
                            group1 :: any()}).

-record(erlcapnp_TestGroupInUnion,{'' ::
                                       {unionVar1, any()} |
                                       {unionVar2,
                                        -2147483648..2147483647} |
                                       {unionVar3,
                                        -9223372036854775808..
                                        9223372036854775807},
                                   union2 ::
                                       {testVar1,
                                        -2147483648..2147483647} |
                                       {testVar2,
                                        capnp:capnp_preformat() |
                                        undefined |
                                        [-2147483648..2147483647]}}).

-record(erlcapnp_TestGroupInUnion_unionVar1,{testVar1 ::
                                                 -2147483648..2147483647,
                                             testVar2 ::
                                                 -9223372036854775808..
                                                 9223372036854775807}).

-record(erlcapnp_TestGroup_group1,{testVar1 :: -2147483648..2147483647,
                                   testVar2 ::
                                       -9223372036854775808..
                                       9223372036854775807}).

-record(erlcapnp_TestLessBoringPointer,{testVar2 :: -32768..32767,
                                        testVar1 :: any(),
                                        testVar3 :: any()}).

-record(erlcapnp_TestMultipleIntegers,{testVar1 ::
                                           0..18446744073709551615,
                                       testVar2 :: 0..4294967295,
                                       testVar3 :: 0..65535,
                                       testVar4 :: 0..255,
                                       testVar5 :: -128..127,
                                       testVar6 :: -128..127,
                                       testVar7 ::
                                           -9223372036854775808..
                                           9223372036854775807}).

-record(erlcapnp_TestPointerList,{testVar1 :: any()}).

-record(erlcapnp_TestPrimitiveList,{testVar1 ::
                                        capnp:capnp_preformat() |
                                        undefined | [true | false],
                                    testVar2 ::
                                        capnp:capnp_preformat() |
                                        undefined | [-128..127],
                                    testVar3 ::
                                        capnp:capnp_preformat() |
                                        undefined | [-32768..32767],
                                    testVar4 ::
                                        capnp:capnp_preformat() |
                                        undefined |
                                        [-2147483648..2147483647],
                                    testVar5 ::
                                        capnp:capnp_preformat() |
                                        undefined |
                                        [-9223372036854775808..
                                         9223372036854775807]}).

-record(erlcapnp_TestShortList,{testVar1 :: any(),testVar2 :: any()}).

-record(erlcapnp_TestTextList,{testVar1 ::
                                   capnp:capnp_preformat() |
                                   undefined | [undefined | iodata()]}).

-record(erlcapnp_TestTextType,{testVar1 :: undefined | iodata(),
                               testVar2 :: undefined | iodata()}).

-record(erlcapnp_TestUnion,{testVar1 :: -2147483648..2147483647,
                            '' ::
                                {union1, -2147483648..2147483647} |
                                {union2,
                                 -9223372036854775808..
                                 9223372036854775807} |
                                {union3,
                                 capnp:capnp_preformat() |
                                 undefined | [-128..127]} |
                                {union4, undefined}}).

-record(message_ref,{current_offset,current_segment,segments}).



