-file("erlcapnp_test1.erl", 1).

-module(erlcapnp_test1).

-include_lib("include/erlcapnp_test1.hrl").

-compile([export_all]).

decode_envelope(<<RawSegCount:32/little-unsigned-integer,Rest/binary>>) ->
    SegLengthLength = RawSegCount + 1 bsr 1 bsl 1 + 1 bsl 2,
    <<SegLengthData:SegLengthLength/binary,SegData/binary>> = Rest,
    SegLengths =
        [ 
         X bsl 3 ||
             <<X:32/little-unsigned-integer>> <= SegLengthData,
             X > 0
        ],
    {SegsR,Dregs} =
        lists:foldl(fun(Length, {SplitSegs,Data}) ->
                           <<Seg:Length/binary,Remain/binary>> = Data,
                           {[Seg|SplitSegs],Remain}
                    end,
                    {[],SegData},
                    SegLengths),
    Segs = lists:reverse(SegsR),
    <<Ptr:64/little-unsigned-integer,_/binary>> = hd(Segs),
    {#message_ref{current_offset = 0,
                  current_segment = hd(Segs),
                  segments = list_to_tuple(Segs)},
     Ptr,
     Dregs}.

decode_erlcapnp_SimpleShortStruct(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_SimpleShortStruct/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestBoringInteger(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestBoringInteger/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestBoringPointer(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestBoringPointer/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestCompositeList(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestCompositeList/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestDefaults(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestDefaults/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestEnum(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestEnum/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestEnumList(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestEnumList/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestGroup(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestGroup/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestGroupInUnion(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestGroupInUnion/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_erlcapnp_TestGroupInUnion.'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_erlcapnp_TestGroupInUnion.'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestGroupInUnion_union2(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestGroupInUnion_union2/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestGroupInUnion_unionVar1(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestGroupInUnion_unionVar1/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestGroup_group1(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestGroup_group1/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestLessBoringPointer(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestLessBoringPointer/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestMultipleIntegers(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestMultipleIntegers/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestPointerList(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestPointerList/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestPrimitiveList(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestPrimitiveList/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestShortList(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestShortList/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestTextList(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestTextList/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestTextType(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestTextType/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_erlcapnp_TestUnion(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_erlcapnp_TestUnion/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_erlcapnp_TestUnion.'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_erlcapnp_TestUnion.'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_far_pointer(PointerInt,
                   MessageRef = #message_ref{segments = Segments})
    when PointerInt band 3 == 2 ->
    PointerOffset = (PointerInt bsr 3) band (1 bsl 29 - 1),
    SkipBits = PointerOffset bsl 6,
    Segment = element(PointerInt bsr 32 + 1, Segments),
    <<_:SkipBits,LandingPadInt:64/little-unsigned-integer,_/bitstring>> =
        Segment,
    case PointerInt band 4 of
        0 ->
            NewPointerInt = LandingPadInt,
            NewMessageRef =
                MessageRef#message_ref{current_segment = Segment,
                                       current_offset = PointerOffset};
        1 ->
            2 = LandingPadInt band 7,
            SecondPointerOffset =
                (LandingPadInt bsr 3) band (1 bsl 29 - 1),
            SecondSegment = element(LandingPadInt bsr 32 + 1, Segments),
            <<_:SkipBits,
              0:64,
              NewPointerInt:64/little-unsigned-integer,
              _/bitstring>> =
                Segment,
            0 = (NewPointerInt bsr 2) band (1 bsl 30 - 1),
            NewMessageRef =
                MessageRef#message_ref{current_segment = SecondSegment,
                                       current_offset =
                                           SecondPointerOffset - 1}
    end,
    {NewPointerInt,NewMessageRef}.

decode_struct_list(DecodeFun, Length, DWords, PWords, MessageRef) ->
    Offset = MessageRef#message_ref.current_offset,
    SkipBits = Offset * 64,
    <<_:SkipBits,Rest/binary>> = MessageRef#message_ref.current_segment,
    Words = DWords + PWords,
    DBits = DWords * 64,
    PBits = PWords * 64,
    {_,ListR} =
        lists:foldl(fun(N, {OldRest,Acc}) ->
                           <<ThisData:DBits/bitstring,
                             ThisPointers:PBits/bitstring,
                             NewRest/binary>> =
                               OldRest,
                           New =
                               DecodeFun(ThisData,
                                         ThisPointers,
                                         MessageRef#message_ref{current_offset =
                                                                    Offset
                                                                    +
                                                                    DWords
                                                                    +
                                                                    Words
                                                                    *
                                                                    N}),
                           {NewRest,[New|Acc]}
                    end,
                    {Rest,[]},
                    lists:seq(0, Length - 1)),
    lists:reverse(ListR).

encode_erlcapnp_SimpleShortStruct(#erlcapnp_SimpleShortStruct{testVar1 =
                                                                  VartestVar1,
                                                              testVar2 =
                                                                  VartestVar2},
                                  PtrOffsetWordsFromEnd0) ->
    {4294967296,
     1,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<VartestVar1:8/little-signed-integer,
       0:8/integer,
       VartestVar2:16/little-signed-integer,
       0:32/integer>>,
     []};
encode_erlcapnp_SimpleShortStruct(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_SimpleShortStruct({ZeroOffsetPtrInt,
                                   MainLen,
                                   ExtraLen,
                                   MainData,
                                   ExtraData},
                                  0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestBoringInteger(#erlcapnp_TestBoringInteger{testVar1 =
                                                                  VartestVar1},
                                  PtrOffsetWordsFromEnd0) ->
    {4294967296,
     1,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<VartestVar1:64/little-unsigned-integer>>,
     []};
encode_erlcapnp_TestBoringInteger(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestBoringInteger({ZeroOffsetPtrInt,
                                   MainLen,
                                   ExtraLen,
                                   MainData,
                                   ExtraData},
                                  0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestBoringPointer(#erlcapnp_TestBoringPointer{testVar1 =
                                                                  VartestVar1},
                                  PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrInttestVar1,
     MainLentestVar1,
     ExtraLentestVar1,
     Data1,
     Extra1} =
        encode_erlcapnp_TestBoringInteger(VartestVar1, 0),
    PtrtestVar1 =
        case ZeroOffsetPtrInttestVar1 of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 0 bsl 2
                +
                ZeroOffsetPtrInttestVar1
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLentestVar1 + ExtraLentestVar1,
    {281474976710656,
     1,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<PtrtestVar1:64/little-unsigned-integer>>,
     [Data1,Extra1]};
encode_erlcapnp_TestBoringPointer(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestBoringPointer({ZeroOffsetPtrInt,
                                   MainLen,
                                   ExtraLen,
                                   MainData,
                                   ExtraData},
                                  0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestCompositeList(#erlcapnp_TestCompositeList{testVar1 =
                                                                  VartestVar1,
                                                              testVar2 =
                                                                  VartestVar2},
                                  PtrOffsetWordsFromEnd0) ->
    case VartestVar1 of
        _ when is_list(VartestVar1) ->
            DataLentestVar1 = length(VartestVar1),
            {FinalOffsettestVar1,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {17179869184,
                                    4,
                                    ExtraLentestVar1,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_erlcapnp_TestMultipleIntegers(Element,
                                                                            Offset
                                                                            -
                                                                            4),
                                   {ExtraLentestVar1 + Offset - 4,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLentestVar1 * 4,
                             [<<(DataLentestVar1 bsl 2 + 17179869184):64/unsigned-little-integer>>],
                             []},
                            VartestVar1),
            FinalOffsettestVar1 = round(iolist_size(Extra1) / 8),
            PtrtestVar1 =
                1 bor (1 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLentestVar1 * 4 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLentestVar1 * 4
                +
                FinalOffsettestVar1;
        {0,0,0,_,_} ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0;
        {PointerAsInt1testVar1,
         MainLentestVar1,
         ExtraLentestVar1,
         Data1,
         Extra1} ->
            PtrtestVar1 =
                PointerAsInt1testVar1
                bor
                (1 + PtrOffsetWordsFromEnd0 bsl 2),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + MainLentestVar1
                +
                ExtraLentestVar1;
        undefined ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    case VartestVar2 of
        _ when is_list(VartestVar2) ->
            DataLentestVar2 = length(VartestVar2),
            {FinalOffsettestVar2,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {562954248388608,
                                    3,
                                    ExtraLentestVar2,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_erlcapnp_TestLessBoringPointer(Element,
                                                                             Offset
                                                                             -
                                                                             3),
                                   {ExtraLentestVar2 + Offset - 3,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLentestVar2 * 3,
                             [<<(DataLentestVar2 bsl 2 + 562954248388608):64/unsigned-little-integer>>],
                             []},
                            VartestVar2),
            FinalOffsettestVar2 = round(iolist_size(Extra2) / 8),
            PtrtestVar2 =
                1 bor (0 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLentestVar2 * 3 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLentestVar2 * 3
                +
                FinalOffsettestVar2;
        {0,0,0,_,_} ->
            Extra2 = <<>>,
            Data2 = [],
            PtrtestVar2 = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1;
        {PointerAsInt1testVar2,
         MainLentestVar2,
         ExtraLentestVar2,
         Data2,
         Extra2} ->
            PtrtestVar2 =
                PointerAsInt1testVar2
                bor
                (0 + PtrOffsetWordsFromEnd1 bsl 2),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + MainLentestVar2
                +
                ExtraLentestVar2;
        undefined ->
            Extra2 = <<>>,
            Data2 = [],
            PtrtestVar2 = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    {562949953421312,
     2,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<PtrtestVar1:64/little-unsigned-integer,
       PtrtestVar2:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
encode_erlcapnp_TestCompositeList(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestCompositeList({ZeroOffsetPtrInt,
                                   MainLen,
                                   ExtraLen,
                                   MainData,
                                   ExtraData},
                                  0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestDefaults(#erlcapnp_TestDefaults{testVar1 =
                                                        VartestVar1,
                                                    testVar2 =
                                                        VartestVar2,
                                                    testVar3 =
                                                        VartestVar3,
                                                    testVar4 =
                                                        VartestVar4},
                             PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrInttestVar2,
     MainLentestVar2,
     ExtraLentestVar2,
     Data1,
     Extra1} =
        encode_erlcapnp_TestBoringInteger(VartestVar2, 0),
    PtrtestVar2 =
        case ZeroOffsetPtrInttestVar2 of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 2 bsl 2
                +
                ZeroOffsetPtrInttestVar2
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLentestVar2 + ExtraLentestVar2,
    {ZeroOffsetPtrInttestVar3,
     MainLentestVar3,
     ExtraLentestVar3,
     Data2,
     Extra2} =
        encode_erlcapnp_TestLessBoringPointer(VartestVar3, 0),
    PtrtestVar3 =
        case ZeroOffsetPtrInttestVar3 of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd1 + 1 bsl 2
                +
                ZeroOffsetPtrInttestVar3
        end,
    PtrOffsetWordsFromEnd2 =
        PtrOffsetWordsFromEnd1 + MainLentestVar3 + ExtraLentestVar3,
    case VartestVar4 of
        _ when is_list(VartestVar4) ->
            Extra3 = <<>>,
            DataLentestVar4 = length(VartestVar4),
            Data3 =
                [<< 
                   <<X:64/little-signed-integer>> ||
                       X <- VartestVar4
                 >>,
                 <<0:(- DataLentestVar4 * 64 band 63)/unsigned-little-integer>>],
            PtrtestVar4 =
                1 bor (PtrOffsetWordsFromEnd2 + 0 bsl 2) bor (5 bsl 32)
                bor
                (DataLentestVar4 bsl 35),
            PtrOffsetWordsFromEnd3 =
                PtrOffsetWordsFromEnd2
                +
                (DataLentestVar4 * 64 + 63 bsr 6);
        {0,0,0,_,_} ->
            Extra3 = <<>>,
            Data3 = [],
            PtrtestVar4 = 0,
            PtrOffsetWordsFromEnd3 = PtrOffsetWordsFromEnd2;
        {PointerAsInt1testVar4,
         MainLentestVar4,
         ExtraLentestVar4,
         Data3,
         Extra3} ->
            PtrtestVar4 =
                PointerAsInt1testVar4
                bor
                (0 + PtrOffsetWordsFromEnd2 bsl 2),
            PtrOffsetWordsFromEnd3 =
                PtrOffsetWordsFromEnd2 + MainLentestVar4
                +
                ExtraLentestVar4;
        undefined ->
            Extra3 = <<>>,
            Data3 = [],
            PtrtestVar4 = 0,
            PtrOffsetWordsFromEnd3 = PtrOffsetWordsFromEnd2
    end,
    {844429225099264,
     4,
     PtrOffsetWordsFromEnd3 - PtrOffsetWordsFromEnd0,
     <<(VartestVar1 bxor 53):64/little-signed-integer,
       PtrtestVar2:64/little-unsigned-integer,
       PtrtestVar3:64/little-unsigned-integer,
       PtrtestVar4:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2,Data3,Extra3]};
encode_erlcapnp_TestDefaults(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestDefaults({ZeroOffsetPtrInt,
                              MainLen,
                              ExtraLen,
                              MainData,
                              ExtraData},
                             0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestEnum(#erlcapnp_TestEnum{testVar1 = VartestVar1},
                         PtrOffsetWordsFromEnd0) ->
    {4294967296,
     1,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<case VartestVar1 of
           testEnum1 ->
               0;
           testEnum2 ->
               1;
           testEnum3 ->
               2
       end:16/little-unsigned-integer,
       0:48/integer>>,
     []};
encode_erlcapnp_TestEnum(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestEnum({ZeroOffsetPtrInt,
                          MainLen,
                          ExtraLen,
                          MainData,
                          ExtraData},
                         0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestEnumList(#erlcapnp_TestEnumList{testVar1 =
                                                        VartestVar1},
                             PtrOffsetWordsFromEnd0) ->
    case VartestVar1 of
        _ when is_list(VartestVar1) ->
            Extra1 = <<>>,
            DataLentestVar1 = length(VartestVar1),
            Data1 =
                [<< 
                   <<case X of
                         testEnum1 ->
                             0;
                         testEnum2 ->
                             1;
                         testEnum3 ->
                             2
                     end:16/little-unsigned-integer>> ||
                       X <- VartestVar1
                 >>,
                 <<0:(- DataLentestVar1 * 16 band 63)/unsigned-little-integer>>],
            PtrtestVar1 =
                1 bor (PtrOffsetWordsFromEnd0 + 0 bsl 2) bor (3 bsl 32)
                bor
                (DataLentestVar1 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0
                +
                (DataLentestVar1 * 16 + 63 bsr 6);
        {0,0,0,_,_} ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0;
        {PointerAsInt1testVar1,
         MainLentestVar1,
         ExtraLentestVar1,
         Data1,
         Extra1} ->
            PtrtestVar1 =
                PointerAsInt1testVar1
                bor
                (0 + PtrOffsetWordsFromEnd0 bsl 2),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + MainLentestVar1
                +
                ExtraLentestVar1;
        undefined ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {281474976710656,
     1,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<PtrtestVar1:64/little-unsigned-integer>>,
     [Data1,Extra1]};
encode_erlcapnp_TestEnumList(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestEnumList({ZeroOffsetPtrInt,
                              MainLen,
                              ExtraLen,
                              MainData,
                              ExtraData},
                             0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestGroup(#erlcapnp_TestGroup{testVar3 = VartestVar3,
                                              group1 =
                                                  #erlcapnp_TestGroup_group1{testVar1 =
                                                                                 Vargroup1testVar1,
                                                                             testVar2 =
                                                                                 Vargroup1testVar2}},
                          PtrOffsetWordsFromEnd0) ->
    {8589934592,
     2,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<Vargroup1testVar1:32/little-signed-integer,
       VartestVar3:32/little-signed-integer,
       Vargroup1testVar2:64/little-signed-integer>>,
     []};
encode_erlcapnp_TestGroup(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestGroup({ZeroOffsetPtrInt,
                           MainLen,
                           ExtraLen,
                           MainData,
                           ExtraData},
                          0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestGroupInUnion(#erlcapnp_TestGroupInUnion{'' = Var,
                                                            union2 =
                                                                Varunion2},
                                 PtrOffsetWordsFromEnd0) ->
    <<NoGroupBodyDataAsInt:256/integer>> =
        <<0:192/integer,0:64/integer>>,
    {_ZeroOffsetPtrInt,_NewBodyLen,ExtraDataLen,BodyData,ExtraData} =
        'encode_erlcapnp_TestGroupInUnion.'(Var,
                                            PtrOffsetWordsFromEnd0
                                            -
                                            PtrOffsetWordsFromEnd0),
    <<BodyDataAsIntFrom:256/integer>> = BodyData,
    {_ZeroOffsetPtrIntunion2,
     _NewBodyLenunion2,
     ExtraDataLenunion2,
     BodyDataunion2,
     ExtraDataunion2} =
        encode_erlcapnp_TestGroupInUnion_union2(Varunion2,
                                                PtrOffsetWordsFromEnd0
                                                -
                                                PtrOffsetWordsFromEnd0
                                                +
                                                ExtraDataLen),
    <<BodyDataAsIntFromunion2:256/integer>> = BodyDataunion2,
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0 + ExtraDataLen
     +
     ExtraDataLenunion2,
     <<(NoGroupBodyDataAsInt bor BodyDataAsIntFrom
        bor
        BodyDataAsIntFromunion2):256/integer>>,
     [[[]|ExtraData]|ExtraDataunion2]};
encode_erlcapnp_TestGroupInUnion(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestGroupInUnion({ZeroOffsetPtrInt,
                                  MainLen,
                                  ExtraLen,
                                  MainData,
                                  ExtraData},
                                 0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

'encode_erlcapnp_TestGroupInUnion.'({VarDiscriminant,Var},
                                    PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        unionVar1 ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:256/little-unsigned-integer>>,
             ExtraData} =
                encode_erlcapnp_TestGroupInUnion_unionVar1(Var,
                                                           PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (0 bsl 32)):256/little-unsigned-integer>>,
             ExtraData};
        unionVar2 ->
            {281487861612544,
             4,
             0,
             <<Var:32/little-signed-integer,
               1:16/little-unsigned-integer,
               0:144/integer,
               0:64/integer>>,
             []};
        unionVar3 ->
            {281487861612544,
             4,
             0,
             <<0:32/integer,
               2:16/little-unsigned-integer,
               0:16/integer,
               Var:64/little-signed-integer,
               0:64/integer,
               0:64/integer>>,
             []}
    end.

encode_erlcapnp_TestGroupInUnion_union2({VarDiscriminant,Var},
                                        PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        testVar1 ->
            {281487861612544,
             4,
             0,
             <<0:48/integer,
               0:16/little-unsigned-integer,
               0:64/integer,
               Var:32/little-signed-integer,
               0:32/integer,
               0:64/integer>>,
             []};
        testVar2 ->
            case Var of
                _ when is_list(Var) ->
                    Extra1 = <<>>,
                    DataLen = length(Var),
                    Data1 =
                        [<< 
                           <<X:32/little-signed-integer>> ||
                               X <- Var
                         >>,
                         <<0:(- DataLen * 32 band 63)/unsigned-little-integer>>],
                    Ptr =
                        1 bor (PtrOffsetWordsFromEnd0 + 0 bsl 2)
                        bor
                        (4 bsl 32)
                        bor
                        (DataLen bsl 35),
                    PtrOffsetWordsFromEnd1 =
                        PtrOffsetWordsFromEnd0
                        +
                        (DataLen * 32 + 63 bsr 6);
                {0,0,0,_,_} ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0;
                {PointerAsInt1,MainLen,ExtraLen,Data1,Extra1} ->
                    Ptr =
                        PointerAsInt1
                        bor
                        (0 + PtrOffsetWordsFromEnd0 bsl 2),
                    PtrOffsetWordsFromEnd1 =
                        PtrOffsetWordsFromEnd0 + MainLen + ExtraLen;
                undefined ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {281487861612544,
             4,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<0:48/integer,
               1:16/little-unsigned-integer,
               0:128/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]}
    end.

encode_erlcapnp_TestGroupInUnion_unionVar1(#erlcapnp_TestGroupInUnion_unionVar1{testVar1 =
                                                                                    VartestVar1,
                                                                                testVar2 =
                                                                                    VartestVar2},
                                           PtrOffsetWordsFromEnd0) ->
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<VartestVar1:32/little-signed-integer,
       0:32/integer,
       VartestVar2:64/little-signed-integer,
       0:64/integer,
       0:64/integer>>,
     []};
encode_erlcapnp_TestGroupInUnion_unionVar1(undefined,
                                           _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestGroupInUnion_unionVar1({ZeroOffsetPtrInt,
                                            MainLen,
                                            ExtraLen,
                                            MainData,
                                            ExtraData},
                                           0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestGroup_group1(#erlcapnp_TestGroup_group1{testVar1 =
                                                                VartestVar1,
                                                            testVar2 =
                                                                VartestVar2},
                                 PtrOffsetWordsFromEnd0) ->
    {8589934592,
     2,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<VartestVar1:32/little-signed-integer,
       0:32/integer,
       VartestVar2:64/little-signed-integer>>,
     []};
encode_erlcapnp_TestGroup_group1(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestGroup_group1({ZeroOffsetPtrInt,
                                  MainLen,
                                  ExtraLen,
                                  MainData,
                                  ExtraData},
                                 0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestLessBoringPointer(#erlcapnp_TestLessBoringPointer{testVar2 =
                                                                          VartestVar2,
                                                                      testVar1 =
                                                                          VartestVar1,
                                                                      testVar3 =
                                                                          VartestVar3},
                                      PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrInttestVar1,
     MainLentestVar1,
     ExtraLentestVar1,
     Data1,
     Extra1} =
        encode_erlcapnp_TestBoringPointer(VartestVar1, 0),
    PtrtestVar1 =
        case ZeroOffsetPtrInttestVar1 of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 1 bsl 2
                +
                ZeroOffsetPtrInttestVar1
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLentestVar1 + ExtraLentestVar1,
    {ZeroOffsetPtrInttestVar3,
     MainLentestVar3,
     ExtraLentestVar3,
     Data2,
     Extra2} =
        encode_erlcapnp_TestMultipleIntegers(VartestVar3, 0),
    PtrtestVar3 =
        case ZeroOffsetPtrInttestVar3 of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd1 + 0 bsl 2
                +
                ZeroOffsetPtrInttestVar3
        end,
    PtrOffsetWordsFromEnd2 =
        PtrOffsetWordsFromEnd1 + MainLentestVar3 + ExtraLentestVar3,
    {562954248388608,
     3,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<VartestVar2:16/little-signed-integer,
       0:48/integer,
       PtrtestVar1:64/little-unsigned-integer,
       PtrtestVar3:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
encode_erlcapnp_TestLessBoringPointer(undefined,
                                      _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestLessBoringPointer({ZeroOffsetPtrInt,
                                       MainLen,
                                       ExtraLen,
                                       MainData,
                                       ExtraData},
                                      0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestMultipleIntegers(#erlcapnp_TestMultipleIntegers{testVar1 =
                                                                        VartestVar1,
                                                                    testVar2 =
                                                                        VartestVar2,
                                                                    testVar3 =
                                                                        VartestVar3,
                                                                    testVar4 =
                                                                        VartestVar4,
                                                                    testVar5 =
                                                                        VartestVar5,
                                                                    testVar6 =
                                                                        VartestVar6,
                                                                    testVar7 =
                                                                        VartestVar7},
                                     PtrOffsetWordsFromEnd0) ->
    {17179869184,
     4,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<VartestVar1:64/little-unsigned-integer,
       VartestVar2:32/little-unsigned-integer,
       VartestVar3:16/little-unsigned-integer,
       VartestVar4:8/little-unsigned-integer,
       VartestVar5:8/little-signed-integer,
       VartestVar6:8/little-signed-integer,
       0:56/integer,
       VartestVar7:64/little-signed-integer>>,
     []};
encode_erlcapnp_TestMultipleIntegers(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestMultipleIntegers({ZeroOffsetPtrInt,
                                      MainLen,
                                      ExtraLen,
                                      MainData,
                                      ExtraData},
                                     0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestPointerList(#erlcapnp_TestPointerList{testVar1 =
                                                              VartestVar1},
                                PtrOffsetWordsFromEnd0) ->
    case VartestVar1 of
        _ when is_list(VartestVar1) ->
            DataLentestVar1 = length(VartestVar1),
            {FinalOffsettestVar1,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {281474976710656,
                                    1,
                                    ExtraLentestVar1,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_erlcapnp_TestBoringPointer(Element,
                                                                         Offset
                                                                         -
                                                                         1),
                                   {ExtraLentestVar1 + Offset - 1,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLentestVar1 * 1,
                             [<<(DataLentestVar1 bsl 2 + 281474976710656):64/unsigned-little-integer>>],
                             []},
                            VartestVar1),
            FinalOffsettestVar1 = round(iolist_size(Extra1) / 8),
            PtrtestVar1 =
                1 bor (0 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLentestVar1 * 1 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLentestVar1 * 1
                +
                FinalOffsettestVar1;
        {0,0,0,_,_} ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0;
        {PointerAsInt1testVar1,
         MainLentestVar1,
         ExtraLentestVar1,
         Data1,
         Extra1} ->
            PtrtestVar1 =
                PointerAsInt1testVar1
                bor
                (0 + PtrOffsetWordsFromEnd0 bsl 2),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + MainLentestVar1
                +
                ExtraLentestVar1;
        undefined ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {281474976710656,
     1,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<PtrtestVar1:64/little-unsigned-integer>>,
     [Data1,Extra1]};
encode_erlcapnp_TestPointerList(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestPointerList({ZeroOffsetPtrInt,
                                 MainLen,
                                 ExtraLen,
                                 MainData,
                                 ExtraData},
                                0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestPrimitiveList(#erlcapnp_TestPrimitiveList{testVar1 =
                                                                  VartestVar1,
                                                              testVar2 =
                                                                  VartestVar2,
                                                              testVar3 =
                                                                  VartestVar3,
                                                              testVar4 =
                                                                  VartestVar4,
                                                              testVar5 =
                                                                  VartestVar5},
                                  PtrOffsetWordsFromEnd0) ->
    case VartestVar1 of
        _ when is_list(VartestVar1) ->
            Extra1 = <<>>,
            DataLentestVar1 = length(VartestVar1),
            DataFixedtestVar1 =
                massage_bool_list([ 
                                   if
                                       Y =:= true ->
                                           1;
                                       true ->
                                           0
                                   end ||
                                       Y <- VartestVar1
                                  ]),
            Data1 =
                <<<< 
                    <<X:1>> ||
                        X <- DataFixedtestVar1
                  >>/bitstring,
                  0:(- length(DataFixedtestVar1) band 63)/unsigned-little-integer>>,
            PtrtestVar1 =
                1 bor (PtrOffsetWordsFromEnd0 + 4 bsl 2) bor (1 bsl 32)
                bor
                (DataLentestVar1 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLentestVar1 + 63 bsr 6);
        {0,0,0,_,_} ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0;
        {PointerAsInt1testVar1,
         MainLentestVar1,
         ExtraLentestVar1,
         Data1,
         Extra1} ->
            PtrtestVar1 =
                PointerAsInt1testVar1
                bor
                (4 + PtrOffsetWordsFromEnd0 bsl 2),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + MainLentestVar1
                +
                ExtraLentestVar1;
        undefined ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    case VartestVar2 of
        _ when is_list(VartestVar2) ->
            Extra2 = <<>>,
            DataLentestVar2 = length(VartestVar2),
            Data2 =
                [<< 
                   <<X:8/little-signed-integer>> ||
                       X <- VartestVar2
                 >>,
                 <<0:(- DataLentestVar2 * 8 band 63)/unsigned-little-integer>>],
            PtrtestVar2 =
                1 bor (PtrOffsetWordsFromEnd1 + 3 bsl 2) bor (2 bsl 32)
                bor
                (DataLentestVar2 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1
                +
                (DataLentestVar2 * 8 + 63 bsr 6);
        {0,0,0,_,_} ->
            Extra2 = <<>>,
            Data2 = [],
            PtrtestVar2 = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1;
        {PointerAsInt1testVar2,
         MainLentestVar2,
         ExtraLentestVar2,
         Data2,
         Extra2} ->
            PtrtestVar2 =
                PointerAsInt1testVar2
                bor
                (3 + PtrOffsetWordsFromEnd1 bsl 2),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + MainLentestVar2
                +
                ExtraLentestVar2;
        undefined ->
            Extra2 = <<>>,
            Data2 = [],
            PtrtestVar2 = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    case VartestVar3 of
        _ when is_list(VartestVar3) ->
            Extra3 = <<>>,
            DataLentestVar3 = length(VartestVar3),
            Data3 =
                [<< 
                   <<X:16/little-signed-integer>> ||
                       X <- VartestVar3
                 >>,
                 <<0:(- DataLentestVar3 * 16 band 63)/unsigned-little-integer>>],
            PtrtestVar3 =
                1 bor (PtrOffsetWordsFromEnd2 + 2 bsl 2) bor (3 bsl 32)
                bor
                (DataLentestVar3 bsl 35),
            PtrOffsetWordsFromEnd3 =
                PtrOffsetWordsFromEnd2
                +
                (DataLentestVar3 * 16 + 63 bsr 6);
        {0,0,0,_,_} ->
            Extra3 = <<>>,
            Data3 = [],
            PtrtestVar3 = 0,
            PtrOffsetWordsFromEnd3 = PtrOffsetWordsFromEnd2;
        {PointerAsInt1testVar3,
         MainLentestVar3,
         ExtraLentestVar3,
         Data3,
         Extra3} ->
            PtrtestVar3 =
                PointerAsInt1testVar3
                bor
                (2 + PtrOffsetWordsFromEnd2 bsl 2),
            PtrOffsetWordsFromEnd3 =
                PtrOffsetWordsFromEnd2 + MainLentestVar3
                +
                ExtraLentestVar3;
        undefined ->
            Extra3 = <<>>,
            Data3 = [],
            PtrtestVar3 = 0,
            PtrOffsetWordsFromEnd3 = PtrOffsetWordsFromEnd2
    end,
    case VartestVar4 of
        _ when is_list(VartestVar4) ->
            Extra4 = <<>>,
            DataLentestVar4 = length(VartestVar4),
            Data4 =
                [<< 
                   <<X:32/little-signed-integer>> ||
                       X <- VartestVar4
                 >>,
                 <<0:(- DataLentestVar4 * 32 band 63)/unsigned-little-integer>>],
            PtrtestVar4 =
                1 bor (PtrOffsetWordsFromEnd3 + 1 bsl 2) bor (4 bsl 32)
                bor
                (DataLentestVar4 bsl 35),
            PtrOffsetWordsFromEnd4 =
                PtrOffsetWordsFromEnd3
                +
                (DataLentestVar4 * 32 + 63 bsr 6);
        {0,0,0,_,_} ->
            Extra4 = <<>>,
            Data4 = [],
            PtrtestVar4 = 0,
            PtrOffsetWordsFromEnd4 = PtrOffsetWordsFromEnd3;
        {PointerAsInt1testVar4,
         MainLentestVar4,
         ExtraLentestVar4,
         Data4,
         Extra4} ->
            PtrtestVar4 =
                PointerAsInt1testVar4
                bor
                (1 + PtrOffsetWordsFromEnd3 bsl 2),
            PtrOffsetWordsFromEnd4 =
                PtrOffsetWordsFromEnd3 + MainLentestVar4
                +
                ExtraLentestVar4;
        undefined ->
            Extra4 = <<>>,
            Data4 = [],
            PtrtestVar4 = 0,
            PtrOffsetWordsFromEnd4 = PtrOffsetWordsFromEnd3
    end,
    case VartestVar5 of
        _ when is_list(VartestVar5) ->
            Extra5 = <<>>,
            DataLentestVar5 = length(VartestVar5),
            Data5 =
                [<< 
                   <<X:64/little-signed-integer>> ||
                       X <- VartestVar5
                 >>,
                 <<0:(- DataLentestVar5 * 64 band 63)/unsigned-little-integer>>],
            PtrtestVar5 =
                1 bor (PtrOffsetWordsFromEnd4 + 0 bsl 2) bor (5 bsl 32)
                bor
                (DataLentestVar5 bsl 35),
            PtrOffsetWordsFromEnd5 =
                PtrOffsetWordsFromEnd4
                +
                (DataLentestVar5 * 64 + 63 bsr 6);
        {0,0,0,_,_} ->
            Extra5 = <<>>,
            Data5 = [],
            PtrtestVar5 = 0,
            PtrOffsetWordsFromEnd5 = PtrOffsetWordsFromEnd4;
        {PointerAsInt1testVar5,
         MainLentestVar5,
         ExtraLentestVar5,
         Data5,
         Extra5} ->
            PtrtestVar5 =
                PointerAsInt1testVar5
                bor
                (0 + PtrOffsetWordsFromEnd4 bsl 2),
            PtrOffsetWordsFromEnd5 =
                PtrOffsetWordsFromEnd4 + MainLentestVar5
                +
                ExtraLentestVar5;
        undefined ->
            Extra5 = <<>>,
            Data5 = [],
            PtrtestVar5 = 0,
            PtrOffsetWordsFromEnd5 = PtrOffsetWordsFromEnd4
    end,
    {1407374883553280,
     5,
     PtrOffsetWordsFromEnd5 - PtrOffsetWordsFromEnd0,
     <<PtrtestVar1:64/little-unsigned-integer,
       PtrtestVar2:64/little-unsigned-integer,
       PtrtestVar3:64/little-unsigned-integer,
       PtrtestVar4:64/little-unsigned-integer,
       PtrtestVar5:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2,Data3,Extra3,Data4,Extra4,Data5,Extra5]};
encode_erlcapnp_TestPrimitiveList(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestPrimitiveList({ZeroOffsetPtrInt,
                                   MainLen,
                                   ExtraLen,
                                   MainData,
                                   ExtraData},
                                  0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestShortList(#erlcapnp_TestShortList{testVar1 =
                                                          VartestVar1,
                                                      testVar2 =
                                                          VartestVar2},
                              PtrOffsetWordsFromEnd0) ->
    case VartestVar1 of
        _ when is_list(VartestVar1) ->
            DataLentestVar1 = length(VartestVar1),
            {FinalOffsettestVar1,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {4294967296,
                                    1,
                                    ExtraLentestVar1,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_erlcapnp_TestBoringInteger(Element,
                                                                         Offset
                                                                         -
                                                                         1),
                                   {ExtraLentestVar1 + Offset - 1,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLentestVar1 * 1,
                             [<<(DataLentestVar1 bsl 2 + 4294967296):64/unsigned-little-integer>>],
                             []},
                            VartestVar1),
            FinalOffsettestVar1 = round(iolist_size(Extra1) / 8),
            PtrtestVar1 =
                1 bor (1 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLentestVar1 * 1 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLentestVar1 * 1
                +
                FinalOffsettestVar1;
        {0,0,0,_,_} ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0;
        {PointerAsInt1testVar1,
         MainLentestVar1,
         ExtraLentestVar1,
         Data1,
         Extra1} ->
            PtrtestVar1 =
                PointerAsInt1testVar1
                bor
                (1 + PtrOffsetWordsFromEnd0 bsl 2),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + MainLentestVar1
                +
                ExtraLentestVar1;
        undefined ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    case VartestVar2 of
        _ when is_list(VartestVar2) ->
            DataLentestVar2 = length(VartestVar2),
            {FinalOffsettestVar2,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {4294967296,
                                    1,
                                    ExtraLentestVar2,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_erlcapnp_SimpleShortStruct(Element,
                                                                         Offset
                                                                         -
                                                                         1),
                                   {ExtraLentestVar2 + Offset - 1,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLentestVar2 * 1,
                             [<<(DataLentestVar2 bsl 2 + 4294967296):64/unsigned-little-integer>>],
                             []},
                            VartestVar2),
            FinalOffsettestVar2 = round(iolist_size(Extra2) / 8),
            PtrtestVar2 =
                1 bor (0 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLentestVar2 * 1 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLentestVar2 * 1
                +
                FinalOffsettestVar2;
        {0,0,0,_,_} ->
            Extra2 = <<>>,
            Data2 = [],
            PtrtestVar2 = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1;
        {PointerAsInt1testVar2,
         MainLentestVar2,
         ExtraLentestVar2,
         Data2,
         Extra2} ->
            PtrtestVar2 =
                PointerAsInt1testVar2
                bor
                (0 + PtrOffsetWordsFromEnd1 bsl 2),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + MainLentestVar2
                +
                ExtraLentestVar2;
        undefined ->
            Extra2 = <<>>,
            Data2 = [],
            PtrtestVar2 = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    {562949953421312,
     2,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<PtrtestVar1:64/little-unsigned-integer,
       PtrtestVar2:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
encode_erlcapnp_TestShortList(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestShortList({ZeroOffsetPtrInt,
                               MainLen,
                               ExtraLen,
                               MainData,
                               ExtraData},
                              0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestTextList(#erlcapnp_TestTextList{testVar1 =
                                                        VartestVar1},
                             PtrOffsetWordsFromEnd0) ->
    case VartestVar1 of
        _ when is_list(VartestVar1) ->
            DataLentestVar1 = length(VartestVar1),
            {FinalOffsettestVar1,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {281474976710656,
                                    1,
                                    ExtraLentestVar1,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_text(Element, Offset - 1),
                                   {ExtraLentestVar1 + Offset - 1,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLentestVar1 * 1,
                             [<<(DataLentestVar1 bsl 2 + 281474976710656):64/unsigned-little-integer>>],
                             []},
                            VartestVar1),
            FinalOffsettestVar1 = round(iolist_size(Extra1) / 8),
            PtrtestVar1 =
                1 bor (0 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLentestVar1 * 1 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLentestVar1 * 1
                +
                FinalOffsettestVar1;
        {0,0,0,_,_} ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0;
        {PointerAsInt1testVar1,
         MainLentestVar1,
         ExtraLentestVar1,
         Data1,
         Extra1} ->
            PtrtestVar1 =
                PointerAsInt1testVar1
                bor
                (0 + PtrOffsetWordsFromEnd0 bsl 2),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + MainLentestVar1
                +
                ExtraLentestVar1;
        undefined ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {281474976710656,
     1,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<PtrtestVar1:64/little-unsigned-integer>>,
     [Data1,Extra1]};
encode_erlcapnp_TestTextList(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestTextList({ZeroOffsetPtrInt,
                              MainLen,
                              ExtraLen,
                              MainData,
                              ExtraData},
                             0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestTextType(#erlcapnp_TestTextType{testVar1 =
                                                        VartestVar1,
                                                    testVar2 =
                                                        VartestVar2},
                             PtrOffsetWordsFromEnd0) ->
    if
        is_list(VartestVar1);is_binary(VartestVar1) ->
            Extra1 = <<>>,
            DataLentestVar1 = iolist_size(VartestVar1) + 1,
            Data1 =
                [VartestVar1,
                 <<0:8,
                   0:(- DataLentestVar1 band 7 * 8)/unsigned-little-integer>>],
            PtrtestVar1 =
                1 bor (PtrOffsetWordsFromEnd0 + 1 bsl 2) bor (2 bsl 32)
                bor
                (DataLentestVar1 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLentestVar1 + 7 bsr 3);
        VartestVar1 =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            PtrtestVar1 = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    if
        is_list(VartestVar2);is_binary(VartestVar2) ->
            Extra2 = <<>>,
            DataLentestVar2 = iolist_size(VartestVar2),
            Data2 =
                [VartestVar2,
                 <<0:(- DataLentestVar2 band 7 * 8)/unsigned-little-integer>>],
            PtrtestVar2 =
                1 bor (PtrOffsetWordsFromEnd1 + 0 bsl 2) bor (2 bsl 32)
                bor
                (DataLentestVar2 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + (DataLentestVar2 + 7 bsr 3);
        VartestVar2 =:= undefined ->
            Extra2 = <<>>,
            Data2 = [],
            PtrtestVar2 = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    {562949953421312,
     2,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<PtrtestVar1:64/little-unsigned-integer,
       PtrtestVar2:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
encode_erlcapnp_TestTextType(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestTextType({ZeroOffsetPtrInt,
                              MainLen,
                              ExtraLen,
                              MainData,
                              ExtraData},
                             0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

encode_erlcapnp_TestUnion(#erlcapnp_TestUnion{testVar1 = VartestVar1,
                                              '' = Var},
                          PtrOffsetWordsFromEnd0) ->
    <<NoGroupBodyDataAsInt:256/integer>> =
        <<VartestVar1:32/little-signed-integer,
          0:160/integer,
          0:64/integer>>,
    {_ZeroOffsetPtrInt,_NewBodyLen,ExtraDataLen,BodyData,ExtraData} =
        'encode_erlcapnp_TestUnion.'(Var,
                                     PtrOffsetWordsFromEnd0
                                     -
                                     PtrOffsetWordsFromEnd0),
    <<BodyDataAsIntFrom:256/integer>> = BodyData,
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0 + ExtraDataLen,
     <<(NoGroupBodyDataAsInt bor BodyDataAsIntFrom):256/integer>>,
     [[]|ExtraData]};
encode_erlcapnp_TestUnion(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]};
encode_erlcapnp_TestUnion({ZeroOffsetPtrInt,
                           MainLen,
                           ExtraLen,
                           MainData,
                           ExtraData},
                          0)
    when
        is_integer(ZeroOffsetPtrInt),
        is_integer(MainLen),
        is_integer(ExtraLen) ->
    {ZeroOffsetPtrInt,MainLen,ExtraLen,MainData,ExtraData}.

'encode_erlcapnp_TestUnion.'({VarDiscriminant,Var},
                             PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        union1 ->
            {281487861612544,
             4,
             0,
             <<0:32/integer,
               Var:32/little-signed-integer,
               0:16/little-unsigned-integer,
               0:112/integer,
               0:64/integer>>,
             []};
        union2 ->
            {281487861612544,
             4,
             0,
             <<0:64/integer,
               1:16/little-unsigned-integer,
               0:48/integer,
               Var:64/little-signed-integer,
               0:64/integer>>,
             []};
        union3 ->
            case Var of
                _ when is_list(Var) ->
                    Extra1 = <<>>,
                    DataLen = length(Var),
                    Data1 =
                        [<< 
                           <<X:8/little-signed-integer>> ||
                               X <- Var
                         >>,
                         <<0:(- DataLen * 8 band 63)/unsigned-little-integer>>],
                    Ptr =
                        1 bor (PtrOffsetWordsFromEnd0 + 0 bsl 2)
                        bor
                        (2 bsl 32)
                        bor
                        (DataLen bsl 35),
                    PtrOffsetWordsFromEnd1 =
                        PtrOffsetWordsFromEnd0
                        +
                        (DataLen * 8 + 63 bsr 6);
                {0,0,0,_,_} ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0;
                {PointerAsInt1,MainLen,ExtraLen,Data1,Extra1} ->
                    Ptr =
                        PointerAsInt1
                        bor
                        (0 + PtrOffsetWordsFromEnd0 bsl 2),
                    PtrOffsetWordsFromEnd1 =
                        PtrOffsetWordsFromEnd0 + MainLen + ExtraLen;
                undefined ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {281487861612544,
             4,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<0:64/integer,
               2:16/little-unsigned-integer,
               0:112/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]};
        union4 ->
            {281487861612544,
             4,
             0,
             <<case Var of
                   undefined ->
                       0
               end:0/integer,
               0:64/integer,
               3:16/little-unsigned-integer,
               0:112/integer,
               0:64/integer>>,
             []}
    end.

encode_text(undefined, _Offset) ->
    {281474976710656,1,0,[<<0:64>>],[]};
encode_text(List, Offset) ->
    DataLen = iolist_size(List) + 1,
    Data =
        [List,<<0:8,0:(- DataLen band 7 * 8)/unsigned-little-integer>>],
    Ptr = 1 bor (Offset bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
    {281474976710656,
     1,
     DataLen + 7 bsr 3,
     <<Ptr:64/unsigned-little-integer>>,
     Data}.

envelope_erlcapnp_SimpleShortStruct(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_SimpleShortStruct(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestBoringInteger(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestBoringInteger(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestBoringPointer(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestBoringPointer(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestCompositeList(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestCompositeList(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestDefaults(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestDefaults(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestEnum(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestEnum(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestEnumList(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestEnumList(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestGroup(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestGroup(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestGroupInUnion(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestGroupInUnion(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestLessBoringPointer(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestLessBoringPointer(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestMultipleIntegers(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestMultipleIntegers(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestPointerList(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestPointerList(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestPrimitiveList(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestPrimitiveList(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestShortList(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestShortList(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestTextList(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestTextList(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestTextType(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestTextType(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_erlcapnp_TestUnion(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_erlcapnp_TestUnion(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

follow_bool_list_pointer(0, _MessageRef) ->
    undefined;
follow_bool_list_pointer(PointerInt, MessageRef)
    when
        PointerInt band 3 =:= 1
        andalso
        (PointerInt bsr 32) band 7 =:= 1 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    Offset = MessageRef#message_ref.current_offset + PointerOffset,
    SkipBits = Offset * 64,
    Length = PointerInt bsr 35,
    WholeParts = Length + 7 bsr 3 bsl 3,
    <<_:SkipBits,ListData:WholeParts/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    lists:sublist(lists:append([ 
                                lists:reverse([ 
                                               case Bit of
                                                   0 ->
                                                       false;
                                                   1 ->
                                                       true
                                               end ||
                                                   <<Bit:1>> <= Byte
                                              ]) ||
                                    <<Byte:8/bitstring>> <= ListData
                               ]),
                  Length).

follow_data_pointer(PointerInt, MessageRef) ->
    follow_text_or_data_pointer(PointerInt, MessageRef, 0).

follow_int16_list_pointer(0, _MessageRef) ->
    undefined;
follow_int16_list_pointer(PointerInt, MessageRef)
    when
        PointerInt band 3 =:= 1
        andalso
        (PointerInt bsr 32) band 7 =:= 3 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    Offset = MessageRef#message_ref.current_offset + PointerOffset,
    SkipBits = Offset * 64,
    Length = PointerInt bsr 35,
    MessageBits = Length * 16,
    <<_:SkipBits,ListData:MessageBits/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    [ 
     X ||
         <<X:16/little-signed-integer>> <= ListData
    ].

follow_int32_list_pointer(0, _MessageRef) ->
    undefined;
follow_int32_list_pointer(PointerInt, MessageRef)
    when
        PointerInt band 3 =:= 1
        andalso
        (PointerInt bsr 32) band 7 =:= 4 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    Offset = MessageRef#message_ref.current_offset + PointerOffset,
    SkipBits = Offset * 64,
    Length = PointerInt bsr 35,
    MessageBits = Length * 32,
    <<_:SkipBits,ListData:MessageBits/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    [ 
     X ||
         <<X:32/little-signed-integer>> <= ListData
    ].

follow_int64_list_pointer(0, _MessageRef) ->
    undefined;
follow_int64_list_pointer(PointerInt, MessageRef)
    when
        PointerInt band 3 =:= 1
        andalso
        (PointerInt bsr 32) band 7 =:= 5 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    Offset = MessageRef#message_ref.current_offset + PointerOffset,
    SkipBits = Offset * 64,
    Length = PointerInt bsr 35,
    MessageBits = Length * 64,
    <<_:SkipBits,ListData:MessageBits/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    [ 
     X ||
         <<X:64/little-signed-integer>> <= ListData
    ].

follow_int8_list_pointer(0, _MessageRef) ->
    undefined;
follow_int8_list_pointer(PointerInt, MessageRef)
    when
        PointerInt band 3 =:= 1
        andalso
        (PointerInt bsr 32) band 7 =:= 2 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    Offset = MessageRef#message_ref.current_offset + PointerOffset,
    SkipBits = Offset * 64,
    Length = PointerInt bsr 35,
    MessageBits = Length * 8,
    <<_:SkipBits,ListData:MessageBits/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    [ 
     X ||
         <<X:8/little-signed-integer>> <= ListData
    ].

follow_struct_list_pointer(_DecodeFun, 0, _MessageRef) ->
    undefined;
follow_struct_list_pointer(DecodeFun, PointerInt, MessageRef)
    when
        PointerInt band 3 == 1
        andalso
        (PointerInt bsr 32) band 7 >= 5 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    LengthFromPointer = (PointerInt bsr 35) band (1 bsl 29 - 1),
    NewOffset = MessageRef#message_ref.current_offset + PointerOffset,
    case (PointerInt bsr 32) band 7 of
        5 ->
            Length = LengthFromPointer,
            DWords = 1,
            PWords = 0,
            ListStartOffset = NewOffset;
        6 ->
            Length = LengthFromPointer,
            DWords = 0,
            PWords = 1,
            ListStartOffset = NewOffset;
        7 ->
            SkipBits = NewOffset bsl 6,
            <<_:SkipBits,Tag:64/little-unsigned-integer,_/binary>> =
                MessageRef#message_ref.current_segment,
            0 = Tag band 3,
            Length = (Tag bsr 2) band (1 bsl 30 - 1),
            DWords = (Tag bsr 32) band (1 bsl 16 - 1),
            PWords = (Tag bsr 48) band (1 bsl 16 - 1),
            LengthFromPointer = Length * (DWords + PWords),
            ListStartOffset = NewOffset + 1
    end,
    decode_struct_list(DecodeFun,
                       Length,
                       DWords,
                       PWords,
                       MessageRef#message_ref{current_offset =
                                                  ListStartOffset});
follow_struct_list_pointer(_DecodeFun, PointerInt, _MessageRef)
    when
        PointerInt band 3 == 1
        andalso
        (PointerInt bsr 32) band 7 < 5 ->
    error({not_supported,
           "cannot currently decode List(struct {}) which is stored as "
           "a list of subword values"});
follow_struct_list_pointer(DecodeFun,
                           PointerInt,
                           MessageRef = #message_ref{})
    when PointerInt band 3 == 2 ->
    {NewPointerInt,NewMessageRef} =
        decode_far_pointer(PointerInt, MessageRef),
    follow_struct_list_pointer(DecodeFun, NewPointerInt, NewMessageRef).

follow_struct_pointer(_DecodeFun, 0, _MessageRef) ->
    undefined;
follow_struct_pointer(DecodeFun, PointerInt, MessageRef)
    when PointerInt band 3 == 0 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    NewOffset = MessageRef#message_ref.current_offset + PointerOffset,
    DWords = (PointerInt bsr 32) band (1 bsl 16 - 1),
    PWords = (PointerInt bsr 48) band (1 bsl 16 - 1),
    NewMessageRef =
        MessageRef#message_ref{current_offset = NewOffset + DWords},
    SkipBits = NewOffset bsl 6,
    DBits = DWords bsl 6,
    PBits = PWords bsl 6,
    <<_:SkipBits,Data:DBits/bitstring,Pointers:PBits/bitstring,_/binary>> =
        MessageRef#message_ref.current_segment,
    DecodeFun(Data, Pointers, NewMessageRef);
follow_struct_pointer(DecodeFun,
                      PointerInt,
                      MessageRef = #message_ref{})
    when PointerInt band 3 == 2 ->
    {NewPointerInt,NewMessageRef} =
        decode_far_pointer(PointerInt, MessageRef),
    follow_struct_pointer(DecodeFun, NewPointerInt, NewMessageRef).

follow_text_or_data_pointer(0, _MessageRef, _Trail) ->
    undefined;
follow_text_or_data_pointer(PointerInt, MessageRef, Trail)
    when
        PointerInt band 3 =:= 1
        andalso
        (PointerInt bsr 32) band 7 =:= 2 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    Offset = MessageRef#message_ref.current_offset + PointerOffset,
    SkipBits = Offset bsl 6,
    Length = PointerInt bsr 35 - Trail,
    MessageBits = Length bsl 3,
    <<_:SkipBits,ListData:MessageBits/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    ListData;
follow_text_or_data_pointer(PointerInt,
                            MessageRef = #message_ref{},
                            Trail)
    when PointerInt band 3 == 2 ->
    {NewPointerInt,NewMessageRef} =
        decode_far_pointer(PointerInt, MessageRef),
    follow_text_or_data_pointer(NewPointerInt, NewMessageRef, Trail).

follow_text_pointer(PointerInt, MessageRef) ->
    follow_text_or_data_pointer(PointerInt, MessageRef, 1).

follow_undefined_list_pointer(0, _MessageRef) ->
    undefined;
follow_undefined_list_pointer(PointerInt, MessageRef)
    when
        PointerInt band 3 =:= 1
        andalso
        (PointerInt bsr 32) band 7 =:= 3 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    Offset = MessageRef#message_ref.current_offset + PointerOffset,
    SkipBits = Offset * 64,
    Length = PointerInt bsr 35,
    MessageBits = Length * 16,
    <<_:SkipBits,ListData:MessageBits/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    [ 
     element(X + 1, {testEnum1,testEnum2,testEnum3}) ||
         <<X:16/little-unsigned-integer>> <= ListData
    ].

internal_decode_erlcapnp_SimpleShortStruct(<<VartestVar1:8/little-signed-integer,
                                             _:8/integer,
                                             VartestVar2:16/little-signed-integer,
                                             _:32/integer>>,
                                           <<>>,
                                           _MessageRef) ->
    #erlcapnp_SimpleShortStruct{testVar1 = VartestVar1,
                                testVar2 = VartestVar2};
internal_decode_erlcapnp_SimpleShortStruct(Data,
                                           Pointers,
                                           MessageRef = #message_ref{}) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 0 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:0/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_SimpleShortStruct(PaddedData,
                                               PaddedPointers,
                                               MessageRef).

internal_decode_erlcapnp_TestBoringInteger(<<VartestVar1:64/little-unsigned-integer>>,
                                           <<>>,
                                           _MessageRef) ->
    #erlcapnp_TestBoringInteger{testVar1 = VartestVar1};
internal_decode_erlcapnp_TestBoringInteger(Data,
                                           Pointers,
                                           MessageRef = #message_ref{}) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 0 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:0/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestBoringInteger(PaddedData,
                                               PaddedPointers,
                                               MessageRef).

internal_decode_erlcapnp_TestBoringPointer(<<>>,
                                           <<VartestVar1:64/little-unsigned-integer>>,
                                           MessageRef) ->
    #erlcapnp_TestBoringPointer{testVar1 =
                                    follow_struct_pointer(fun internal_decode_erlcapnp_TestBoringInteger/3,
                                                          VartestVar1,
                                                          MessageRef#message_ref{current_offset =
                                                                                     MessageRef#message_ref.current_offset
                                                                                     +
                                                                                     0})};
internal_decode_erlcapnp_TestBoringPointer(Data,
                                           Pointers,
                                           MessageRef = #message_ref{}) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestBoringPointer(PaddedData,
                                               PaddedPointers,
                                               MessageRef).

internal_decode_erlcapnp_TestCompositeList(<<>>,
                                           <<VartestVar1:64/little-unsigned-integer,
                                             VartestVar2:64/little-unsigned-integer>>,
                                           MessageRef) ->
    #erlcapnp_TestCompositeList{testVar1 =
                                    follow_struct_list_pointer(fun internal_decode_erlcapnp_TestMultipleIntegers/3,
                                                               VartestVar1,
                                                               MessageRef#message_ref{current_offset =
                                                                                          MessageRef#message_ref.current_offset
                                                                                          +
                                                                                          0}),
                                testVar2 =
                                    follow_struct_list_pointer(fun internal_decode_erlcapnp_TestLessBoringPointer/3,
                                                               VartestVar2,
                                                               MessageRef#message_ref{current_offset =
                                                                                          MessageRef#message_ref.current_offset
                                                                                          +
                                                                                          1})};
internal_decode_erlcapnp_TestCompositeList(Data,
                                           Pointers,
                                           MessageRef = #message_ref{}) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 128 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:128/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestCompositeList(PaddedData,
                                               PaddedPointers,
                                               MessageRef).

internal_decode_erlcapnp_TestDefaults(<<VartestVar1:64/little-signed-integer>>,
                                      <<VartestVar2:64/little-unsigned-integer,
                                        VartestVar3:64/little-unsigned-integer,
                                        VartestVar4:64/little-unsigned-integer>>,
                                      MessageRef) ->
    #erlcapnp_TestDefaults{testVar1 = VartestVar1 bxor 53,
                           testVar2 =
                               follow_struct_pointer(fun internal_decode_erlcapnp_TestBoringInteger/3,
                                                     VartestVar2,
                                                     MessageRef#message_ref{current_offset =
                                                                                MessageRef#message_ref.current_offset
                                                                                +
                                                                                0}),
                           testVar3 =
                               follow_struct_pointer(fun internal_decode_erlcapnp_TestLessBoringPointer/3,
                                                     VartestVar3,
                                                     MessageRef#message_ref{current_offset =
                                                                                MessageRef#message_ref.current_offset
                                                                                +
                                                                                1}),
                           testVar4 =
                               follow_int64_list_pointer(VartestVar4,
                                                         MessageRef#message_ref{current_offset =
                                                                                    MessageRef#message_ref.current_offset
                                                                                    +
                                                                                    2})};
internal_decode_erlcapnp_TestDefaults(Data,
                                      Pointers,
                                      MessageRef = #message_ref{}) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 192 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:192/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestDefaults(PaddedData,
                                          PaddedPointers,
                                          MessageRef).

internal_decode_erlcapnp_TestEnum(<<VartestVar1:16/little-unsigned-integer,
                                    _:48/integer>>,
                                  <<>>,
                                  _MessageRef) ->
    #erlcapnp_TestEnum{testVar1 =
                           element(VartestVar1 + 1,
                                   {testEnum1,testEnum2,testEnum3})};
internal_decode_erlcapnp_TestEnum(Data,
                                  Pointers,
                                  MessageRef = #message_ref{}) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 0 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:0/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestEnum(PaddedData,
                                      PaddedPointers,
                                      MessageRef).

internal_decode_erlcapnp_TestEnumList(<<>>,
                                      <<VartestVar1:64/little-unsigned-integer>>,
                                      MessageRef) ->
    #erlcapnp_TestEnumList{testVar1 =
                               follow_undefined_list_pointer(VartestVar1,
                                                             MessageRef#message_ref{current_offset =
                                                                                        MessageRef#message_ref.current_offset
                                                                                        +
                                                                                        0})};
internal_decode_erlcapnp_TestEnumList(Data,
                                      Pointers,
                                      MessageRef = #message_ref{}) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestEnumList(PaddedData,
                                          PaddedPointers,
                                          MessageRef).

internal_decode_erlcapnp_TestGroup(Data =
                                       <<_:32/integer,
                                         VartestVar3:32/little-signed-integer,
                                         _:64/integer>>,
                                   Pointers = <<>>,
                                   MessageRef) ->
    #erlcapnp_TestGroup{testVar3 = VartestVar3,
                        group1 =
                            internal_decode_erlcapnp_TestGroup_group1(Data,
                                                                      Pointers,
                                                                      MessageRef)};
internal_decode_erlcapnp_TestGroup(Data,
                                   Pointers,
                                   MessageRef = #message_ref{}) ->
    DataPadLength = 128 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:128/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 0 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:0/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestGroup(PaddedData,
                                       PaddedPointers,
                                       MessageRef).

internal_decode_erlcapnp_TestGroupInUnion(Data = <<_:192/integer>>,
                                          Pointers = <<_:64/integer>>,
                                          MessageRef) ->
    #erlcapnp_TestGroupInUnion{'' =
                                   'internal_decode_erlcapnp_TestGroupInUnion.'(Data,
                                                                                Pointers,
                                                                                MessageRef),
                               union2 =
                                   internal_decode_erlcapnp_TestGroupInUnion_union2(Data,
                                                                                    Pointers,
                                                                                    MessageRef)};
internal_decode_erlcapnp_TestGroupInUnion(Data,
                                          Pointers,
                                          MessageRef = #message_ref{}) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestGroupInUnion(PaddedData,
                                              PaddedPointers,
                                              MessageRef).

'internal_decode_erlcapnp_TestGroupInUnion.'(Data =
                                                 <<_:32,
                                                   Discriminant:16/little-unsigned-integer,
                                                   _:144>>,
                                             Pointers = <<_:64>>,
                                             MessageRef) ->
    case Discriminant of
        0 ->
            {unionVar1,
             internal_decode_erlcapnp_TestGroupInUnion_unionVar1(Data,
                                                                 Pointers,
                                                                 MessageRef)};
        1 ->
            <<_:0,Var:32/little-signed-integer,_/bitstring>> = Data,
            {unionVar2,Var};
        2 ->
            <<_:64,Var:64/little-signed-integer,_/bitstring>> = Data,
            {unionVar3,Var}
    end;
'internal_decode_erlcapnp_TestGroupInUnion.'(Data,
                                             Pointers,
                                             MessageRef = #message_ref{}) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    'internal_decode_erlcapnp_TestGroupInUnion.'(PaddedData,
                                                 PaddedPointers,
                                                 MessageRef).

internal_decode_erlcapnp_TestGroupInUnion_union2(Data =
                                                     <<_:48,
                                                       Discriminant:16/little-unsigned-integer,
                                                       _:128>>,
                                                 Pointers = <<_:64>>,
                                                 MessageRef) ->
    case Discriminant of
        0 ->
            <<_:128,Var:32/little-signed-integer,_/bitstring>> = Data,
            {testVar1,Var};
        1 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {testVar2,
             follow_int32_list_pointer(Var,
                                       MessageRef#message_ref{current_offset =
                                                                  MessageRef#message_ref.current_offset
                                                                  +
                                                                  0})}
    end;
internal_decode_erlcapnp_TestGroupInUnion_union2(Data,
                                                 Pointers,
                                                 MessageRef =
                                                     #message_ref{}) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestGroupInUnion_union2(PaddedData,
                                                     PaddedPointers,
                                                     MessageRef).

internal_decode_erlcapnp_TestGroupInUnion_unionVar1(<<VartestVar1:32/little-signed-integer,
                                                      _:32/integer,
                                                      VartestVar2:64/little-signed-integer,
                                                      _:64/integer>>,
                                                    <<_:64/integer>>,
                                                    _MessageRef) ->
    #erlcapnp_TestGroupInUnion_unionVar1{testVar1 = VartestVar1,
                                         testVar2 = VartestVar2};
internal_decode_erlcapnp_TestGroupInUnion_unionVar1(Data,
                                                    Pointers,
                                                    MessageRef =
                                                        #message_ref{}) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestGroupInUnion_unionVar1(PaddedData,
                                                        PaddedPointers,
                                                        MessageRef).

internal_decode_erlcapnp_TestGroup_group1(<<VartestVar1:32/little-signed-integer,
                                            _:32/integer,
                                            VartestVar2:64/little-signed-integer>>,
                                          <<>>,
                                          _MessageRef) ->
    #erlcapnp_TestGroup_group1{testVar1 = VartestVar1,
                               testVar2 = VartestVar2};
internal_decode_erlcapnp_TestGroup_group1(Data,
                                          Pointers,
                                          MessageRef = #message_ref{}) ->
    DataPadLength = 128 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:128/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 0 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:0/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestGroup_group1(PaddedData,
                                              PaddedPointers,
                                              MessageRef).

internal_decode_erlcapnp_TestLessBoringPointer(<<VartestVar2:16/little-signed-integer,
                                                 _:48/integer>>,
                                               <<VartestVar1:64/little-unsigned-integer,
                                                 VartestVar3:64/little-unsigned-integer>>,
                                               MessageRef) ->
    #erlcapnp_TestLessBoringPointer{testVar2 = VartestVar2,
                                    testVar1 =
                                        follow_struct_pointer(fun internal_decode_erlcapnp_TestBoringPointer/3,
                                                              VartestVar1,
                                                              MessageRef#message_ref{current_offset =
                                                                                         MessageRef#message_ref.current_offset
                                                                                         +
                                                                                         0}),
                                    testVar3 =
                                        follow_struct_pointer(fun internal_decode_erlcapnp_TestMultipleIntegers/3,
                                                              VartestVar3,
                                                              MessageRef#message_ref{current_offset =
                                                                                         MessageRef#message_ref.current_offset
                                                                                         +
                                                                                         1})};
internal_decode_erlcapnp_TestLessBoringPointer(Data,
                                               Pointers,
                                               MessageRef =
                                                   #message_ref{}) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 128 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:128/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestLessBoringPointer(PaddedData,
                                                   PaddedPointers,
                                                   MessageRef).

internal_decode_erlcapnp_TestMultipleIntegers(<<VartestVar1:64/little-unsigned-integer,
                                                VartestVar2:32/little-unsigned-integer,
                                                VartestVar3:16/little-unsigned-integer,
                                                VartestVar4:8/little-unsigned-integer,
                                                VartestVar5:8/little-signed-integer,
                                                VartestVar6:8/little-signed-integer,
                                                _:56/integer,
                                                VartestVar7:64/little-signed-integer>>,
                                              <<>>,
                                              _MessageRef) ->
    #erlcapnp_TestMultipleIntegers{testVar1 = VartestVar1,
                                   testVar2 = VartestVar2,
                                   testVar3 = VartestVar3,
                                   testVar4 = VartestVar4,
                                   testVar5 = VartestVar5,
                                   testVar6 = VartestVar6,
                                   testVar7 = VartestVar7};
internal_decode_erlcapnp_TestMultipleIntegers(Data,
                                              Pointers,
                                              MessageRef =
                                                  #message_ref{}) ->
    DataPadLength = 256 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:256/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 0 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:0/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestMultipleIntegers(PaddedData,
                                                  PaddedPointers,
                                                  MessageRef).

internal_decode_erlcapnp_TestPointerList(<<>>,
                                         <<VartestVar1:64/little-unsigned-integer>>,
                                         MessageRef) ->
    #erlcapnp_TestPointerList{testVar1 =
                                  follow_struct_list_pointer(fun internal_decode_erlcapnp_TestBoringPointer/3,
                                                             VartestVar1,
                                                             MessageRef#message_ref{current_offset =
                                                                                        MessageRef#message_ref.current_offset
                                                                                        +
                                                                                        0})};
internal_decode_erlcapnp_TestPointerList(Data,
                                         Pointers,
                                         MessageRef = #message_ref{}) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestPointerList(PaddedData,
                                             PaddedPointers,
                                             MessageRef).

internal_decode_erlcapnp_TestPrimitiveList(<<>>,
                                           <<VartestVar1:64/little-unsigned-integer,
                                             VartestVar2:64/little-unsigned-integer,
                                             VartestVar3:64/little-unsigned-integer,
                                             VartestVar4:64/little-unsigned-integer,
                                             VartestVar5:64/little-unsigned-integer>>,
                                           MessageRef) ->
    #erlcapnp_TestPrimitiveList{testVar1 =
                                    follow_bool_list_pointer(VartestVar1,
                                                             MessageRef#message_ref{current_offset =
                                                                                        MessageRef#message_ref.current_offset
                                                                                        +
                                                                                        0}),
                                testVar2 =
                                    follow_int8_list_pointer(VartestVar2,
                                                             MessageRef#message_ref{current_offset =
                                                                                        MessageRef#message_ref.current_offset
                                                                                        +
                                                                                        1}),
                                testVar3 =
                                    follow_int16_list_pointer(VartestVar3,
                                                              MessageRef#message_ref{current_offset =
                                                                                         MessageRef#message_ref.current_offset
                                                                                         +
                                                                                         2}),
                                testVar4 =
                                    follow_int32_list_pointer(VartestVar4,
                                                              MessageRef#message_ref{current_offset =
                                                                                         MessageRef#message_ref.current_offset
                                                                                         +
                                                                                         3}),
                                testVar5 =
                                    follow_int64_list_pointer(VartestVar5,
                                                              MessageRef#message_ref{current_offset =
                                                                                         MessageRef#message_ref.current_offset
                                                                                         +
                                                                                         4})};
internal_decode_erlcapnp_TestPrimitiveList(Data,
                                           Pointers,
                                           MessageRef = #message_ref{}) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 320 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:320/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestPrimitiveList(PaddedData,
                                               PaddedPointers,
                                               MessageRef).

internal_decode_erlcapnp_TestShortList(<<>>,
                                       <<VartestVar1:64/little-unsigned-integer,
                                         VartestVar2:64/little-unsigned-integer>>,
                                       MessageRef) ->
    #erlcapnp_TestShortList{testVar1 =
                                follow_struct_list_pointer(fun internal_decode_erlcapnp_TestBoringInteger/3,
                                                           VartestVar1,
                                                           MessageRef#message_ref{current_offset =
                                                                                      MessageRef#message_ref.current_offset
                                                                                      +
                                                                                      0}),
                            testVar2 =
                                follow_struct_list_pointer(fun internal_decode_erlcapnp_SimpleShortStruct/3,
                                                           VartestVar2,
                                                           MessageRef#message_ref{current_offset =
                                                                                      MessageRef#message_ref.current_offset
                                                                                      +
                                                                                      1})};
internal_decode_erlcapnp_TestShortList(Data,
                                       Pointers,
                                       MessageRef = #message_ref{}) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 128 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:128/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestShortList(PaddedData,
                                           PaddedPointers,
                                           MessageRef).

internal_decode_erlcapnp_TestTextList(<<>>,
                                      <<VartestVar1:64/little-unsigned-integer>>,
                                      MessageRef) ->
    #erlcapnp_TestTextList{testVar1 =
                               follow_struct_list_pointer(fun internal_decode_text/3,
                                                          VartestVar1,
                                                          MessageRef#message_ref{current_offset =
                                                                                     MessageRef#message_ref.current_offset
                                                                                     +
                                                                                     0})};
internal_decode_erlcapnp_TestTextList(Data,
                                      Pointers,
                                      MessageRef = #message_ref{}) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestTextList(PaddedData,
                                          PaddedPointers,
                                          MessageRef).

internal_decode_erlcapnp_TestTextType(<<>>,
                                      <<VartestVar1:64/little-unsigned-integer,
                                        VartestVar2:64/little-unsigned-integer>>,
                                      MessageRef) ->
    #erlcapnp_TestTextType{testVar1 =
                               follow_text_pointer(VartestVar1,
                                                   MessageRef#message_ref{current_offset =
                                                                              MessageRef#message_ref.current_offset
                                                                              +
                                                                              0}),
                           testVar2 =
                               follow_data_pointer(VartestVar2,
                                                   MessageRef#message_ref{current_offset =
                                                                              MessageRef#message_ref.current_offset
                                                                              +
                                                                              1})};
internal_decode_erlcapnp_TestTextType(Data,
                                      Pointers,
                                      MessageRef = #message_ref{}) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 128 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:128/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestTextType(PaddedData,
                                          PaddedPointers,
                                          MessageRef).

internal_decode_erlcapnp_TestUnion(Data =
                                       <<VartestVar1:32/little-signed-integer,
                                         _:160/integer>>,
                                   Pointers = <<_:64/integer>>,
                                   MessageRef) ->
    #erlcapnp_TestUnion{testVar1 = VartestVar1,
                        '' =
                            'internal_decode_erlcapnp_TestUnion.'(Data,
                                                                  Pointers,
                                                                  MessageRef)};
internal_decode_erlcapnp_TestUnion(Data,
                                   Pointers,
                                   MessageRef = #message_ref{}) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    internal_decode_erlcapnp_TestUnion(PaddedData,
                                       PaddedPointers,
                                       MessageRef).

'internal_decode_erlcapnp_TestUnion.'(Data =
                                          <<_:64,
                                            Discriminant:16/little-unsigned-integer,
                                            _:112>>,
                                      Pointers = <<_:64>>,
                                      MessageRef) ->
    case Discriminant of
        0 ->
            <<_:32,Var:32/little-signed-integer,_/bitstring>> = Data,
            {union1,Var};
        1 ->
            <<_:128,Var:64/little-signed-integer,_/bitstring>> = Data,
            {union2,Var};
        2 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {union3,
             follow_int8_list_pointer(Var,
                                      MessageRef#message_ref{current_offset =
                                                                 MessageRef#message_ref.current_offset
                                                                 +
                                                                 0})};
        3 ->
            {union4,undefined}
    end;
'internal_decode_erlcapnp_TestUnion.'(Data,
                                      Pointers,
                                      MessageRef = #message_ref{}) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring,_/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring,_/bitstring>> = Pointers
    end,
    'internal_decode_erlcapnp_TestUnion.'(PaddedData,
                                          PaddedPointers,
                                          MessageRef).

internal_decode_text(_,
                     <<Var:64/little-unsigned-integer,_/binary>>,
                     MessageRef) ->
    follow_text_pointer(Var, MessageRef);
internal_decode_text(_, <<>>, _MessageRef) ->
    undefined.

massage_bool_list(List) ->
    try lists:split(8, List) of
        {First,Last} ->
            lists:reverse(First) ++ massage_bool_list(Last)
    catch
        error:badarg ->
            lists:reverse(List
                          ++
                          lists:duplicate(- length(List) band 7, 0))
    end.



