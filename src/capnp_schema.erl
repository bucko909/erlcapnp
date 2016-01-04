-file("capnp_schema.erl", 1).

-module(capnp_schema).

-include_lib("include/capnp_schema.hrl").

-compile([export_all]).

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

decode_Annotation(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Annotation/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_Brand(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Brand/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Brand.Binding'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Brand.Binding'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Brand.Scope'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Brand.Scope'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Brand.Scope.'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Brand.Scope.'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_CodeGeneratorRequest(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_CodeGeneratorRequest/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_CodeGeneratorRequest.RequestedFile'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_CodeGeneratorRequest.RequestedFile'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_CodeGeneratorRequest.RequestedFile.Import'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_CodeGeneratorRequest.RequestedFile.Import'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_Enumerant(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Enumerant/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_Field(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Field/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Field.'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Field.'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Field.group'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Field.group'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Field.ordinal'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Field.ordinal'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Field.slot'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Field.slot'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_Method(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Method/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_Node(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Node/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Node.'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Node.'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Node.NestedNode'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Node.NestedNode'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Node.Parameter'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Node.Parameter'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Node.annotation'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Node.annotation'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Node.const'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Node.const'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Node.enum'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Node.enum'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Node.interface'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Node.interface'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Node.struct'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Node.struct'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_Superclass(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Superclass/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_Type(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Type/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Type.anyPointer'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Type.anyPointer'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Type.anyPointer.implicitMethodParameter'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Type.anyPointer.implicitMethodParameter'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Type.anyPointer.parameter'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Type.anyPointer.parameter'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Type.enum'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Type.enum'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Type.interface'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Type.interface'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Type.list'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Type.list'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

'decode_Type.struct'(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun 'internal_decode_Type.struct'/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

decode_Value(Data) ->
    {MessageRef,Ptr,Dregs} = decode_envelope(Data),
    Decoded =
        follow_struct_pointer(fun internal_decode_Value/3,
                              Ptr,
                              MessageRef),
    {Decoded,Dregs}.

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

encode_Annotation(#'Annotation'{id = Varid,
                                value = Varvalue,
                                brand = Varbrand},
                  PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrIntvalue,MainLenvalue,ExtraLenvalue,Data1,Extra1} =
        encode_Value(Varvalue, 0),
    Ptrvalue =
        case ZeroOffsetPtrIntvalue of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 1 bsl 2 + ZeroOffsetPtrIntvalue
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLenvalue + ExtraLenvalue,
    {ZeroOffsetPtrIntbrand,MainLenbrand,ExtraLenbrand,Data2,Extra2} =
        encode_Brand(Varbrand, 0),
    Ptrbrand =
        case ZeroOffsetPtrIntbrand of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd1 + 0 bsl 2 + ZeroOffsetPtrIntbrand
        end,
    PtrOffsetWordsFromEnd2 =
        PtrOffsetWordsFromEnd1 + MainLenbrand + ExtraLenbrand,
    {562954248388608,
     3,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<Varid:64/little-unsigned-integer,
       Ptrvalue:64/little-unsigned-integer,
       Ptrbrand:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
encode_Annotation(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

encode_Brand(#'Brand'{scopes = Varscopes}, PtrOffsetWordsFromEnd0) ->
    if
        Varscopes =/= undefined ->
            DataLenscopes = length(Varscopes),
            {FinalOffsetscopes,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {281483566645248,
                                    3,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       'encode_Brand.Scope'(Element,
                                                            Offset - 3),
                                   {ExtraLen + Offset - 3,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenscopes * 3,
                             [<<(DataLenscopes bsl 2 + 281483566645248):64/unsigned-little-integer>>],
                             []},
                            Varscopes),
            FinalOffsetscopes = round(iolist_size(Extra1) / 8),
            Ptrscopes =
                1 bor (0 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLenscopes * 3 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLenscopes * 3
                +
                FinalOffsetscopes;
        true ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrscopes = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {281474976710656,
     1,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<Ptrscopes:64/little-unsigned-integer>>,
     [Data1,Extra1]};
encode_Brand(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Brand.Binding'({VarDiscriminant,Var}, PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        unbound ->
            {281479271677952,
             2,
             0,
             <<0:16/little-unsigned-integer,0:48/integer,0:64/integer>>,
             []};
        type ->
            {ZeroOffsetPtrInt,MainLen,ExtraLen,Data1,Extra1} =
                encode_Type(Var, 0),
            Ptr =
                case ZeroOffsetPtrInt of
                    0 ->
                        0;
                    _ ->
                        PtrOffsetWordsFromEnd0 + 0 bsl 2
                        +
                        ZeroOffsetPtrInt
                end,
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + MainLen + ExtraLen,
            {281479271677952,
             2,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<1:16/little-unsigned-integer,
               0:48/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]}
    end.

'encode_Brand.Scope'(#'Brand.Scope'{scopeId = VarscopeId,'' = Var},
                     PtrOffsetWordsFromEnd0) ->
    <<NoGroupBodyDataAsInt:192/integer>> =
        <<VarscopeId:64/little-unsigned-integer,
          0:64/integer,
          0:64/integer>>,
    {_ZeroOffsetPtrInt,_NewBodyLen,ExtraDataLen,BodyData,ExtraData} =
        'encode_Brand.Scope.'(Var,
                              PtrOffsetWordsFromEnd0
                              -
                              PtrOffsetWordsFromEnd0),
    <<BodyDataAsIntFrom:192/integer>> = BodyData,
    {281483566645248,
     3,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0 + ExtraDataLen,
     <<(NoGroupBodyDataAsInt bor BodyDataAsIntFrom):192/integer>>,
     [[]|ExtraData]};
'encode_Brand.Scope'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Brand.Scope.'({VarDiscriminant,Var}, PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        bind ->
            if
                Var =/= undefined ->
                    DataLen = length(Var),
                    {FinalOffset,Data1,Extra1} =
                        lists:foldl(fun(Element,
                                        {Offset,DataAcc,ExtraAcc}) ->
                                           {281479271677952,
                                            2,
                                            ExtraLen,
                                            ThisBody,
                                            ThisExtra} =
                                               'encode_Brand.Binding'(Element,
                                                                      Offset
                                                                      -
                                                                      2),
                                           {ExtraLen + Offset - 2,
                                            [DataAcc,ThisBody],
                                            [ExtraAcc|ThisExtra]}
                                    end,
                                    {DataLen * 2,
                                     [<<(DataLen bsl 2 + 281479271677952):64/unsigned-little-integer>>],
                                     []},
                                    Var),
                    FinalOffset = round(iolist_size(Extra1) / 8),
                    Ptr =
                        1 bor (0 + PtrOffsetWordsFromEnd0 bsl 2)
                        bor
                        (7 bsl 32)
                        bor
                        (DataLen * 2 bsl 35),
                    PtrOffsetWordsFromEnd1 =
                        PtrOffsetWordsFromEnd0 + 1 + DataLen * 2
                        +
                        FinalOffset;
                true ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {281483566645248,
             3,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<0:64/integer,
               0:16/little-unsigned-integer,
               0:48/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]};
        inherit ->
            {281483566645248,
             3,
             0,
             <<case Var of
                   undefined ->
                       0
               end:0/integer,
               0:64/integer,
               1:16/little-unsigned-integer,
               0:48/integer,
               0:64/integer>>,
             []}
    end.

encode_CodeGeneratorRequest(#'CodeGeneratorRequest'{nodes = Varnodes,
                                                    requestedFiles =
                                                        VarrequestedFiles},
                            PtrOffsetWordsFromEnd0) ->
    if
        Varnodes =/= undefined ->
            DataLennodes = length(Varnodes),
            {FinalOffsetnodes,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {1688871335100416,
                                    11,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Node(Element, Offset - 11),
                                   {ExtraLen + Offset - 11,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLennodes * 11,
                             [<<(DataLennodes bsl 2 + 1688871335100416):64/unsigned-little-integer>>],
                             []},
                            Varnodes),
            FinalOffsetnodes = round(iolist_size(Extra1) / 8),
            Ptrnodes =
                1 bor (1 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLennodes * 11 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLennodes * 11
                +
                FinalOffsetnodes;
        true ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrnodes = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    if
        VarrequestedFiles =/= undefined ->
            DataLenrequestedFiles = length(VarrequestedFiles),
            {FinalOffsetrequestedFiles,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {562954248388608,
                                    3,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       'encode_CodeGeneratorRequest.RequestedFile'(Element,
                                                                                   Offset
                                                                                   -
                                                                                   3),
                                   {ExtraLen + Offset - 3,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenrequestedFiles * 3,
                             [<<(DataLenrequestedFiles bsl 2
                                 +
                                 562954248388608):64/unsigned-little-integer>>],
                             []},
                            VarrequestedFiles),
            FinalOffsetrequestedFiles = round(iolist_size(Extra2) / 8),
            PtrrequestedFiles =
                1 bor (0 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLenrequestedFiles * 3 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLenrequestedFiles * 3
                +
                FinalOffsetrequestedFiles;
        true ->
            Extra2 = <<>>,
            Data2 = [],
            PtrrequestedFiles = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    {562949953421312,
     2,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<Ptrnodes:64/little-unsigned-integer,
       PtrrequestedFiles:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
encode_CodeGeneratorRequest(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_CodeGeneratorRequest.RequestedFile'(#'CodeGeneratorRequest.RequestedFile'{id =
                                                                                      Varid,
                                                                                  filename =
                                                                                      Varfilename,
                                                                                  imports =
                                                                                      Varimports},
                                            PtrOffsetWordsFromEnd0) ->
    if
        is_list(Varfilename);is_binary(Varfilename) ->
            Extra1 = <<>>,
            DataLenfilename = iolist_size(Varfilename) + 1,
            Data1 =
                [Varfilename,
                 <<0:8,
                   0:(- DataLenfilename band 7 * 8)/unsigned-little-integer>>],
            Ptrfilename =
                1 bor (PtrOffsetWordsFromEnd0 + 1 bsl 2) bor (2 bsl 32)
                bor
                (DataLenfilename bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLenfilename + 7 bsr 3);
        Varfilename =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrfilename = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    if
        Varimports =/= undefined ->
            DataLenimports = length(Varimports),
            {FinalOffsetimports,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {281479271677952,
                                    2,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       'encode_CodeGeneratorRequest.RequestedFile.Import'(Element,
                                                                                          Offset
                                                                                          -
                                                                                          2),
                                   {ExtraLen + Offset - 2,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenimports * 2,
                             [<<(DataLenimports bsl 2 + 281479271677952):64/unsigned-little-integer>>],
                             []},
                            Varimports),
            FinalOffsetimports = round(iolist_size(Extra2) / 8),
            Ptrimports =
                1 bor (0 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLenimports * 2 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLenimports * 2
                +
                FinalOffsetimports;
        true ->
            Extra2 = <<>>,
            Data2 = [],
            Ptrimports = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    {562954248388608,
     3,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<Varid:64/little-unsigned-integer,
       Ptrfilename:64/little-unsigned-integer,
       Ptrimports:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
'encode_CodeGeneratorRequest.RequestedFile'(undefined,
                                            _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_CodeGeneratorRequest.RequestedFile.Import'(#'CodeGeneratorRequest.RequestedFile.Import'{id =
                                                                                                    Varid,
                                                                                                name =
                                                                                                    Varname},
                                                   PtrOffsetWordsFromEnd0) ->
    if
        is_list(Varname);is_binary(Varname) ->
            Extra1 = <<>>,
            DataLenname = iolist_size(Varname) + 1,
            Data1 =
                [Varname,
                 <<0:8,
                   0:(- DataLenname band 7 * 8)/unsigned-little-integer>>],
            Ptrname =
                1 bor (PtrOffsetWordsFromEnd0 + 0 bsl 2) bor (2 bsl 32)
                bor
                (DataLenname bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLenname + 7 bsr 3);
        Varname =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrname = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {281479271677952,
     2,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<Varid:64/little-unsigned-integer,
       Ptrname:64/little-unsigned-integer>>,
     [Data1,Extra1]};
'encode_CodeGeneratorRequest.RequestedFile.Import'(undefined,
                                                   _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

encode_Enumerant(#'Enumerant'{codeOrder = VarcodeOrder,
                              name = Varname,
                              annotations = Varannotations},
                 PtrOffsetWordsFromEnd0) ->
    if
        is_list(Varname);is_binary(Varname) ->
            Extra1 = <<>>,
            DataLenname = iolist_size(Varname) + 1,
            Data1 =
                [Varname,
                 <<0:8,
                   0:(- DataLenname band 7 * 8)/unsigned-little-integer>>],
            Ptrname =
                1 bor (PtrOffsetWordsFromEnd0 + 1 bsl 2) bor (2 bsl 32)
                bor
                (DataLenname bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLenname + 7 bsr 3);
        Varname =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrname = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    if
        Varannotations =/= undefined ->
            DataLenannotations = length(Varannotations),
            {FinalOffsetannotations,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {562954248388608,
                                    3,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Annotation(Element,
                                                         Offset - 3),
                                   {ExtraLen + Offset - 3,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenannotations * 3,
                             [<<(DataLenannotations bsl 2
                                 +
                                 562954248388608):64/unsigned-little-integer>>],
                             []},
                            Varannotations),
            FinalOffsetannotations = round(iolist_size(Extra2) / 8),
            Ptrannotations =
                1 bor (0 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLenannotations * 3 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLenannotations * 3
                +
                FinalOffsetannotations;
        true ->
            Extra2 = <<>>,
            Data2 = [],
            Ptrannotations = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    {562954248388608,
     3,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<VarcodeOrder:16/little-unsigned-integer,
       0:48/integer,
       Ptrname:64/little-unsigned-integer,
       Ptrannotations:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
encode_Enumerant(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

encode_Field(#'Field'{codeOrder = VarcodeOrder,
                      discriminantValue = VardiscriminantValue,
                      name = Varname,
                      annotations = Varannotations,
                      '' = Var,
                      ordinal = Varordinal},
             PtrOffsetWordsFromEnd0) ->
    if
        is_list(Varname);is_binary(Varname) ->
            Extra1 = <<>>,
            DataLenname = iolist_size(Varname) + 1,
            Data1 =
                [Varname,
                 <<0:8,
                   0:(- DataLenname band 7 * 8)/unsigned-little-integer>>],
            Ptrname =
                1 bor (PtrOffsetWordsFromEnd0 + 3 bsl 2) bor (2 bsl 32)
                bor
                (DataLenname bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLenname + 7 bsr 3);
        Varname =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrname = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    if
        Varannotations =/= undefined ->
            DataLenannotations = length(Varannotations),
            {FinalOffsetannotations,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {562954248388608,
                                    3,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Annotation(Element,
                                                         Offset - 3),
                                   {ExtraLen + Offset - 3,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenannotations * 3,
                             [<<(DataLenannotations bsl 2
                                 +
                                 562954248388608):64/unsigned-little-integer>>],
                             []},
                            Varannotations),
            FinalOffsetannotations = round(iolist_size(Extra2) / 8),
            Ptrannotations =
                1 bor (2 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLenannotations * 3 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLenannotations * 3
                +
                FinalOffsetannotations;
        true ->
            Extra2 = <<>>,
            Data2 = [],
            Ptrannotations = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    <<NoGroupBodyDataAsInt:448/integer>> =
        <<VarcodeOrder:16/little-unsigned-integer,
          (VardiscriminantValue bxor 65535):16/little-unsigned-integer,
          0:160/integer,
          Ptrname:64/little-unsigned-integer,
          Ptrannotations:64/little-unsigned-integer,
          0:128/integer>>,
    {_ZeroOffsetPtrInt,_NewBodyLen,ExtraDataLen,BodyData,ExtraData} =
        'encode_Field.'(Var,
                        PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0),
    <<BodyDataAsIntFrom:448/integer>> = BodyData,
    {_ZeroOffsetPtrIntordinal,
     _NewBodyLenordinal,
     ExtraDataLenordinal,
     BodyDataordinal,
     ExtraDataordinal} =
        'encode_Field.ordinal'(Varordinal,
                               PtrOffsetWordsFromEnd2
                               -
                               PtrOffsetWordsFromEnd0
                               +
                               ExtraDataLen),
    <<BodyDataAsIntFromordinal:448/integer>> = BodyDataordinal,
    {1125912791744512,
     7,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0 + ExtraDataLen
     +
     ExtraDataLenordinal,
     <<(NoGroupBodyDataAsInt bor BodyDataAsIntFrom
        bor
        BodyDataAsIntFromordinal):448/integer>>,
     [[[Data1,Extra1,Data2,Extra2]|ExtraData]|ExtraDataordinal]};
encode_Field(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Field.'({VarDiscriminant,Var}, PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        slot ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:448/little-unsigned-integer>>,
             ExtraData} =
                'encode_Field.slot'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (0 bsl 64)):448/little-unsigned-integer>>,
             ExtraData};
        group ->
            {1125912791744512,
             7,
             0,
             <<0:64/integer,
               1:16/little-unsigned-integer,
               0:48/integer,
               Var:64/little-unsigned-integer,
               0:256/integer>>,
             []}
    end.

'encode_Field.group'(#'Field.group'{typeId = VartypeId},
                     PtrOffsetWordsFromEnd0) ->
    {1125912791744512,
     7,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<0:128/integer,VartypeId:64/little-unsigned-integer,0:256/integer>>,
     []};
'encode_Field.group'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Field.ordinal'({VarDiscriminant,Var}, PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        implicit ->
            {1125912791744512,
             7,
             0,
             <<case Var of
                   undefined ->
                       0
               end:0/integer,
               0:80/integer,
               0:16/little-unsigned-integer,
               0:96/integer,
               0:256/integer>>,
             []};
        explicit ->
            {1125912791744512,
             7,
             0,
             <<0:80/integer,
               1:16/little-unsigned-integer,
               Var:16/little-unsigned-integer,
               0:80/integer,
               0:256/integer>>,
             []}
    end.

'encode_Field.slot'(#'Field.slot'{offset = Varoffset,
                                  hadExplicitDefault =
                                      VarhadExplicitDefault,
                                  type = Vartype,
                                  defaultValue = VardefaultValue},
                    PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrInttype,MainLentype,ExtraLentype,Data1,Extra1} =
        encode_Type(Vartype, 0),
    Ptrtype =
        case ZeroOffsetPtrInttype of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 1 bsl 2 + ZeroOffsetPtrInttype
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLentype + ExtraLentype,
    {ZeroOffsetPtrIntdefaultValue,
     MainLendefaultValue,
     ExtraLendefaultValue,
     Data2,
     Extra2} =
        encode_Value(VardefaultValue, 0),
    PtrdefaultValue =
        case ZeroOffsetPtrIntdefaultValue of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd1 + 0 bsl 2
                +
                ZeroOffsetPtrIntdefaultValue
        end,
    PtrOffsetWordsFromEnd2 =
        PtrOffsetWordsFromEnd1 + MainLendefaultValue
        +
        ExtraLendefaultValue,
    {1125912791744512,
     7,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<0:32/integer,
       Varoffset:32/little-unsigned-integer,
       0:71/integer,
       case VarhadExplicitDefault of
           false ->
               0;
           true ->
               1
       end:1/integer,
       0:56/integer,
       0:128/integer,
       Ptrtype:64/little-unsigned-integer,
       PtrdefaultValue:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2]};
'encode_Field.slot'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

encode_Method(#'Method'{codeOrder = VarcodeOrder,
                        paramStructType = VarparamStructType,
                        resultStructType = VarresultStructType,
                        name = Varname,
                        annotations = Varannotations,
                        paramBrand = VarparamBrand,
                        resultBrand = VarresultBrand,
                        implicitParameters = VarimplicitParameters},
              PtrOffsetWordsFromEnd0) ->
    if
        is_list(Varname);is_binary(Varname) ->
            Extra1 = <<>>,
            DataLenname = iolist_size(Varname) + 1,
            Data1 =
                [Varname,
                 <<0:8,
                   0:(- DataLenname band 7 * 8)/unsigned-little-integer>>],
            Ptrname =
                1 bor (PtrOffsetWordsFromEnd0 + 4 bsl 2) bor (2 bsl 32)
                bor
                (DataLenname bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLenname + 7 bsr 3);
        Varname =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrname = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    if
        Varannotations =/= undefined ->
            DataLenannotations = length(Varannotations),
            {FinalOffsetannotations,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {562954248388608,
                                    3,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Annotation(Element,
                                                         Offset - 3),
                                   {ExtraLen + Offset - 3,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenannotations * 3,
                             [<<(DataLenannotations bsl 2
                                 +
                                 562954248388608):64/unsigned-little-integer>>],
                             []},
                            Varannotations),
            FinalOffsetannotations = round(iolist_size(Extra2) / 8),
            Ptrannotations =
                1 bor (3 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLenannotations * 3 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLenannotations * 3
                +
                FinalOffsetannotations;
        true ->
            Extra2 = <<>>,
            Data2 = [],
            Ptrannotations = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    {ZeroOffsetPtrIntparamBrand,
     MainLenparamBrand,
     ExtraLenparamBrand,
     Data3,
     Extra3} =
        encode_Brand(VarparamBrand, 0),
    PtrparamBrand =
        case ZeroOffsetPtrIntparamBrand of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd2 + 2 bsl 2
                +
                ZeroOffsetPtrIntparamBrand
        end,
    PtrOffsetWordsFromEnd3 =
        PtrOffsetWordsFromEnd2 + MainLenparamBrand + ExtraLenparamBrand,
    {ZeroOffsetPtrIntresultBrand,
     MainLenresultBrand,
     ExtraLenresultBrand,
     Data4,
     Extra4} =
        encode_Brand(VarresultBrand, 0),
    PtrresultBrand =
        case ZeroOffsetPtrIntresultBrand of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd3 + 1 bsl 2
                +
                ZeroOffsetPtrIntresultBrand
        end,
    PtrOffsetWordsFromEnd4 =
        PtrOffsetWordsFromEnd3 + MainLenresultBrand
        +
        ExtraLenresultBrand,
    if
        VarimplicitParameters =/= undefined ->
            DataLenimplicitParameters = length(VarimplicitParameters),
            {FinalOffsetimplicitParameters,Data5,Extra5} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {281474976710656,
                                    1,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       'encode_Node.Parameter'(Element,
                                                               Offset
                                                               -
                                                               1),
                                   {ExtraLen + Offset - 1,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenimplicitParameters * 1,
                             [<<(DataLenimplicitParameters bsl 2
                                 +
                                 281474976710656):64/unsigned-little-integer>>],
                             []},
                            VarimplicitParameters),
            FinalOffsetimplicitParameters =
                round(iolist_size(Extra5) / 8),
            PtrimplicitParameters =
                1 bor (0 + PtrOffsetWordsFromEnd4 bsl 2) bor (7 bsl 32)
                bor
                (DataLenimplicitParameters * 1 bsl 35),
            PtrOffsetWordsFromEnd5 =
                PtrOffsetWordsFromEnd4 + 1
                +
                DataLenimplicitParameters * 1
                +
                FinalOffsetimplicitParameters;
        true ->
            Extra5 = <<>>,
            Data5 = [],
            PtrimplicitParameters = 0,
            PtrOffsetWordsFromEnd5 = PtrOffsetWordsFromEnd4
    end,
    {1407387768455168,
     8,
     PtrOffsetWordsFromEnd5 - PtrOffsetWordsFromEnd0,
     <<VarcodeOrder:16/little-unsigned-integer,
       0:48/integer,
       VarparamStructType:64/little-unsigned-integer,
       VarresultStructType:64/little-unsigned-integer,
       Ptrname:64/little-unsigned-integer,
       Ptrannotations:64/little-unsigned-integer,
       PtrparamBrand:64/little-unsigned-integer,
       PtrresultBrand:64/little-unsigned-integer,
       PtrimplicitParameters:64/little-unsigned-integer>>,
     [Data1,Extra1,Data2,Extra2,Data3,Extra3,Data4,Extra4,Data5,Extra5]};
encode_Method(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

encode_Node(#'Node'{id = Varid,
                    displayNamePrefixLength = VardisplayNamePrefixLength,
                    scopeId = VarscopeId,
                    isGeneric = VarisGeneric,
                    displayName = VardisplayName,
                    nestedNodes = VarnestedNodes,
                    annotations = Varannotations,
                    parameters = Varparameters,
                    '' = Var},
            PtrOffsetWordsFromEnd0) ->
    if
        is_list(VardisplayName);is_binary(VardisplayName) ->
            Extra1 = <<>>,
            DataLendisplayName = iolist_size(VardisplayName) + 1,
            Data1 =
                [VardisplayName,
                 <<0:8,
                   0:(- DataLendisplayName band 7 * 8)/unsigned-little-integer>>],
            PtrdisplayName =
                1 bor (PtrOffsetWordsFromEnd0 + 5 bsl 2) bor (2 bsl 32)
                bor
                (DataLendisplayName bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLendisplayName + 7 bsr 3);
        VardisplayName =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            PtrdisplayName = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    if
        VarnestedNodes =/= undefined ->
            DataLennestedNodes = length(VarnestedNodes),
            {FinalOffsetnestedNodes,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {281479271677952,
                                    2,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       'encode_Node.NestedNode'(Element,
                                                                Offset
                                                                -
                                                                2),
                                   {ExtraLen + Offset - 2,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLennestedNodes * 2,
                             [<<(DataLennestedNodes bsl 2
                                 +
                                 281479271677952):64/unsigned-little-integer>>],
                             []},
                            VarnestedNodes),
            FinalOffsetnestedNodes = round(iolist_size(Extra2) / 8),
            PtrnestedNodes =
                1 bor (4 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLennestedNodes * 2 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLennestedNodes * 2
                +
                FinalOffsetnestedNodes;
        true ->
            Extra2 = <<>>,
            Data2 = [],
            PtrnestedNodes = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    if
        Varannotations =/= undefined ->
            DataLenannotations = length(Varannotations),
            {FinalOffsetannotations,Data3,Extra3} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {562954248388608,
                                    3,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Annotation(Element,
                                                         Offset - 3),
                                   {ExtraLen + Offset - 3,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenannotations * 3,
                             [<<(DataLenannotations bsl 2
                                 +
                                 562954248388608):64/unsigned-little-integer>>],
                             []},
                            Varannotations),
            FinalOffsetannotations = round(iolist_size(Extra3) / 8),
            Ptrannotations =
                1 bor (3 + PtrOffsetWordsFromEnd2 bsl 2) bor (7 bsl 32)
                bor
                (DataLenannotations * 3 bsl 35),
            PtrOffsetWordsFromEnd3 =
                PtrOffsetWordsFromEnd2 + 1 + DataLenannotations * 3
                +
                FinalOffsetannotations;
        true ->
            Extra3 = <<>>,
            Data3 = [],
            Ptrannotations = 0,
            PtrOffsetWordsFromEnd3 = PtrOffsetWordsFromEnd2
    end,
    if
        Varparameters =/= undefined ->
            DataLenparameters = length(Varparameters),
            {FinalOffsetparameters,Data4,Extra4} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {281474976710656,
                                    1,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       'encode_Node.Parameter'(Element,
                                                               Offset
                                                               -
                                                               1),
                                   {ExtraLen + Offset - 1,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenparameters * 1,
                             [<<(DataLenparameters bsl 2
                                 +
                                 281474976710656):64/unsigned-little-integer>>],
                             []},
                            Varparameters),
            FinalOffsetparameters = round(iolist_size(Extra4) / 8),
            Ptrparameters =
                1 bor (0 + PtrOffsetWordsFromEnd3 bsl 2) bor (7 bsl 32)
                bor
                (DataLenparameters * 1 bsl 35),
            PtrOffsetWordsFromEnd4 =
                PtrOffsetWordsFromEnd3 + 1 + DataLenparameters * 1
                +
                FinalOffsetparameters;
        true ->
            Extra4 = <<>>,
            Data4 = [],
            Ptrparameters = 0,
            PtrOffsetWordsFromEnd4 = PtrOffsetWordsFromEnd3
    end,
    <<NoGroupBodyDataAsInt:704/integer>> =
        <<Varid:64/little-unsigned-integer,
          VardisplayNamePrefixLength:32/little-unsigned-integer,
          0:32/integer,
          VarscopeId:64/little-unsigned-integer,
          0:103/integer,
          case VarisGeneric of
              false ->
                  0;
              true ->
                  1
          end:1/integer,
          0:24/integer,
          PtrdisplayName:64/little-unsigned-integer,
          PtrnestedNodes:64/little-unsigned-integer,
          Ptrannotations:64/little-unsigned-integer,
          0:128/integer,
          Ptrparameters:64/little-unsigned-integer>>,
    {_ZeroOffsetPtrInt,_NewBodyLen,ExtraDataLen,BodyData,ExtraData} =
        'encode_Node.'(Var,
                       PtrOffsetWordsFromEnd4 - PtrOffsetWordsFromEnd0),
    <<BodyDataAsIntFrom:704/integer>> = BodyData,
    {1688871335100416,
     11,
     PtrOffsetWordsFromEnd4 - PtrOffsetWordsFromEnd0 + ExtraDataLen,
     <<(NoGroupBodyDataAsInt bor BodyDataAsIntFrom):704/integer>>,
     [[Data1,Extra1,Data2,Extra2,Data3,Extra3,Data4,Extra4]|ExtraData]};
encode_Node(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Node.'({VarDiscriminant,Var}, PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        file ->
            {1688871335100416,
             11,
             0,
             <<case Var of
                   undefined ->
                       0
               end:0/integer,
               0:96/integer,
               0:16/little-unsigned-integer,
               0:208/integer,
               0:384/integer>>,
             []};
        struct ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:704/little-unsigned-integer>>,
             ExtraData} =
                'encode_Node.struct'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (1 bsl 96)):704/little-unsigned-integer>>,
             ExtraData};
        enum ->
            if
                Var =/= undefined ->
                    DataLen = length(Var),
                    {FinalOffset,Data1,Extra1} =
                        lists:foldl(fun(Element,
                                        {Offset,DataAcc,ExtraAcc}) ->
                                           {562954248388608,
                                            3,
                                            ExtraLen,
                                            ThisBody,
                                            ThisExtra} =
                                               encode_Enumerant(Element,
                                                                Offset
                                                                -
                                                                3),
                                           {ExtraLen + Offset - 3,
                                            [DataAcc,ThisBody],
                                            [ExtraAcc|ThisExtra]}
                                    end,
                                    {DataLen * 3,
                                     [<<(DataLen bsl 2 + 562954248388608):64/unsigned-little-integer>>],
                                     []},
                                    Var),
                    FinalOffset = round(iolist_size(Extra1) / 8),
                    Ptr =
                        1 bor (0 + PtrOffsetWordsFromEnd0 bsl 2)
                        bor
                        (7 bsl 32)
                        bor
                        (DataLen * 3 bsl 35),
                    PtrOffsetWordsFromEnd1 =
                        PtrOffsetWordsFromEnd0 + 1 + DataLen * 3
                        +
                        FinalOffset;
                true ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {1688871335100416,
             11,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<0:96/integer,
               2:16/little-unsigned-integer,
               0:208/integer,
               0:192/integer,
               Ptr:64/little-unsigned-integer,
               0:128/integer>>,
             [Data1,Extra1]};
        interface ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:704/little-unsigned-integer>>,
             ExtraData} =
                'encode_Node.interface'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (3 bsl 96)):704/little-unsigned-integer>>,
             ExtraData};
        const ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:704/little-unsigned-integer>>,
             ExtraData} =
                'encode_Node.const'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (4 bsl 96)):704/little-unsigned-integer>>,
             ExtraData};
        annotation ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:704/little-unsigned-integer>>,
             ExtraData} =
                'encode_Node.annotation'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (5 bsl 96)):704/little-unsigned-integer>>,
             ExtraData}
    end.

'encode_Node.NestedNode'(#'Node.NestedNode'{id = Varid,name = Varname},
                         PtrOffsetWordsFromEnd0) ->
    if
        is_list(Varname);is_binary(Varname) ->
            Extra1 = <<>>,
            DataLenname = iolist_size(Varname) + 1,
            Data1 =
                [Varname,
                 <<0:8,
                   0:(- DataLenname band 7 * 8)/unsigned-little-integer>>],
            Ptrname =
                1 bor (PtrOffsetWordsFromEnd0 + 0 bsl 2) bor (2 bsl 32)
                bor
                (DataLenname bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLenname + 7 bsr 3);
        Varname =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrname = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {281479271677952,
     2,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<Varid:64/little-unsigned-integer,
       Ptrname:64/little-unsigned-integer>>,
     [Data1,Extra1]};
'encode_Node.NestedNode'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Node.Parameter'(#'Node.Parameter'{name = Varname},
                        PtrOffsetWordsFromEnd0) ->
    if
        is_list(Varname);is_binary(Varname) ->
            Extra1 = <<>>,
            DataLenname = iolist_size(Varname) + 1,
            Data1 =
                [Varname,
                 <<0:8,
                   0:(- DataLenname band 7 * 8)/unsigned-little-integer>>],
            Ptrname =
                1 bor (PtrOffsetWordsFromEnd0 + 0 bsl 2) bor (2 bsl 32)
                bor
                (DataLenname bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + (DataLenname + 7 bsr 3);
        Varname =:= undefined ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrname = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {281474976710656,
     1,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<Ptrname:64/little-unsigned-integer>>,
     [Data1,Extra1]};
'encode_Node.Parameter'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Node.annotation'(#'Node.annotation'{targetsGroup =
                                                VartargetsGroup,
                                            targetsUnion =
                                                VartargetsUnion,
                                            targetsField =
                                                VartargetsField,
                                            targetsStruct =
                                                VartargetsStruct,
                                            targetsEnumerant =
                                                VartargetsEnumerant,
                                            targetsEnum = VartargetsEnum,
                                            targetsConst =
                                                VartargetsConst,
                                            targetsFile = VartargetsFile,
                                            targetsAnnotation =
                                                VartargetsAnnotation,
                                            targetsParam =
                                                VartargetsParam,
                                            targetsMethod =
                                                VartargetsMethod,
                                            targetsInterface =
                                                VartargetsInterface,
                                            type = Vartype},
                         PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrInttype,MainLentype,ExtraLentype,Data1,Extra1} =
        encode_Type(Vartype, 0),
    Ptrtype =
        case ZeroOffsetPtrInttype of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 2 bsl 2 + ZeroOffsetPtrInttype
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLentype + ExtraLentype,
    {1688871335100416,
     11,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<0:112/integer,
       case VartargetsGroup of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsUnion of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsField of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsStruct of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsEnumerant of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsEnum of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsConst of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsFile of
           false ->
               0;
           true ->
               1
       end:1/integer,
       0:4/integer,
       case VartargetsAnnotation of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsParam of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsMethod of
           false ->
               0;
           true ->
               1
       end:1/integer,
       case VartargetsInterface of
           false ->
               0;
           true ->
               1
       end:1/integer,
       0:192/integer,
       0:192/integer,
       Ptrtype:64/little-unsigned-integer,
       0:128/integer>>,
     [Data1,Extra1]};
'encode_Node.annotation'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Node.const'(#'Node.const'{type = Vartype,value = Varvalue},
                    PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrInttype,MainLentype,ExtraLentype,Data1,Extra1} =
        encode_Type(Vartype, 0),
    Ptrtype =
        case ZeroOffsetPtrInttype of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 2 bsl 2 + ZeroOffsetPtrInttype
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLentype + ExtraLentype,
    {ZeroOffsetPtrIntvalue,MainLenvalue,ExtraLenvalue,Data2,Extra2} =
        encode_Value(Varvalue, 0),
    Ptrvalue =
        case ZeroOffsetPtrIntvalue of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd1 + 1 bsl 2 + ZeroOffsetPtrIntvalue
        end,
    PtrOffsetWordsFromEnd2 =
        PtrOffsetWordsFromEnd1 + MainLenvalue + ExtraLenvalue,
    {1688871335100416,
     11,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<0:320/integer,
       0:192/integer,
       Ptrtype:64/little-unsigned-integer,
       Ptrvalue:64/little-unsigned-integer,
       0:64/integer>>,
     [Data1,Extra1,Data2,Extra2]};
'encode_Node.const'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Node.enum'(#'Node.enum'{enumerants = Varenumerants},
                   PtrOffsetWordsFromEnd0) ->
    if
        Varenumerants =/= undefined ->
            DataLenenumerants = length(Varenumerants),
            {FinalOffsetenumerants,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {562954248388608,
                                    3,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Enumerant(Element,
                                                        Offset - 3),
                                   {ExtraLen + Offset - 3,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenenumerants * 3,
                             [<<(DataLenenumerants bsl 2
                                 +
                                 562954248388608):64/unsigned-little-integer>>],
                             []},
                            Varenumerants),
            FinalOffsetenumerants = round(iolist_size(Extra1) / 8),
            Ptrenumerants =
                1 bor (2 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLenenumerants * 3 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLenenumerants * 3
                +
                FinalOffsetenumerants;
        true ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrenumerants = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {1688871335100416,
     11,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<0:320/integer,
       0:192/integer,
       Ptrenumerants:64/little-unsigned-integer,
       0:128/integer>>,
     [Data1,Extra1]};
'encode_Node.enum'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Node.interface'(#'Node.interface'{methods = Varmethods,
                                          superclasses = Varsuperclasses},
                        PtrOffsetWordsFromEnd0) ->
    if
        Varmethods =/= undefined ->
            DataLenmethods = length(Varmethods),
            {FinalOffsetmethods,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {1407387768455168,
                                    8,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Method(Element,
                                                     Offset - 8),
                                   {ExtraLen + Offset - 8,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenmethods * 8,
                             [<<(DataLenmethods bsl 2 + 1407387768455168):64/unsigned-little-integer>>],
                             []},
                            Varmethods),
            FinalOffsetmethods = round(iolist_size(Extra1) / 8),
            Ptrmethods =
                1 bor (2 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLenmethods * 8 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLenmethods * 8
                +
                FinalOffsetmethods;
        true ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrmethods = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    if
        Varsuperclasses =/= undefined ->
            DataLensuperclasses = length(Varsuperclasses),
            {FinalOffsetsuperclasses,Data2,Extra2} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {281479271677952,
                                    2,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Superclass(Element,
                                                         Offset - 2),
                                   {ExtraLen + Offset - 2,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLensuperclasses * 2,
                             [<<(DataLensuperclasses bsl 2
                                 +
                                 281479271677952):64/unsigned-little-integer>>],
                             []},
                            Varsuperclasses),
            FinalOffsetsuperclasses = round(iolist_size(Extra2) / 8),
            Ptrsuperclasses =
                1 bor (1 + PtrOffsetWordsFromEnd1 bsl 2) bor (7 bsl 32)
                bor
                (DataLensuperclasses * 2 bsl 35),
            PtrOffsetWordsFromEnd2 =
                PtrOffsetWordsFromEnd1 + 1 + DataLensuperclasses * 2
                +
                FinalOffsetsuperclasses;
        true ->
            Extra2 = <<>>,
            Data2 = [],
            Ptrsuperclasses = 0,
            PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1
    end,
    {1688871335100416,
     11,
     PtrOffsetWordsFromEnd2 - PtrOffsetWordsFromEnd0,
     <<0:320/integer,
       0:192/integer,
       Ptrmethods:64/little-unsigned-integer,
       Ptrsuperclasses:64/little-unsigned-integer,
       0:64/integer>>,
     [Data1,Extra1,Data2,Extra2]};
'encode_Node.interface'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Node.struct'(#'Node.struct'{dataWordCount = VardataWordCount,
                                    pointerCount = VarpointerCount,
                                    preferredListEncoding =
                                        VarpreferredListEncoding,
                                    isGroup = VarisGroup,
                                    discriminantCount =
                                        VardiscriminantCount,
                                    discriminantOffset =
                                        VardiscriminantOffset,
                                    fields = Varfields},
                     PtrOffsetWordsFromEnd0) ->
    if
        Varfields =/= undefined ->
            DataLenfields = length(Varfields),
            {FinalOffsetfields,Data1,Extra1} =
                lists:foldl(fun(Element, {Offset,DataAcc,ExtraAcc}) ->
                                   {1125912791744512,
                                    7,
                                    ExtraLen,
                                    ThisBody,
                                    ThisExtra} =
                                       encode_Field(Element, Offset - 7),
                                   {ExtraLen + Offset - 7,
                                    [DataAcc,ThisBody],
                                    [ExtraAcc|ThisExtra]}
                            end,
                            {DataLenfields * 7,
                             [<<(DataLenfields bsl 2 + 1125912791744512):64/unsigned-little-integer>>],
                             []},
                            Varfields),
            FinalOffsetfields = round(iolist_size(Extra1) / 8),
            Ptrfields =
                1 bor (2 + PtrOffsetWordsFromEnd0 bsl 2) bor (7 bsl 32)
                bor
                (DataLenfields * 7 bsl 35),
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + 1 + DataLenfields * 7
                +
                FinalOffsetfields;
        true ->
            Extra1 = <<>>,
            Data1 = [],
            Ptrfields = 0,
            PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
    end,
    {1688871335100416,
     11,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<0:112/integer,
       VardataWordCount:16/little-unsigned-integer,
       0:64/integer,
       VarpointerCount:16/little-unsigned-integer,
       case VarpreferredListEncoding of
           empty ->
               0;
           bit ->
               1;
           byte ->
               2;
           twoBytes ->
               3;
           fourBytes ->
               4;
           eightBytes ->
               5;
           pointer ->
               6;
           inlineComposite ->
               7
       end:16/little-unsigned-integer,
       0:7/integer,
       case VarisGroup of
           false ->
               0;
           true ->
               1
       end:1/integer,
       0:8/integer,
       VardiscriminantCount:16/little-unsigned-integer,
       VardiscriminantOffset:32/little-unsigned-integer,
       0:32/integer,
       0:192/integer,
       Ptrfields:64/little-unsigned-integer,
       0:128/integer>>,
     [Data1,Extra1]};
'encode_Node.struct'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

encode_Superclass(#'Superclass'{id = Varid,brand = Varbrand},
                  PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrIntbrand,MainLenbrand,ExtraLenbrand,Data1,Extra1} =
        encode_Brand(Varbrand, 0),
    Ptrbrand =
        case ZeroOffsetPtrIntbrand of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 0 bsl 2 + ZeroOffsetPtrIntbrand
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLenbrand + ExtraLenbrand,
    {281479271677952,
     2,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<Varid:64/little-unsigned-integer,
       Ptrbrand:64/little-unsigned-integer>>,
     [Data1,Extra1]};
encode_Superclass(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

encode_Type({VarDiscriminant,Var}, PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        void ->
            {281487861612544,
             4,
             0,
             <<0:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        bool ->
            {281487861612544,
             4,
             0,
             <<1:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        int8 ->
            {281487861612544,
             4,
             0,
             <<2:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        int16 ->
            {281487861612544,
             4,
             0,
             <<3:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        int32 ->
            {281487861612544,
             4,
             0,
             <<4:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        int64 ->
            {281487861612544,
             4,
             0,
             <<5:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        uint8 ->
            {281487861612544,
             4,
             0,
             <<6:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        uint16 ->
            {281487861612544,
             4,
             0,
             <<7:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        uint32 ->
            {281487861612544,
             4,
             0,
             <<8:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        uint64 ->
            {281487861612544,
             4,
             0,
             <<9:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        float32 ->
            {281487861612544,
             4,
             0,
             <<10:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        float64 ->
            {281487861612544,
             4,
             0,
             <<11:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        text ->
            {281487861612544,
             4,
             0,
             <<12:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        data ->
            {281487861612544,
             4,
             0,
             <<13:16/little-unsigned-integer,0:176/integer,0:64/integer>>,
             []};
        list ->
            {ZeroOffsetPtrInt,MainLen,ExtraLen,Data1,Extra1} =
                encode_Type(Var, 0),
            Ptr =
                case ZeroOffsetPtrInt of
                    0 ->
                        0;
                    _ ->
                        PtrOffsetWordsFromEnd0 + 0 bsl 2
                        +
                        ZeroOffsetPtrInt
                end,
            PtrOffsetWordsFromEnd1 =
                PtrOffsetWordsFromEnd0 + MainLen + ExtraLen,
            {281487861612544,
             4,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<14:16/little-unsigned-integer,
               0:176/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]};
        enum ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:256/little-unsigned-integer>>,
             ExtraData} =
                'encode_Type.enum'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (15 bsl 0)):256/little-unsigned-integer>>,
             ExtraData};
        struct ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:256/little-unsigned-integer>>,
             ExtraData} =
                'encode_Type.struct'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (16 bsl 0)):256/little-unsigned-integer>>,
             ExtraData};
        interface ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:256/little-unsigned-integer>>,
             ExtraData} =
                'encode_Type.interface'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (17 bsl 0)):256/little-unsigned-integer>>,
             ExtraData};
        anyPointer ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:256/little-unsigned-integer>>,
             ExtraData} =
                'encode_Type.anyPointer'(Var, PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (18 bsl 0)):256/little-unsigned-integer>>,
             ExtraData}
    end.

'encode_Type.anyPointer'({VarDiscriminant,Var}, PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        unconstrained ->
            {281487861612544,
             4,
             0,
             <<case Var of
                   undefined ->
                       0
               end:0/integer,
               0:64/integer,
               0:16/little-unsigned-integer,
               0:112/integer,
               0:64/integer>>,
             []};
        parameter ->
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<DataInt:256/little-unsigned-integer>>,
             ExtraData} =
                'encode_Type.anyPointer.parameter'(Var,
                                                   PtrOffsetWordsFromEnd0),
            {ZeroOffsetPtrInt,
             MainLen,
             ExtraLen,
             <<(DataInt bor (1 bsl 64)):256/little-unsigned-integer>>,
             ExtraData};
        implicitMethodParameter ->
            {281487861612544,
             4,
             0,
             <<0:64/integer,
               2:16/little-unsigned-integer,
               Var:16/little-unsigned-integer,
               0:96/integer,
               0:64/integer>>,
             []}
    end.

'encode_Type.anyPointer.implicitMethodParameter'(#'Type.anyPointer.implicitMethodParameter'{parameterIndex =
                                                                                                VarparameterIndex},
                                                 PtrOffsetWordsFromEnd0) ->
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<0:80/integer,
       VarparameterIndex:16/little-unsigned-integer,
       0:96/integer,
       0:64/integer>>,
     []};
'encode_Type.anyPointer.implicitMethodParameter'(undefined,
                                                 _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Type.anyPointer.parameter'(#'Type.anyPointer.parameter'{parameterIndex =
                                                                    VarparameterIndex,
                                                                scopeId =
                                                                    VarscopeId},
                                   PtrOffsetWordsFromEnd0) ->
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd0 - PtrOffsetWordsFromEnd0,
     <<0:80/integer,
       VarparameterIndex:16/little-unsigned-integer,
       0:32/integer,
       VarscopeId:64/little-unsigned-integer,
       0:64/integer>>,
     []};
'encode_Type.anyPointer.parameter'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Type.enum'(#'Type.enum'{typeId = VartypeId,brand = Varbrand},
                   PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrIntbrand,MainLenbrand,ExtraLenbrand,Data1,Extra1} =
        encode_Brand(Varbrand, 0),
    Ptrbrand =
        case ZeroOffsetPtrIntbrand of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 0 bsl 2 + ZeroOffsetPtrIntbrand
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLenbrand + ExtraLenbrand,
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<0:64/integer,
       VartypeId:64/little-unsigned-integer,
       0:64/integer,
       Ptrbrand:64/little-unsigned-integer>>,
     [Data1,Extra1]};
'encode_Type.enum'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Type.interface'(#'Type.interface'{typeId = VartypeId,
                                          brand = Varbrand},
                        PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrIntbrand,MainLenbrand,ExtraLenbrand,Data1,Extra1} =
        encode_Brand(Varbrand, 0),
    Ptrbrand =
        case ZeroOffsetPtrIntbrand of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 0 bsl 2 + ZeroOffsetPtrIntbrand
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLenbrand + ExtraLenbrand,
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<0:64/integer,
       VartypeId:64/little-unsigned-integer,
       0:64/integer,
       Ptrbrand:64/little-unsigned-integer>>,
     [Data1,Extra1]};
'encode_Type.interface'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Type.list'(#'Type.list'{elementType = VarelementType},
                   PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrIntelementType,
     MainLenelementType,
     ExtraLenelementType,
     Data1,
     Extra1} =
        encode_Type(VarelementType, 0),
    PtrelementType =
        case ZeroOffsetPtrIntelementType of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 0 bsl 2
                +
                ZeroOffsetPtrIntelementType
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLenelementType
        +
        ExtraLenelementType,
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<0:192/integer,PtrelementType:64/little-unsigned-integer>>,
     [Data1,Extra1]};
'encode_Type.list'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

'encode_Type.struct'(#'Type.struct'{typeId = VartypeId,brand = Varbrand},
                     PtrOffsetWordsFromEnd0) ->
    {ZeroOffsetPtrIntbrand,MainLenbrand,ExtraLenbrand,Data1,Extra1} =
        encode_Brand(Varbrand, 0),
    Ptrbrand =
        case ZeroOffsetPtrIntbrand of
            0 ->
                0;
            _ ->
                PtrOffsetWordsFromEnd0 + 0 bsl 2 + ZeroOffsetPtrIntbrand
        end,
    PtrOffsetWordsFromEnd1 =
        PtrOffsetWordsFromEnd0 + MainLenbrand + ExtraLenbrand,
    {281487861612544,
     4,
     PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
     <<0:64/integer,
       VartypeId:64/little-unsigned-integer,
       0:64/integer,
       Ptrbrand:64/little-unsigned-integer>>,
     [Data1,Extra1]};
'encode_Type.struct'(undefined, _PtrOffsetWordsFromEnd0) ->
    {0,0,0,[],[]}.

encode_Value({VarDiscriminant,Var}, PtrOffsetWordsFromEnd0) ->
    case VarDiscriminant of
        void ->
            {281483566645248,
             3,
             0,
             <<0:16/little-unsigned-integer,0:112/integer,0:64/integer>>,
             []};
        bool ->
            {281483566645248,
             3,
             0,
             <<1:16/little-unsigned-integer,
               0:7/integer,
               case Var of
                   false ->
                       0;
                   true ->
                       1
               end:1/integer,
               0:104/integer,
               0:64/integer>>,
             []};
        int8 ->
            {281483566645248,
             3,
             0,
             <<2:16/little-unsigned-integer,
               Var:8/little-signed-integer,
               0:104/integer,
               0:64/integer>>,
             []};
        int16 ->
            {281483566645248,
             3,
             0,
             <<3:16/little-unsigned-integer,
               Var:16/little-signed-integer,
               0:96/integer,
               0:64/integer>>,
             []};
        int32 ->
            {281483566645248,
             3,
             0,
             <<4:16/little-unsigned-integer,
               0:16/integer,
               Var:32/little-signed-integer,
               0:64/integer,
               0:64/integer>>,
             []};
        int64 ->
            {281483566645248,
             3,
             0,
             <<5:16/little-unsigned-integer,
               0:48/integer,
               Var:64/little-signed-integer,
               0:64/integer>>,
             []};
        uint8 ->
            {281483566645248,
             3,
             0,
             <<6:16/little-unsigned-integer,
               Var:8/little-unsigned-integer,
               0:104/integer,
               0:64/integer>>,
             []};
        uint16 ->
            {281483566645248,
             3,
             0,
             <<7:16/little-unsigned-integer,
               Var:16/little-unsigned-integer,
               0:96/integer,
               0:64/integer>>,
             []};
        uint32 ->
            {281483566645248,
             3,
             0,
             <<8:16/little-unsigned-integer,
               0:16/integer,
               Var:32/little-unsigned-integer,
               0:64/integer,
               0:64/integer>>,
             []};
        uint64 ->
            {281483566645248,
             3,
             0,
             <<9:16/little-unsigned-integer,
               0:48/integer,
               Var:64/little-unsigned-integer,
               0:64/integer>>,
             []};
        float32 ->
            {281483566645248,
             3,
             0,
             <<10:16/little-unsigned-integer,
               0:16/integer,
               Var:32/little-float,
               0:64/integer,
               0:64/integer>>,
             []};
        float64 ->
            {281483566645248,
             3,
             0,
             <<11:16/little-unsigned-integer,
               0:48/integer,
               Var:64/little-float,
               0:64/integer>>,
             []};
        text ->
            if
                is_list(Var);is_binary(Var) ->
                    Extra1 = <<>>,
                    DataLen = iolist_size(Var) + 1,
                    Data1 =
                        [Var,
                         <<0:8,
                           0:(- DataLen band 7 * 8)/unsigned-little-integer>>],
                    Ptr =
                        1 bor (PtrOffsetWordsFromEnd0 + 0 bsl 2)
                        bor
                        (2 bsl 32)
                        bor
                        (DataLen bsl 35),
                    PtrOffsetWordsFromEnd1 =
                        PtrOffsetWordsFromEnd0 + (DataLen + 7 bsr 3);
                Var =:= undefined ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {281483566645248,
             3,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<12:16/little-unsigned-integer,
               0:112/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]};
        data ->
            if
                is_list(Var);is_binary(Var) ->
                    Extra1 = <<>>,
                    DataLen = iolist_size(Var),
                    Data1 =
                        [Var,
                         <<0:(- DataLen band 7 * 8)/unsigned-little-integer>>],
                    Ptr =
                        1 bor (PtrOffsetWordsFromEnd0 + 0 bsl 2)
                        bor
                        (2 bsl 32)
                        bor
                        (DataLen bsl 35),
                    PtrOffsetWordsFromEnd1 =
                        PtrOffsetWordsFromEnd0 + (DataLen + 7 bsr 3);
                Var =:= undefined ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {281483566645248,
             3,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<13:16/little-unsigned-integer,
               0:112/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]};
        list ->
            if
                Var =:= undefined ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {281483566645248,
             3,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<14:16/little-unsigned-integer,
               0:112/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]};
        enum ->
            {281483566645248,
             3,
             0,
             <<15:16/little-unsigned-integer,
               Var:16/little-unsigned-integer,
               0:96/integer,
               0:64/integer>>,
             []};
        struct ->
            if
                Var =:= undefined ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {281483566645248,
             3,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<16:16/little-unsigned-integer,
               0:112/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]};
        interface ->
            {281483566645248,
             3,
             0,
             <<17:16/little-unsigned-integer,0:112/integer,0:64/integer>>,
             []};
        anyPointer ->
            if
                Var =:= undefined ->
                    Extra1 = <<>>,
                    Data1 = [],
                    Ptr = 0,
                    PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0
            end,
            {281483566645248,
             3,
             PtrOffsetWordsFromEnd1 - PtrOffsetWordsFromEnd0,
             <<18:16/little-unsigned-integer,
               0:112/integer,
               Ptr:64/little-unsigned-integer>>,
             [Data1,Extra1]}
    end.

envelope_Annotation(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Annotation(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_Brand(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Brand(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

'envelope_Brand.Binding'(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        'encode_Brand.Binding'(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

'envelope_Brand.Scope'(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        'encode_Brand.Scope'(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_CodeGeneratorRequest(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_CodeGeneratorRequest(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

'envelope_CodeGeneratorRequest.RequestedFile'(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        'encode_CodeGeneratorRequest.RequestedFile'(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

'envelope_CodeGeneratorRequest.RequestedFile.Import'(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        'encode_CodeGeneratorRequest.RequestedFile.Import'(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_Enumerant(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Enumerant(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_Field(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Field(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_Method(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Method(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_Node(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Node(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

'envelope_Node.NestedNode'(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        'encode_Node.NestedNode'(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

'envelope_Node.Parameter'(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        'encode_Node.Parameter'(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_Superclass(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Superclass(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_Type(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Type(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

envelope_Value(Input) ->
    {ZeroOffsetPtrInt,MainDataLen,ExtraDataLen,MainData,ExtraData} =
        encode_Value(Input, 0),
    list_to_binary([<<0:32/unsigned-little-integer,
                      (1 + MainDataLen + ExtraDataLen):32/unsigned-little-integer,
                      ZeroOffsetPtrInt:64/unsigned-little-integer>>,
                    MainData,
                    ExtraData]).

follow_data_pointer(0, _) ->
    undefined;
follow_data_pointer(PointerInt, MessageRef)
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
    Length = PointerInt bsr 35 - 0,
    MessageBits = Length bsl 3,
    <<_:SkipBits,ListData:MessageBits/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    ListData;
follow_data_pointer(PointerInt,
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
    follow_text_pointer(NewPointerInt, NewMessageRef).

follow_struct_pointer(DecodeFun, 0, MessageRef) ->
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
    follow_struct_pointer(DecodeFun, NewPointerInt, NewMessageRef).

follow_tagged_struct_list_pointer(DecodeFun, 0, MessageRef) ->
    undefined;
follow_tagged_struct_list_pointer(DecodeFun, PointerInt, MessageRef)
    when PointerInt band 3 == 1 ->
    PointerOffset =
        case PointerInt band (1 bsl 31) of
            0 ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) + 1;
            _ ->
                (PointerInt bsr 2) band (1 bsl 30 - 1) - (1 bsl 30) + 1
        end,
    NewOffset = MessageRef#message_ref.current_offset + PointerOffset,
    SkipBits = NewOffset bsl 6,
    <<_:SkipBits,Tag:64/little-unsigned-integer,Rest/binary>> =
        MessageRef#message_ref.current_segment,
    Length = (Tag bsr 2) band (1 bsl 30 - 1),
    DWords = (Tag bsr 32) band (1 bsl 16 - 1),
    PWords = (Tag bsr 48) band (1 bsl 16 - 1),
    decode_struct_list(DecodeFun,
                       Length,
                       DWords,
                       PWords,
                       MessageRef#message_ref{current_offset =
                                                  NewOffset + 1});
follow_tagged_struct_list_pointer(DecodeFun,
                                  PointerInt,
                                  MessageRef =
                                      #message_ref{segments = Segments})
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
    follow_tagged_struct_list_pointer(DecodeFun,
                                      NewPointerInt,
                                      NewMessageRef).

follow_text_pointer(0, _) ->
    undefined;
follow_text_pointer(PointerInt, MessageRef)
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
    Length = PointerInt bsr 35 - 1,
    MessageBits = Length bsl 3,
    <<_:SkipBits,ListData:MessageBits/bitstring,_/bitstring>> =
        MessageRef#message_ref.current_segment,
    ListData;
follow_text_pointer(PointerInt,
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
    follow_text_pointer(NewPointerInt, NewMessageRef).

internal_decode_Annotation(Data = <<Varid:64/little-unsigned-integer>>,
                           Pointers =
                               <<Varvalue:64/little-unsigned-integer,
                                 Varbrand:64/little-unsigned-integer>>,
                           MessageRef) ->
    #'Annotation'{id = Varid,
                  value =
                      follow_struct_pointer(fun internal_decode_Value/3,
                                            Varvalue,
                                            MessageRef#message_ref{current_offset =
                                                                       MessageRef#message_ref.current_offset
                                                                       +
                                                                       0}),
                  brand =
                      follow_struct_pointer(fun internal_decode_Brand/3,
                                            Varbrand,
                                            MessageRef#message_ref{current_offset =
                                                                       MessageRef#message_ref.current_offset
                                                                       +
                                                                       1})};
internal_decode_Annotation(Data, Pointers, MessageRef) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring>> = Data
    end,
    PointerPadLength = 128 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:128/bitstring>> = Pointers
    end,
    internal_decode_Annotation(PaddedData, PaddedPointers, MessageRef).

internal_decode_Brand(Data = <<>>,
                      Pointers =
                          <<Varscopes:64/little-unsigned-integer>>,
                      MessageRef) ->
    #'Brand'{scopes =
                 follow_tagged_struct_list_pointer(fun 'internal_decode_Brand.Scope'/3,
                                                   Varscopes,
                                                   MessageRef#message_ref{current_offset =
                                                                              MessageRef#message_ref.current_offset
                                                                              +
                                                                              0})};
internal_decode_Brand(Data, Pointers, MessageRef) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    internal_decode_Brand(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Brand.Binding'(Data =
                                    <<_:0,
                                      Discriminant:16/little-unsigned-integer,
                                      _:48>>,
                                Pointers = <<_:64>>,
                                MessageRef) ->
    case Discriminant of
        0 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {unbound,undefined};
        1 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {type,
             follow_struct_pointer(fun internal_decode_Type/3,
                                   Var,
                                   MessageRef#message_ref{current_offset =
                                                              MessageRef#message_ref.current_offset
                                                              +
                                                              0})}
    end;
'internal_decode_Brand.Binding'(Data, Pointers, MessageRef) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Brand.Binding'(PaddedData,
                                    PaddedPointers,
                                    MessageRef).

'internal_decode_Brand.Scope'(Data =
                                  <<VarscopeId:64/little-unsigned-integer,
                                    _:64/integer>>,
                              Pointers = <<_:64/integer>>,
                              MessageRef) ->
    #'Brand.Scope'{scopeId = VarscopeId,
                   '' =
                       'internal_decode_Brand.Scope.'(Data,
                                                      Pointers,
                                                      MessageRef)};
'internal_decode_Brand.Scope'(Data, Pointers, MessageRef) ->
    DataPadLength = 128 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:128/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Brand.Scope'(PaddedData,
                                  PaddedPointers,
                                  MessageRef).

'internal_decode_Brand.Scope.'(Data =
                                   <<_:64,
                                     Discriminant:16/little-unsigned-integer,
                                     _:48>>,
                               Pointers = <<_:64>>,
                               MessageRef) ->
    case Discriminant of
        0 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {bind,
             follow_tagged_struct_list_pointer(fun 'internal_decode_Brand.Binding'/3,
                                               Var,
                                               MessageRef#message_ref{current_offset =
                                                                          MessageRef#message_ref.current_offset
                                                                          +
                                                                          0})};
        1 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {inherit,undefined}
    end;
'internal_decode_Brand.Scope.'(Data, Pointers, MessageRef) ->
    DataPadLength = 128 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:128/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Brand.Scope.'(PaddedData,
                                   PaddedPointers,
                                   MessageRef).

internal_decode_CodeGeneratorRequest(Data = <<>>,
                                     Pointers =
                                         <<Varnodes:64/little-unsigned-integer,
                                           VarrequestedFiles:64/little-unsigned-integer>>,
                                     MessageRef) ->
    #'CodeGeneratorRequest'{nodes =
                                follow_tagged_struct_list_pointer(fun internal_decode_Node/3,
                                                                  Varnodes,
                                                                  MessageRef#message_ref{current_offset =
                                                                                             MessageRef#message_ref.current_offset
                                                                                             +
                                                                                             0}),
                            requestedFiles =
                                follow_tagged_struct_list_pointer(fun 'internal_decode_CodeGeneratorRequest.RequestedFile'/3,
                                                                  VarrequestedFiles,
                                                                  MessageRef#message_ref{current_offset =
                                                                                             MessageRef#message_ref.current_offset
                                                                                             +
                                                                                             1})};
internal_decode_CodeGeneratorRequest(Data, Pointers, MessageRef) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring>> = Data
    end,
    PointerPadLength = 128 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:128/bitstring>> = Pointers
    end,
    internal_decode_CodeGeneratorRequest(PaddedData,
                                         PaddedPointers,
                                         MessageRef).

'internal_decode_CodeGeneratorRequest.RequestedFile'(Data =
                                                         <<Varid:64/little-unsigned-integer>>,
                                                     Pointers =
                                                         <<Varfilename:64/little-unsigned-integer,
                                                           Varimports:64/little-unsigned-integer>>,
                                                     MessageRef) ->
    #'CodeGeneratorRequest.RequestedFile'{id = Varid,
                                          filename =
                                              follow_text_pointer(Varfilename,
                                                                  MessageRef#message_ref{current_offset =
                                                                                             MessageRef#message_ref.current_offset
                                                                                             +
                                                                                             0}),
                                          imports =
                                              follow_tagged_struct_list_pointer(fun 'internal_decode_CodeGeneratorRequest.RequestedFile.Import'/3,
                                                                                Varimports,
                                                                                MessageRef#message_ref{current_offset =
                                                                                                           MessageRef#message_ref.current_offset
                                                                                                           +
                                                                                                           1})};
'internal_decode_CodeGeneratorRequest.RequestedFile'(Data,
                                                     Pointers,
                                                     MessageRef) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring>> = Data
    end,
    PointerPadLength = 128 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:128/bitstring>> = Pointers
    end,
    'internal_decode_CodeGeneratorRequest.RequestedFile'(PaddedData,
                                                         PaddedPointers,
                                                         MessageRef).

'internal_decode_CodeGeneratorRequest.RequestedFile.Import'(Data =
                                                                <<Varid:64/little-unsigned-integer>>,
                                                            Pointers =
                                                                <<Varname:64/little-unsigned-integer>>,
                                                            MessageRef) ->
    #'CodeGeneratorRequest.RequestedFile.Import'{id = Varid,
                                                 name =
                                                     follow_text_pointer(Varname,
                                                                         MessageRef#message_ref{current_offset =
                                                                                                    MessageRef#message_ref.current_offset
                                                                                                    +
                                                                                                    0})};
'internal_decode_CodeGeneratorRequest.RequestedFile.Import'(Data,
                                                            Pointers,
                                                            MessageRef) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_CodeGeneratorRequest.RequestedFile.Import'(PaddedData,
                                                                PaddedPointers,
                                                                MessageRef).

internal_decode_Enumerant(Data =
                              <<VarcodeOrder:16/little-unsigned-integer,
                                _:48/integer>>,
                          Pointers =
                              <<Varname:64/little-unsigned-integer,
                                Varannotations:64/little-unsigned-integer>>,
                          MessageRef) ->
    #'Enumerant'{codeOrder = VarcodeOrder,
                 name =
                     follow_text_pointer(Varname,
                                         MessageRef#message_ref{current_offset =
                                                                    MessageRef#message_ref.current_offset
                                                                    +
                                                                    0}),
                 annotations =
                     follow_tagged_struct_list_pointer(fun internal_decode_Annotation/3,
                                                       Varannotations,
                                                       MessageRef#message_ref{current_offset =
                                                                                  MessageRef#message_ref.current_offset
                                                                                  +
                                                                                  1})};
internal_decode_Enumerant(Data, Pointers, MessageRef) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring>> = Data
    end,
    PointerPadLength = 128 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:128/bitstring>> = Pointers
    end,
    internal_decode_Enumerant(PaddedData, PaddedPointers, MessageRef).

internal_decode_Field(Data =
                          <<VarcodeOrder:16/little-unsigned-integer,
                            VardiscriminantValue:16/little-unsigned-integer,
                            _:160/integer>>,
                      Pointers =
                          <<Varname:64/little-unsigned-integer,
                            Varannotations:64/little-unsigned-integer,
                            _:128/integer>>,
                      MessageRef) ->
    #'Field'{codeOrder = VarcodeOrder,
             discriminantValue = VardiscriminantValue bxor 65535,
             name =
                 follow_text_pointer(Varname,
                                     MessageRef#message_ref{current_offset =
                                                                MessageRef#message_ref.current_offset
                                                                +
                                                                0}),
             annotations =
                 follow_tagged_struct_list_pointer(fun internal_decode_Annotation/3,
                                                   Varannotations,
                                                   MessageRef#message_ref{current_offset =
                                                                              MessageRef#message_ref.current_offset
                                                                              +
                                                                              1}),
             '' = 'internal_decode_Field.'(Data, Pointers, MessageRef),
             ordinal =
                 'internal_decode_Field.ordinal'(Data,
                                                 Pointers,
                                                 MessageRef)};
internal_decode_Field(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 256 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:256/bitstring>> = Pointers
    end,
    internal_decode_Field(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Field.'(Data =
                             <<_:64,
                               Discriminant:16/little-unsigned-integer,
                               _:112>>,
                         Pointers = <<_:256>>,
                         MessageRef) ->
    case Discriminant of
        0 ->
            {slot,
             'internal_decode_Field.slot'(Data, Pointers, MessageRef)};
        1 ->
            <<_:128,Var:64/little-unsigned-integer,_/bitstring>> = Data,
            {group,Var}
    end;
'internal_decode_Field.'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 256 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:256/bitstring>> = Pointers
    end,
    'internal_decode_Field.'(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Field.group'(Data =
                                  <<_:128/integer,
                                    VartypeId:64/little-unsigned-integer>>,
                              Pointers = <<_:256/integer>>,
                              MessageRef) ->
    #'Field.group'{typeId = VartypeId};
'internal_decode_Field.group'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 256 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:256/bitstring>> = Pointers
    end,
    'internal_decode_Field.group'(PaddedData,
                                  PaddedPointers,
                                  MessageRef).

'internal_decode_Field.ordinal'(Data =
                                    <<_:80,
                                      Discriminant:16/little-unsigned-integer,
                                      _:96>>,
                                Pointers = <<_:256>>,
                                MessageRef) ->
    case Discriminant of
        0 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {implicit,undefined};
        1 ->
            <<_:96,Var:16/little-unsigned-integer,_/bitstring>> = Data,
            {explicit,Var}
    end;
'internal_decode_Field.ordinal'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 256 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:256/bitstring>> = Pointers
    end,
    'internal_decode_Field.ordinal'(PaddedData,
                                    PaddedPointers,
                                    MessageRef).

'internal_decode_Field.slot'(Data =
                                 <<_:32/integer,
                                   Varoffset:32/little-unsigned-integer,
                                   _:71/integer,
                                   VarhadExplicitDefault:1/integer,
                                   _:56/integer>>,
                             Pointers =
                                 <<_:128/integer,
                                   Vartype:64/little-unsigned-integer,
                                   VardefaultValue:64/little-unsigned-integer>>,
                             MessageRef) ->
    #'Field.slot'{offset = Varoffset,
                  hadExplicitDefault =
                      case VarhadExplicitDefault of
                          0 ->
                              false;
                          1 ->
                              true
                      end,
                  type =
                      follow_struct_pointer(fun internal_decode_Type/3,
                                            Vartype,
                                            MessageRef#message_ref{current_offset =
                                                                       MessageRef#message_ref.current_offset
                                                                       +
                                                                       2}),
                  defaultValue =
                      follow_struct_pointer(fun internal_decode_Value/3,
                                            VardefaultValue,
                                            MessageRef#message_ref{current_offset =
                                                                       MessageRef#message_ref.current_offset
                                                                       +
                                                                       3})};
'internal_decode_Field.slot'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 256 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:256/bitstring>> = Pointers
    end,
    'internal_decode_Field.slot'(PaddedData, PaddedPointers, MessageRef).

internal_decode_Method(Data =
                           <<VarcodeOrder:16/little-unsigned-integer,
                             _:48/integer,
                             VarparamStructType:64/little-unsigned-integer,
                             VarresultStructType:64/little-unsigned-integer>>,
                       Pointers =
                           <<Varname:64/little-unsigned-integer,
                             Varannotations:64/little-unsigned-integer,
                             VarparamBrand:64/little-unsigned-integer,
                             VarresultBrand:64/little-unsigned-integer,
                             VarimplicitParameters:64/little-unsigned-integer>>,
                       MessageRef) ->
    #'Method'{codeOrder = VarcodeOrder,
              paramStructType = VarparamStructType,
              resultStructType = VarresultStructType,
              name =
                  follow_text_pointer(Varname,
                                      MessageRef#message_ref{current_offset =
                                                                 MessageRef#message_ref.current_offset
                                                                 +
                                                                 0}),
              annotations =
                  follow_tagged_struct_list_pointer(fun internal_decode_Annotation/3,
                                                    Varannotations,
                                                    MessageRef#message_ref{current_offset =
                                                                               MessageRef#message_ref.current_offset
                                                                               +
                                                                               1}),
              paramBrand =
                  follow_struct_pointer(fun internal_decode_Brand/3,
                                        VarparamBrand,
                                        MessageRef#message_ref{current_offset =
                                                                   MessageRef#message_ref.current_offset
                                                                   +
                                                                   2}),
              resultBrand =
                  follow_struct_pointer(fun internal_decode_Brand/3,
                                        VarresultBrand,
                                        MessageRef#message_ref{current_offset =
                                                                   MessageRef#message_ref.current_offset
                                                                   +
                                                                   3}),
              implicitParameters =
                  follow_tagged_struct_list_pointer(fun 'internal_decode_Node.Parameter'/3,
                                                    VarimplicitParameters,
                                                    MessageRef#message_ref{current_offset =
                                                                               MessageRef#message_ref.current_offset
                                                                               +
                                                                               4})};
internal_decode_Method(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 320 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:320/bitstring>> = Pointers
    end,
    internal_decode_Method(PaddedData, PaddedPointers, MessageRef).

internal_decode_Node(Data =
                         <<Varid:64/little-unsigned-integer,
                           VardisplayNamePrefixLength:32/little-unsigned-integer,
                           _:32/integer,
                           VarscopeId:64/little-unsigned-integer,
                           _:103/integer,
                           VarisGeneric:1/integer,
                           _:24/integer>>,
                     Pointers =
                         <<VardisplayName:64/little-unsigned-integer,
                           VarnestedNodes:64/little-unsigned-integer,
                           Varannotations:64/little-unsigned-integer,
                           _:128/integer,
                           Varparameters:64/little-unsigned-integer>>,
                     MessageRef) ->
    #'Node'{id = Varid,
            displayNamePrefixLength = VardisplayNamePrefixLength,
            scopeId = VarscopeId,
            isGeneric =
                case VarisGeneric of
                    0 ->
                        false;
                    1 ->
                        true
                end,
            displayName =
                follow_text_pointer(VardisplayName,
                                    MessageRef#message_ref{current_offset =
                                                               MessageRef#message_ref.current_offset
                                                               +
                                                               0}),
            nestedNodes =
                follow_tagged_struct_list_pointer(fun 'internal_decode_Node.NestedNode'/3,
                                                  VarnestedNodes,
                                                  MessageRef#message_ref{current_offset =
                                                                             MessageRef#message_ref.current_offset
                                                                             +
                                                                             1}),
            annotations =
                follow_tagged_struct_list_pointer(fun internal_decode_Annotation/3,
                                                  Varannotations,
                                                  MessageRef#message_ref{current_offset =
                                                                             MessageRef#message_ref.current_offset
                                                                             +
                                                                             2}),
            parameters =
                follow_tagged_struct_list_pointer(fun 'internal_decode_Node.Parameter'/3,
                                                  Varparameters,
                                                  MessageRef#message_ref{current_offset =
                                                                             MessageRef#message_ref.current_offset
                                                                             +
                                                                             5}),
            '' = 'internal_decode_Node.'(Data, Pointers, MessageRef)};
internal_decode_Node(Data, Pointers, MessageRef) ->
    DataPadLength = 320 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:320/bitstring>> = Data
    end,
    PointerPadLength = 384 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:384/bitstring>> = Pointers
    end,
    internal_decode_Node(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Node.'(Data =
                            <<_:96,
                              Discriminant:16/little-unsigned-integer,
                              _:208>>,
                        Pointers = <<_:384>>,
                        MessageRef) ->
    case Discriminant of
        0 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {file,undefined};
        1 ->
            {struct,
             'internal_decode_Node.struct'(Data, Pointers, MessageRef)};
        2 ->
            <<_:192,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {enum,
             follow_tagged_struct_list_pointer(fun internal_decode_Enumerant/3,
                                               Var,
                                               MessageRef#message_ref{current_offset =
                                                                          MessageRef#message_ref.current_offset
                                                                          +
                                                                          3})};
        3 ->
            {interface,
             'internal_decode_Node.interface'(Data,
                                              Pointers,
                                              MessageRef)};
        4 ->
            {const,
             'internal_decode_Node.const'(Data, Pointers, MessageRef)};
        5 ->
            {annotation,
             'internal_decode_Node.annotation'(Data,
                                               Pointers,
                                               MessageRef)}
    end;
'internal_decode_Node.'(Data, Pointers, MessageRef) ->
    DataPadLength = 320 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:320/bitstring>> = Data
    end,
    PointerPadLength = 384 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:384/bitstring>> = Pointers
    end,
    'internal_decode_Node.'(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Node.NestedNode'(Data =
                                      <<Varid:64/little-unsigned-integer>>,
                                  Pointers =
                                      <<Varname:64/little-unsigned-integer>>,
                                  MessageRef) ->
    #'Node.NestedNode'{id = Varid,
                       name =
                           follow_text_pointer(Varname,
                                               MessageRef#message_ref{current_offset =
                                                                          MessageRef#message_ref.current_offset
                                                                          +
                                                                          0})};
'internal_decode_Node.NestedNode'(Data, Pointers, MessageRef) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Node.NestedNode'(PaddedData,
                                      PaddedPointers,
                                      MessageRef).

'internal_decode_Node.Parameter'(Data = <<>>,
                                 Pointers =
                                     <<Varname:64/little-unsigned-integer>>,
                                 MessageRef) ->
    #'Node.Parameter'{name =
                          follow_text_pointer(Varname,
                                              MessageRef#message_ref{current_offset =
                                                                         MessageRef#message_ref.current_offset
                                                                         +
                                                                         0})};
'internal_decode_Node.Parameter'(Data, Pointers, MessageRef) ->
    DataPadLength = 0 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:0/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Node.Parameter'(PaddedData,
                                     PaddedPointers,
                                     MessageRef).

'internal_decode_Node.annotation'(Data =
                                      <<_:112/integer,
                                        VartargetsGroup:1/integer,
                                        VartargetsUnion:1/integer,
                                        VartargetsField:1/integer,
                                        VartargetsStruct:1/integer,
                                        VartargetsEnumerant:1/integer,
                                        VartargetsEnum:1/integer,
                                        VartargetsConst:1/integer,
                                        VartargetsFile:1/integer,
                                        _:4/integer,
                                        VartargetsAnnotation:1/integer,
                                        VartargetsParam:1/integer,
                                        VartargetsMethod:1/integer,
                                        VartargetsInterface:1/integer,
                                        _:192/integer>>,
                                  Pointers =
                                      <<_:192/integer,
                                        Vartype:64/little-unsigned-integer,
                                        _:128/integer>>,
                                  MessageRef) ->
    #'Node.annotation'{targetsGroup =
                           case VartargetsGroup of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsUnion =
                           case VartargetsUnion of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsField =
                           case VartargetsField of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsStruct =
                           case VartargetsStruct of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsEnumerant =
                           case VartargetsEnumerant of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsEnum =
                           case VartargetsEnum of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsConst =
                           case VartargetsConst of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsFile =
                           case VartargetsFile of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsAnnotation =
                           case VartargetsAnnotation of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsParam =
                           case VartargetsParam of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsMethod =
                           case VartargetsMethod of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       targetsInterface =
                           case VartargetsInterface of
                               0 ->
                                   false;
                               1 ->
                                   true
                           end,
                       type =
                           follow_struct_pointer(fun internal_decode_Type/3,
                                                 Vartype,
                                                 MessageRef#message_ref{current_offset =
                                                                            MessageRef#message_ref.current_offset
                                                                            +
                                                                            3})};
'internal_decode_Node.annotation'(Data, Pointers, MessageRef) ->
    DataPadLength = 320 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:320/bitstring>> = Data
    end,
    PointerPadLength = 384 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:384/bitstring>> = Pointers
    end,
    'internal_decode_Node.annotation'(PaddedData,
                                      PaddedPointers,
                                      MessageRef).

'internal_decode_Node.const'(Data = <<_:320/integer>>,
                             Pointers =
                                 <<_:192/integer,
                                   Vartype:64/little-unsigned-integer,
                                   Varvalue:64/little-unsigned-integer,
                                   _:64/integer>>,
                             MessageRef) ->
    #'Node.const'{type =
                      follow_struct_pointer(fun internal_decode_Type/3,
                                            Vartype,
                                            MessageRef#message_ref{current_offset =
                                                                       MessageRef#message_ref.current_offset
                                                                       +
                                                                       3}),
                  value =
                      follow_struct_pointer(fun internal_decode_Value/3,
                                            Varvalue,
                                            MessageRef#message_ref{current_offset =
                                                                       MessageRef#message_ref.current_offset
                                                                       +
                                                                       4})};
'internal_decode_Node.const'(Data, Pointers, MessageRef) ->
    DataPadLength = 320 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:320/bitstring>> = Data
    end,
    PointerPadLength = 384 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:384/bitstring>> = Pointers
    end,
    'internal_decode_Node.const'(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Node.enum'(Data = <<_:320/integer>>,
                            Pointers =
                                <<_:192/integer,
                                  Varenumerants:64/little-unsigned-integer,
                                  _:128/integer>>,
                            MessageRef) ->
    #'Node.enum'{enumerants =
                     follow_tagged_struct_list_pointer(fun internal_decode_Enumerant/3,
                                                       Varenumerants,
                                                       MessageRef#message_ref{current_offset =
                                                                                  MessageRef#message_ref.current_offset
                                                                                  +
                                                                                  3})};
'internal_decode_Node.enum'(Data, Pointers, MessageRef) ->
    DataPadLength = 320 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:320/bitstring>> = Data
    end,
    PointerPadLength = 384 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:384/bitstring>> = Pointers
    end,
    'internal_decode_Node.enum'(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Node.interface'(Data = <<_:320/integer>>,
                                 Pointers =
                                     <<_:192/integer,
                                       Varmethods:64/little-unsigned-integer,
                                       Varsuperclasses:64/little-unsigned-integer,
                                       _:64/integer>>,
                                 MessageRef) ->
    #'Node.interface'{methods =
                          follow_tagged_struct_list_pointer(fun internal_decode_Method/3,
                                                            Varmethods,
                                                            MessageRef#message_ref{current_offset =
                                                                                       MessageRef#message_ref.current_offset
                                                                                       +
                                                                                       3}),
                      superclasses =
                          follow_tagged_struct_list_pointer(fun internal_decode_Superclass/3,
                                                            Varsuperclasses,
                                                            MessageRef#message_ref{current_offset =
                                                                                       MessageRef#message_ref.current_offset
                                                                                       +
                                                                                       4})};
'internal_decode_Node.interface'(Data, Pointers, MessageRef) ->
    DataPadLength = 320 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:320/bitstring>> = Data
    end,
    PointerPadLength = 384 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:384/bitstring>> = Pointers
    end,
    'internal_decode_Node.interface'(PaddedData,
                                     PaddedPointers,
                                     MessageRef).

'internal_decode_Node.struct'(Data =
                                  <<_:112/integer,
                                    VardataWordCount:16/little-unsigned-integer,
                                    _:64/integer,
                                    VarpointerCount:16/little-unsigned-integer,
                                    VarpreferredListEncoding:16/little-unsigned-integer,
                                    _:7/integer,
                                    VarisGroup:1/integer,
                                    _:8/integer,
                                    VardiscriminantCount:16/little-unsigned-integer,
                                    VardiscriminantOffset:32/little-unsigned-integer,
                                    _:32/integer>>,
                              Pointers =
                                  <<_:192/integer,
                                    Varfields:64/little-unsigned-integer,
                                    _:128/integer>>,
                              MessageRef) ->
    #'Node.struct'{dataWordCount = VardataWordCount,
                   pointerCount = VarpointerCount,
                   preferredListEncoding =
                       element(VarpreferredListEncoding + 1,
                               {empty,
                                bit,
                                byte,
                                twoBytes,
                                fourBytes,
                                eightBytes,
                                pointer,
                                inlineComposite}),
                   isGroup =
                       case VarisGroup of
                           0 ->
                               false;
                           1 ->
                               true
                       end,
                   discriminantCount = VardiscriminantCount,
                   discriminantOffset = VardiscriminantOffset,
                   fields =
                       follow_tagged_struct_list_pointer(fun internal_decode_Field/3,
                                                         Varfields,
                                                         MessageRef#message_ref{current_offset =
                                                                                    MessageRef#message_ref.current_offset
                                                                                    +
                                                                                    3})};
'internal_decode_Node.struct'(Data, Pointers, MessageRef) ->
    DataPadLength = 320 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:320/bitstring>> = Data
    end,
    PointerPadLength = 384 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:384/bitstring>> = Pointers
    end,
    'internal_decode_Node.struct'(PaddedData,
                                  PaddedPointers,
                                  MessageRef).

internal_decode_Superclass(Data = <<Varid:64/little-unsigned-integer>>,
                           Pointers =
                               <<Varbrand:64/little-unsigned-integer>>,
                           MessageRef) ->
    #'Superclass'{id = Varid,
                  brand =
                      follow_struct_pointer(fun internal_decode_Brand/3,
                                            Varbrand,
                                            MessageRef#message_ref{current_offset =
                                                                       MessageRef#message_ref.current_offset
                                                                       +
                                                                       0})};
internal_decode_Superclass(Data, Pointers, MessageRef) ->
    DataPadLength = 64 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:64/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    internal_decode_Superclass(PaddedData, PaddedPointers, MessageRef).

internal_decode_Type(Data =
                         <<_:0,
                           Discriminant:16/little-unsigned-integer,
                           _:176>>,
                     Pointers = <<_:64>>,
                     MessageRef) ->
    case Discriminant of
        0 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {void,undefined};
        1 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {bool,undefined};
        2 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {int8,undefined};
        3 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {int16,undefined};
        4 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {int32,undefined};
        5 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {int64,undefined};
        6 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {uint8,undefined};
        7 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {uint16,undefined};
        8 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {uint32,undefined};
        9 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {uint64,undefined};
        10 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {float32,undefined};
        11 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {float64,undefined};
        12 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {text,undefined};
        13 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {data,undefined};
        14 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {list,
             follow_struct_pointer(fun internal_decode_Type/3,
                                   Var,
                                   MessageRef#message_ref{current_offset =
                                                              MessageRef#message_ref.current_offset
                                                              +
                                                              0})};
        15 ->
            {enum,
             'internal_decode_Type.enum'(Data, Pointers, MessageRef)};
        16 ->
            {struct,
             'internal_decode_Type.struct'(Data, Pointers, MessageRef)};
        17 ->
            {interface,
             'internal_decode_Type.interface'(Data,
                                              Pointers,
                                              MessageRef)};
        18 ->
            {anyPointer,
             'internal_decode_Type.anyPointer'(Data,
                                               Pointers,
                                               MessageRef)}
    end;
internal_decode_Type(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    internal_decode_Type(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Type.anyPointer'(Data =
                                      <<_:64,
                                        Discriminant:16/little-unsigned-integer,
                                        _:112>>,
                                  Pointers = <<_:64>>,
                                  MessageRef) ->
    case Discriminant of
        0 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {unconstrained,undefined};
        1 ->
            {parameter,
             'internal_decode_Type.anyPointer.parameter'(Data,
                                                         Pointers,
                                                         MessageRef)};
        2 ->
            <<_:80,Var:16/little-unsigned-integer,_/bitstring>> = Data,
            {implicitMethodParameter,Var}
    end;
'internal_decode_Type.anyPointer'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Type.anyPointer'(PaddedData,
                                      PaddedPointers,
                                      MessageRef).

'internal_decode_Type.anyPointer.implicitMethodParameter'(Data =
                                                              <<_:80/integer,
                                                                VarparameterIndex:16/little-unsigned-integer,
                                                                _:96/integer>>,
                                                          Pointers =
                                                              <<_:64/integer>>,
                                                          MessageRef) ->
    #'Type.anyPointer.implicitMethodParameter'{parameterIndex =
                                                   VarparameterIndex};
'internal_decode_Type.anyPointer.implicitMethodParameter'(Data,
                                                          Pointers,
                                                          MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Type.anyPointer.implicitMethodParameter'(PaddedData,
                                                              PaddedPointers,
                                                              MessageRef).

'internal_decode_Type.anyPointer.parameter'(Data =
                                                <<_:80/integer,
                                                  VarparameterIndex:16/little-unsigned-integer,
                                                  _:32/integer,
                                                  VarscopeId:64/little-unsigned-integer>>,
                                            Pointers = <<_:64/integer>>,
                                            MessageRef) ->
    #'Type.anyPointer.parameter'{parameterIndex = VarparameterIndex,
                                 scopeId = VarscopeId};
'internal_decode_Type.anyPointer.parameter'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Type.anyPointer.parameter'(PaddedData,
                                                PaddedPointers,
                                                MessageRef).

'internal_decode_Type.enum'(Data =
                                <<_:64/integer,
                                  VartypeId:64/little-unsigned-integer,
                                  _:64/integer>>,
                            Pointers =
                                <<Varbrand:64/little-unsigned-integer>>,
                            MessageRef) ->
    #'Type.enum'{typeId = VartypeId,
                 brand =
                     follow_struct_pointer(fun internal_decode_Brand/3,
                                           Varbrand,
                                           MessageRef#message_ref{current_offset =
                                                                      MessageRef#message_ref.current_offset
                                                                      +
                                                                      0})};
'internal_decode_Type.enum'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Type.enum'(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Type.interface'(Data =
                                     <<_:64/integer,
                                       VartypeId:64/little-unsigned-integer,
                                       _:64/integer>>,
                                 Pointers =
                                     <<Varbrand:64/little-unsigned-integer>>,
                                 MessageRef) ->
    #'Type.interface'{typeId = VartypeId,
                      brand =
                          follow_struct_pointer(fun internal_decode_Brand/3,
                                                Varbrand,
                                                MessageRef#message_ref{current_offset =
                                                                           MessageRef#message_ref.current_offset
                                                                           +
                                                                           0})};
'internal_decode_Type.interface'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Type.interface'(PaddedData,
                                     PaddedPointers,
                                     MessageRef).

'internal_decode_Type.list'(Data = <<_:192/integer>>,
                            Pointers =
                                <<VarelementType:64/little-unsigned-integer>>,
                            MessageRef) ->
    #'Type.list'{elementType =
                     follow_struct_pointer(fun internal_decode_Type/3,
                                           VarelementType,
                                           MessageRef#message_ref{current_offset =
                                                                      MessageRef#message_ref.current_offset
                                                                      +
                                                                      0})};
'internal_decode_Type.list'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Type.list'(PaddedData, PaddedPointers, MessageRef).

'internal_decode_Type.struct'(Data =
                                  <<_:64/integer,
                                    VartypeId:64/little-unsigned-integer,
                                    _:64/integer>>,
                              Pointers =
                                  <<Varbrand:64/little-unsigned-integer>>,
                              MessageRef) ->
    #'Type.struct'{typeId = VartypeId,
                   brand =
                       follow_struct_pointer(fun internal_decode_Brand/3,
                                             Varbrand,
                                             MessageRef#message_ref{current_offset =
                                                                        MessageRef#message_ref.current_offset
                                                                        +
                                                                        0})};
'internal_decode_Type.struct'(Data, Pointers, MessageRef) ->
    DataPadLength = 192 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:192/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    'internal_decode_Type.struct'(PaddedData,
                                  PaddedPointers,
                                  MessageRef).

internal_decode_Value(Data =
                          <<_:0,
                            Discriminant:16/little-unsigned-integer,
                            _:112>>,
                      Pointers = <<_:64>>,
                      MessageRef) ->
    case Discriminant of
        0 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {void,undefined};
        1 ->
            <<_:23,Var:1/integer,_/bitstring>> = Data,
            {bool,
             case Var of
                 0 ->
                     false;
                 1 ->
                     true
             end};
        2 ->
            <<_:16,Var:8/little-signed-integer,_/bitstring>> = Data,
            {int8,Var};
        3 ->
            <<_:16,Var:16/little-signed-integer,_/bitstring>> = Data,
            {int16,Var};
        4 ->
            <<_:32,Var:32/little-signed-integer,_/bitstring>> = Data,
            {int32,Var};
        5 ->
            <<_:64,Var:64/little-signed-integer,_/bitstring>> = Data,
            {int64,Var};
        6 ->
            <<_:16,Var:8/little-unsigned-integer,_/bitstring>> = Data,
            {uint8,Var};
        7 ->
            <<_:16,Var:16/little-unsigned-integer,_/bitstring>> = Data,
            {uint16,Var};
        8 ->
            <<_:32,Var:32/little-unsigned-integer,_/bitstring>> = Data,
            {uint32,Var};
        9 ->
            <<_:64,Var:64/little-unsigned-integer,_/bitstring>> = Data,
            {uint64,Var};
        10 ->
            <<_:32,Var:32/little-float,_/bitstring>> = Data,
            {float32,Var};
        11 ->
            <<_:64,Var:64/little-float,_/bitstring>> = Data,
            {float64,Var};
        12 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {text,
             follow_text_pointer(Var,
                                 MessageRef#message_ref{current_offset =
                                                            MessageRef#message_ref.current_offset
                                                            +
                                                            0})};
        13 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {data,
             follow_data_pointer(Var,
                                 MessageRef#message_ref{current_offset =
                                                            MessageRef#message_ref.current_offset
                                                            +
                                                            0})};
        14 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {list,not_implemented};
        15 ->
            <<_:16,Var:16/little-unsigned-integer,_/bitstring>> = Data,
            {enum,Var};
        16 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {struct,not_implemented};
        17 ->
            <<_:0,Var:0/integer,_/bitstring>> = Data,
            {interface,undefined};
        18 ->
            <<_:0,Var:64/little-unsigned-integer,_/bitstring>> =
                Pointers,
            {anyPointer,not_implemented}
    end;
internal_decode_Value(Data, Pointers, MessageRef) ->
    DataPadLength = 128 - bit_size(Data),
    if
        DataPadLength > 0 ->
            PaddedData = <<Data/binary,0:DataPadLength>>;
        DataPadLength =:= 0 ->
            PaddedData = Data;
        DataPadLength < 0 ->
            <<PaddedData:128/bitstring>> = Data
    end,
    PointerPadLength = 64 - bit_size(Pointers),
    if
        PointerPadLength > 0 ->
            PaddedPointers = <<Pointers/binary,0:PointerPadLength>>;
        PointerPadLength =:= 0 ->
            PaddedPointers = Pointers;
        PointerPadLength < 0 ->
            <<PaddedPointers:64/bitstring>> = Pointers
    end,
    internal_decode_Value(PaddedData, PaddedPointers, MessageRef).



