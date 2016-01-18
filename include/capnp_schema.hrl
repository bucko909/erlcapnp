-file("capnp_schema.hrl", 1).

-record('Annotation',{id :: 0..18446744073709551615,
                      value ::
                          {void, undefined} |
                          {bool, true | false} |
                          {int8, -128..127} |
                          {int16, -32768..32767} |
                          {int32, -2147483648..2147483647} |
                          {int64,
                           -9223372036854775808..9223372036854775807} |
                          {uint8, 0..255} |
                          {uint16, 0..65535} |
                          {uint32, 0..4294967295} |
                          {uint64, 0..18446744073709551615} |
                          {float32, float()} |
                          {float64, float()} |
                          {text, undefined | iodata()} |
                          {data, undefined | iodata()} |
                          {list, undefined} |
                          {enum, 0..65535} |
                          {struct, undefined} |
                          {interface, undefined} |
                          {anyPointer, undefined},
                      brand :: any()}).

-record('Brand',{scopes :: any()}).

-record('Brand_Scope',{scopeId :: 0..18446744073709551615,
                       '' :: {bind, any()} | {inherit, undefined}}).

-record('CodeGeneratorRequest',{nodes :: any(),requestedFiles :: any()}).

-record('CodeGeneratorRequest_RequestedFile',{id ::
                                                  0..
                                                  18446744073709551615,
                                              filename ::
                                                  undefined | iodata(),
                                              imports :: any()}).

-record('CodeGeneratorRequest_RequestedFile_Import',{id ::
                                                         0..
                                                         18446744073709551615,
                                                     name ::
                                                         undefined |
                                                         iodata()}).

-record('Enumerant',{codeOrder :: 0..65535,
                     name :: undefined | iodata(),
                     annotations :: any()}).

-record('Field',{codeOrder :: 0..65535,
                 discriminantValue :: 0..65535,
                 name :: undefined | iodata(),
                 annotations :: any(),
                 '' :: {slot, any()} | {group, 0..18446744073709551615},
                 ordinal :: {implicit, undefined} | {explicit, 0..65535}}).

-record('Field_group',{typeId :: 0..18446744073709551615}).

-record('Field_slot',{offset :: 0..4294967295,
                      hadExplicitDefault :: true | false,
                      type ::
                          {void, undefined} |
                          {bool, undefined} |
                          {int8, undefined} |
                          {int16, undefined} |
                          {int32, undefined} |
                          {int64, undefined} |
                          {uint8, undefined} |
                          {uint16, undefined} |
                          {uint32, undefined} |
                          {uint64, undefined} |
                          {float32, undefined} |
                          {float64, undefined} |
                          {text, undefined} |
                          {data, undefined} |
                          {list, any()} |
                          {enum, any()} |
                          {struct, any()} |
                          {interface, any()} |
                          {anyPointer,
                           {unconstrained, undefined} |
                           {parameter, any()} |
                           {implicitMethodParameter, 0..65535}},
                      defaultValue ::
                          {void, undefined} |
                          {bool, true | false} |
                          {int8, -128..127} |
                          {int16, -32768..32767} |
                          {int32, -2147483648..2147483647} |
                          {int64,
                           -9223372036854775808..9223372036854775807} |
                          {uint8, 0..255} |
                          {uint16, 0..65535} |
                          {uint32, 0..4294967295} |
                          {uint64, 0..18446744073709551615} |
                          {float32, float()} |
                          {float64, float()} |
                          {text, undefined | iodata()} |
                          {data, undefined | iodata()} |
                          {list, undefined} |
                          {enum, 0..65535} |
                          {struct, undefined} |
                          {interface, undefined} |
                          {anyPointer, undefined}}).

-record('Method',{codeOrder :: 0..65535,
                  paramStructType :: 0..18446744073709551615,
                  resultStructType :: 0..18446744073709551615,
                  name :: undefined | iodata(),
                  annotations :: any(),
                  paramBrand :: any(),
                  resultBrand :: any(),
                  implicitParameters :: any()}).

-record('Node',{id :: 0..18446744073709551615,
                displayNamePrefixLength :: 0..4294967295,
                scopeId :: 0..18446744073709551615,
                isGeneric :: true | false,
                displayName :: undefined | iodata(),
                nestedNodes :: any(),
                annotations :: any(),
                parameters :: any(),
                '' ::
                    {file, undefined} |
                    {struct, any()} |
                    {enum, any()} |
                    {interface, any()} |
                    {const, any()} |
                    {annotation, any()}}).

-record('Node_NestedNode',{id :: 0..18446744073709551615,
                           name :: undefined | iodata()}).

-record('Node_Parameter',{name :: undefined | iodata()}).

-record('Node_annotation',{targetsGroup :: true | false,
                           targetsUnion :: true | false,
                           targetsField :: true | false,
                           targetsStruct :: true | false,
                           targetsEnumerant :: true | false,
                           targetsEnum :: true | false,
                           targetsConst :: true | false,
                           targetsFile :: true | false,
                           targetsAnnotation :: true | false,
                           targetsParam :: true | false,
                           targetsMethod :: true | false,
                           targetsInterface :: true | false,
                           type ::
                               {void, undefined} |
                               {bool, undefined} |
                               {int8, undefined} |
                               {int16, undefined} |
                               {int32, undefined} |
                               {int64, undefined} |
                               {uint8, undefined} |
                               {uint16, undefined} |
                               {uint32, undefined} |
                               {uint64, undefined} |
                               {float32, undefined} |
                               {float64, undefined} |
                               {text, undefined} |
                               {data, undefined} |
                               {list, any()} |
                               {enum, any()} |
                               {struct, any()} |
                               {interface, any()} |
                               {anyPointer,
                                {unconstrained, undefined} |
                                {parameter, any()} |
                                {implicitMethodParameter, 0..65535}}}).

-record('Node_const',{type ::
                          {void, undefined} |
                          {bool, undefined} |
                          {int8, undefined} |
                          {int16, undefined} |
                          {int32, undefined} |
                          {int64, undefined} |
                          {uint8, undefined} |
                          {uint16, undefined} |
                          {uint32, undefined} |
                          {uint64, undefined} |
                          {float32, undefined} |
                          {float64, undefined} |
                          {text, undefined} |
                          {data, undefined} |
                          {list, any()} |
                          {enum, any()} |
                          {struct, any()} |
                          {interface, any()} |
                          {anyPointer,
                           {unconstrained, undefined} |
                           {parameter, any()} |
                           {implicitMethodParameter, 0..65535}},
                      value ::
                          {void, undefined} |
                          {bool, true | false} |
                          {int8, -128..127} |
                          {int16, -32768..32767} |
                          {int32, -2147483648..2147483647} |
                          {int64,
                           -9223372036854775808..9223372036854775807} |
                          {uint8, 0..255} |
                          {uint16, 0..65535} |
                          {uint32, 0..4294967295} |
                          {uint64, 0..18446744073709551615} |
                          {float32, float()} |
                          {float64, float()} |
                          {text, undefined | iodata()} |
                          {data, undefined | iodata()} |
                          {list, undefined} |
                          {enum, 0..65535} |
                          {struct, undefined} |
                          {interface, undefined} |
                          {anyPointer, undefined}}).

-record('Node_enum',{enumerants :: any()}).

-record('Node_interface',{methods :: any(),superclasses :: any()}).

-record('Node_struct',{dataWordCount :: 0..65535,
                       pointerCount :: 0..65535,
                       preferredListEncoding ::
                           empty |
                           bit |
                           byte |
                           twoBytes |
                           fourBytes |
                           eightBytes |
                           pointer |
                           inlineComposite,
                       isGroup :: true | false,
                       discriminantCount :: 0..65535,
                       discriminantOffset :: 0..4294967295,
                       fields :: any()}).

-record('Superclass',{id :: 0..18446744073709551615,brand :: any()}).

-record('Type_anyPointer_implicitMethodParameter',{parameterIndex ::
                                                       0..65535}).

-record('Type_anyPointer_parameter',{parameterIndex :: 0..65535,
                                     scopeId :: 0..18446744073709551615}).

-record('Type_enum',{typeId :: 0..18446744073709551615,brand :: any()}).

-record('Type_interface',{typeId :: 0..18446744073709551615,
                          brand :: any()}).

-record('Type_list',{elementType ::
                         {void, undefined} |
                         {bool, undefined} |
                         {int8, undefined} |
                         {int16, undefined} |
                         {int32, undefined} |
                         {int64, undefined} |
                         {uint8, undefined} |
                         {uint16, undefined} |
                         {uint32, undefined} |
                         {uint64, undefined} |
                         {float32, undefined} |
                         {float64, undefined} |
                         {text, undefined} |
                         {data, undefined} |
                         {list, any()} |
                         {enum, any()} |
                         {struct, any()} |
                         {interface, any()} |
                         {anyPointer,
                          {unconstrained, undefined} |
                          {parameter, any()} |
                          {implicitMethodParameter, 0..65535}}}).

-record('Type_struct',{typeId :: 0..18446744073709551615,brand :: any()}).

-record(message_ref,{current_offset,current_segment,segments}).



