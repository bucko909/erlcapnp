-file("test1.capnp", 1).

%% This file was generated 2016-12-04 21:44:14 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(test1_capnp).

-vsn(13525483159656916942).

-export([schema/1, 'TestBoringInteger'/0, 'TestBoringInteger'/1, '14973849609597724955'/0,
	 'TestMultipleIntegers'/0, 'TestMultipleIntegers'/1, '14598953150269889741'/0, 'TestBoringPointer'/0,
	 'TestBoringPointer'/1, '16759011188907251718'/0, 'TestLessBoringPointer'/0,
	 'TestLessBoringPointer'/1, '15357896496141030843'/0, 'TestTextType'/0, 'TestTextType'/1,
	 '11602115113996880377'/0, 'TestTextList'/0, 'TestTextList'/1, '18323824905083102260'/0,
	 'SimpleEnum'/0, 'SimpleEnum'/1, '16172797807139408828'/0, 'TestEnum'/0, 'TestEnum'/1,
	 '17210050650163916943'/0, 'TestPrimitiveList'/0, 'TestPrimitiveList'/1, '17102922043347402444'/0,
	 'TestEnumList'/0, 'TestEnumList'/1, '10339243589925359431'/0, 'SimpleShortStruct'/0,
	 'SimpleShortStruct'/1, '14043516564278224224'/0, 'TestShortList'/0, 'TestShortList'/1,
	 '13983806467864851022'/0, 'TestPointerList'/0, 'TestPointerList'/1, '17944740380774124201'/0,
	 'TestCompositeList'/0, 'TestCompositeList'/1, '12428803461894409050'/0, 'TestDefaults'/0,
	 'TestDefaults'/1, '9407938069320387404'/0, 'TestUnion'/0, 'TestUnion'/1, '11761371100030074809'/0,
	 'TestGroup'/0, 'TestGroup'/1, '17124799971340330905'/0, '10175146911558660158'/0,
	 'TestGroupInUnion'/0, 'TestGroupInUnion'/1, '16202860960915816089'/0, '10759877427197301733'/0,
	 '13292494231364804825'/0, root/0, root/1, '13525483159656916942'/0]).

-types([{14973849609597724955, 'TestBoringInteger'}, {14598953150269889741, 'TestMultipleIntegers'},
	{16759011188907251718, 'TestBoringPointer'}, {15357896496141030843, 'TestLessBoringPointer'},
	{11602115113996880377, 'TestTextType'}, {18323824905083102260, 'TestTextList'},
	{16172797807139408828, 'SimpleEnum'}, {17210050650163916943, 'TestEnum'},
	{17102922043347402444, 'TestPrimitiveList'}, {10339243589925359431, 'TestEnumList'},
	{14043516564278224224, 'SimpleShortStruct'}, {13983806467864851022, 'TestShortList'},
	{17944740380774124201, 'TestPointerList'}, {12428803461894409050, 'TestCompositeList'},
	{9407938069320387404, 'TestDefaults'}, {11761371100030074809, 'TestUnion'},
	{17124799971340330905, 'TestGroup'}, {10175146911558660158, ['TestGroup', group1]},
	{16202860960915816089, 'TestGroupInUnion'}, {10759877427197301733, ['TestGroupInUnion', union2]},
	{13292494231364804825, ['TestGroupInUnion', unionVar1]}, {13525483159656916942, root}]).

-file("../include/ecapnp_schema.hrl", 1).

-ecapnp_schema_version(4).

-record(schema_node,
	{module  :: atom(), name  :: ecapnp:type_name(), id = 0  :: ecapnp:type_id(),
	 src = <<>>  :: ecapnp:text(), kind = file  :: ecapnp:schema_kind(), annotations = []  :: list(),
	 nodes = []  :: ecapnp:schema_nodes(), scope = 0  :: ecapnp:type_id()}).

-record(struct,
	{dsize = 0  :: ecapnp:word_count(), psize = 0  :: ecapnp:ptr_count(),
	 esize = inlineComposite  :: ecapnp:element_size(),
	 union_field = none  :: none | ecapnp:field_type(), fields = []  :: ecapnp:struct_fields()}).

-record(enum, {values = []  :: ecapnp:enum_values()}).

-record(interface, {extends = []  :: list(), methods = []  :: list()}).

-record(const, {field}).

-record(annotation, {type, targets = []  :: [atom()]}).

-record(field, {id, name, kind, annotations = []}).

-record(ptr,
	{type  :: term(), idx = 0  :: ecapnp:ptr_index(),
	 default = <<0:64/integer-little>>  :: ecapnp:value()}).

-record(data, {type  :: term(), align = 0  :: ecapnp:bit_count(), default  :: ecapnp:value()}).

-record(group, {id = 0  :: ecapnp:type_id()}).

-record(method, {id, name, paramType, resultType}).

-file("test1.capnp", 1).

schema(14973849609597724955) -> '14973849609597724955'();
schema('TestBoringInteger') -> '14973849609597724955'();
schema(['TestBoringInteger']) -> '14973849609597724955'();
schema(14598953150269889741) -> '14598953150269889741'();
schema('TestMultipleIntegers') -> '14598953150269889741'();
schema(['TestMultipleIntegers']) -> '14598953150269889741'();
schema(16759011188907251718) -> '16759011188907251718'();
schema('TestBoringPointer') -> '16759011188907251718'();
schema(['TestBoringPointer']) -> '16759011188907251718'();
schema(15357896496141030843) -> '15357896496141030843'();
schema('TestLessBoringPointer') -> '15357896496141030843'();
schema(['TestLessBoringPointer']) -> '15357896496141030843'();
schema(11602115113996880377) -> '11602115113996880377'();
schema('TestTextType') -> '11602115113996880377'();
schema(['TestTextType']) -> '11602115113996880377'();
schema(18323824905083102260) -> '18323824905083102260'();
schema('TestTextList') -> '18323824905083102260'();
schema(['TestTextList']) -> '18323824905083102260'();
schema(16172797807139408828) -> '16172797807139408828'();
schema('SimpleEnum') -> '16172797807139408828'();
schema(['SimpleEnum']) -> '16172797807139408828'();
schema(17210050650163916943) -> '17210050650163916943'();
schema('TestEnum') -> '17210050650163916943'();
schema(['TestEnum']) -> '17210050650163916943'();
schema(17102922043347402444) -> '17102922043347402444'();
schema('TestPrimitiveList') -> '17102922043347402444'();
schema(['TestPrimitiveList']) -> '17102922043347402444'();
schema(10339243589925359431) -> '10339243589925359431'();
schema('TestEnumList') -> '10339243589925359431'();
schema(['TestEnumList']) -> '10339243589925359431'();
schema(14043516564278224224) -> '14043516564278224224'();
schema('SimpleShortStruct') -> '14043516564278224224'();
schema(['SimpleShortStruct']) -> '14043516564278224224'();
schema(13983806467864851022) -> '13983806467864851022'();
schema('TestShortList') -> '13983806467864851022'();
schema(['TestShortList']) -> '13983806467864851022'();
schema(17944740380774124201) -> '17944740380774124201'();
schema('TestPointerList') -> '17944740380774124201'();
schema(['TestPointerList']) -> '17944740380774124201'();
schema(12428803461894409050) -> '12428803461894409050'();
schema('TestCompositeList') -> '12428803461894409050'();
schema(['TestCompositeList']) -> '12428803461894409050'();
schema(9407938069320387404) -> '9407938069320387404'();
schema('TestDefaults') -> '9407938069320387404'();
schema(['TestDefaults']) -> '9407938069320387404'();
schema(11761371100030074809) -> '11761371100030074809'();
schema('TestUnion') -> '11761371100030074809'();
schema(['TestUnion']) -> '11761371100030074809'();
schema(17124799971340330905) -> '17124799971340330905'();
schema('TestGroup') -> '17124799971340330905'();
schema(['TestGroup']) -> '17124799971340330905'();
schema(10175146911558660158) -> '10175146911558660158'();
schema(['TestGroup', group1]) -> '10175146911558660158'();
schema(16202860960915816089) -> '16202860960915816089'();
schema('TestGroupInUnion') -> '16202860960915816089'();
schema(['TestGroupInUnion']) -> '16202860960915816089'();
schema(10759877427197301733) -> '10759877427197301733'();
schema(['TestGroupInUnion', union2]) -> '10759877427197301733'();
schema(13292494231364804825) -> '13292494231364804825'();
schema(['TestGroupInUnion', unionVar1]) -> '13292494231364804825'();
schema(13525483159656916942) -> '13525483159656916942'();
schema(root) -> '13525483159656916942'();
schema([root]) -> '13525483159656916942'();
schema(_) -> undefined.

root() -> '13525483159656916942'().

root([]) -> '13525483159656916942'().

'13525483159656916942'() ->
    #schema_node{module = test1_capnp, name = root, id = 13525483159656916942, scope = 0,
		 src = <<"test1.capnp">>, kind = file,
		 nodes =
		     [14973849609597724955,  %% TestBoringInteger
		      14598953150269889741,  %% TestMultipleIntegers
		      16759011188907251718,  %% TestBoringPointer
		      15357896496141030843,  %% TestLessBoringPointer
		      11602115113996880377,  %% TestTextType
		      18323824905083102260,  %% TestTextList
		      16172797807139408828,  %% SimpleEnum
		      17210050650163916943,  %% TestEnum
		      17102922043347402444,  %% TestPrimitiveList
		      10339243589925359431,  %% TestEnumList
		      14043516564278224224,  %% SimpleShortStruct
		      13983806467864851022,  %% TestShortList
		      17944740380774124201,  %% TestPointerList
		      12428803461894409050,  %% TestCompositeList
		      9407938069320387404,  %% TestDefaults
		      11761371100030074809,  %% TestUnion
		      17124799971340330905,  %% TestGroup
		      16202860960915816089]}.  %% TestGroupInUnion

'TestGroupInUnion'() -> '16202860960915816089'().

'TestGroupInUnion'([union2]) -> '10759877427197301733'();
'TestGroupInUnion'([unionVar1]) -> '13292494231364804825'();
'TestGroupInUnion'([]) -> '16202860960915816089'().

'16202860960915816089'() ->
    #schema_node{module = test1_capnp, name = 'TestGroupInUnion', id = 16202860960915816089,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestGroupInUnion">>,
		 kind =
		     #struct{dsize = 3, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = unionVar1, kind = #group{id = 13292494231364804825}},
					     #field{id = 1, name = unionVar2, kind = #data{type = int32, align = 0, default = <<0, 0, 0, 0>>}},
					     #field{id = 2, name = unionVar3,
						    kind = #data{type = int64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
				       align = 32, default = <<0, 0>>},
			     fields = [#field{id = 0, name = union2, kind = #group{id = 10759877427197301733}}]}}.

'10759877427197301733'() ->
    #schema_node{module = test1_capnp, name = ['TestGroupInUnion', union2], id = 10759877427197301733,
		 scope = 16202860960915816089, src = <<"test1.capnp:TestGroupInUnion.union2">>,
		 kind =
		     #struct{dsize = 3, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = testVar1, kind = #data{type = int32, align = 128, default = <<0, 0, 0, 0>>}},
					     #field{id = 1, name = testVar2,
						    kind = #ptr{type = {list, int32}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
				       align = 48, default = <<0, 0>>},
			     fields = []}}.

'13292494231364804825'() ->
    #schema_node{module = test1_capnp, name = ['TestGroupInUnion', unionVar1],
		 id = 13292494231364804825, scope = 16202860960915816089,
		 src = <<"test1.capnp:TestGroupInUnion.unionVar1">>,
		 kind =
		     #struct{dsize = 3, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1, kind = #data{type = int32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = testVar2,
					 kind = #data{type = int64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestGroup'() -> '17124799971340330905'().

'TestGroup'([group1]) -> '10175146911558660158'();
'TestGroup'([]) -> '17124799971340330905'().

'17124799971340330905'() ->
    #schema_node{module = test1_capnp, name = 'TestGroup', id = 17124799971340330905,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestGroup">>,
		 kind =
		     #struct{dsize = 2, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = group1, kind = #group{id = 10175146911558660158}},
				  #field{id = 1, name = testVar3,
					 kind = #data{type = int32, align = 32, default = <<0, 0, 0, 0>>}}]}}.

'10175146911558660158'() ->
    #schema_node{module = test1_capnp, name = ['TestGroup', group1], id = 10175146911558660158,
		 scope = 17124799971340330905, src = <<"test1.capnp:TestGroup.group1">>,
		 kind =
		     #struct{dsize = 2, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1, kind = #data{type = int32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = testVar2,
					 kind = #data{type = int64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestUnion'() -> '11761371100030074809'().

'TestUnion'([]) -> '11761371100030074809'().

'11761371100030074809'() ->
    #schema_node{module = test1_capnp, name = 'TestUnion', id = 11761371100030074809,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestUnion">>,
		 kind =
		     #struct{dsize = 3, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = union1, kind = #data{type = int32, align = 32, default = <<0, 0, 0, 0>>}},
					     #field{id = 1, name = union2,
						    kind = #data{type = int64, align = 128, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 2, name = union3,
						    kind = #ptr{type = {list, int8}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 3, name = union4, kind = void}]},
				       align = 64, default = <<0, 0>>},
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind = #data{type = int32, align = 0, default = <<0, 0, 0, 0>>}}]}}.

'TestDefaults'() -> '9407938069320387404'().

'TestDefaults'([]) -> '9407938069320387404'().

'9407938069320387404'() ->
    #schema_node{module = test1_capnp, name = 'TestDefaults', id = 9407938069320387404,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestDefaults">>,
		 kind =
		     #struct{dsize = 1, psize = 3, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind = #data{type = int64, align = 0, default = <<53, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = testVar2,
					 kind =
					     #ptr{type = {struct, 14973849609597724955}, idx = 0,
						  default = <<0, 0, 0, 0, 1, 0, 0, 0, 54, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 2, name = testVar3,
					 kind =
					     #ptr{type = {struct, 15357896496141030843}, idx = 1,
						  default =
						      <<0, 0, 0, 0, 1, 0, 2, 0, 56, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 1, 0, 4, 0, 0, 0, 4, 0, 0, 0,
							16, 0, 0, 0, 1, 0, 0, 0, 56, 0, 0, 0, 0, 0, 0, 0, 57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
							0, 0, 0, 0, 0, 0, 0, 0, 55, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 3, name = testVar4,
					 kind =
					     #ptr{type = {list, int64}, idx = 2,
						  default =
						      <<1, 0, 0, 0, 29, 0, 0, 0, 58, 0, 0, 0, 0, 0, 0, 0, 59, 0, 0, 0, 0, 0, 0, 0, 60, 0, 0, 0, 0, 0, 0,
							0>>}}]}}.

'TestCompositeList'() -> '12428803461894409050'().

'TestCompositeList'([]) -> '12428803461894409050'().

'12428803461894409050'() ->
    #schema_node{module = test1_capnp, name = 'TestCompositeList', id = 12428803461894409050,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestCompositeList">>,
		 kind =
		     #struct{dsize = 0, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind =
					     #ptr{type = {list, {struct, 14598953150269889741}}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = testVar2,
					 kind =
					     #ptr{type = {list, {struct, 15357896496141030843}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestPointerList'() -> '17944740380774124201'().

'TestPointerList'([]) -> '17944740380774124201'().

'17944740380774124201'() ->
    #schema_node{module = test1_capnp, name = 'TestPointerList', id = 17944740380774124201,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestPointerList">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind =
					     #ptr{type = {list, {struct, 16759011188907251718}}, idx = 0,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestShortList'() -> '13983806467864851022'().

'TestShortList'([]) -> '13983806467864851022'().

'13983806467864851022'() ->
    #schema_node{module = test1_capnp, name = 'TestShortList', id = 13983806467864851022,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestShortList">>,
		 kind =
		     #struct{dsize = 0, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind =
					     #ptr{type = {list, {struct, 14973849609597724955}}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = testVar2,
					 kind =
					     #ptr{type = {list, {struct, 14043516564278224224}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'SimpleShortStruct'() -> '14043516564278224224'().

'SimpleShortStruct'([]) -> '14043516564278224224'().

'14043516564278224224'() ->
    #schema_node{module = test1_capnp, name = 'SimpleShortStruct', id = 14043516564278224224,
		 scope = 13525483159656916942, src = <<"test1.capnp:SimpleShortStruct">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1, kind = #data{type = int8, align = 0, default = <<0>>}},
				  #field{id = 1, name = testVar2, kind = #data{type = int16, align = 16, default = <<0, 0>>}}]}}.

'TestEnumList'() -> '10339243589925359431'().

'TestEnumList'([]) -> '10339243589925359431'().

'10339243589925359431'() ->
    #schema_node{module = test1_capnp, name = 'TestEnumList', id = 10339243589925359431,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestEnumList">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind =
					     #ptr{type = {list, {enum, 16172797807139408828}}, idx = 0,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestPrimitiveList'() -> '17102922043347402444'().

'TestPrimitiveList'([]) -> '17102922043347402444'().

'17102922043347402444'() ->
    #schema_node{module = test1_capnp, name = 'TestPrimitiveList', id = 17102922043347402444,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestPrimitiveList">>,
		 kind =
		     #struct{dsize = 0, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind = #ptr{type = {list, bool}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = testVar2,
					 kind = #ptr{type = {list, int8}, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 2, name = testVar3,
					 kind = #ptr{type = {list, int16}, idx = 2, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 3, name = testVar4,
					 kind = #ptr{type = {list, int32}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 4, name = testVar5,
					 kind = #ptr{type = {list, int64}, idx = 4, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestEnum'() -> '17210050650163916943'().

'TestEnum'([]) -> '17210050650163916943'().

'17210050650163916943'() ->
    #schema_node{module = test1_capnp, name = 'TestEnum', id = 17210050650163916943,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestEnum">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind = #data{type = {enum, 16172797807139408828}, align = 0, default = <<0, 0>>}}]}}.

'SimpleEnum'() -> '16172797807139408828'().

'SimpleEnum'([]) -> '16172797807139408828'().

'16172797807139408828'() ->
    #schema_node{module = test1_capnp, name = 'SimpleEnum', id = 16172797807139408828,
		 scope = 13525483159656916942, src = <<"test1.capnp:SimpleEnum">>,
		 kind = #enum{values = [{0, testEnum1}, {1, testEnum2}, {2, testEnum3}]}}.

'TestTextList'() -> '18323824905083102260'().

'TestTextList'([]) -> '18323824905083102260'().

'18323824905083102260'() ->
    #schema_node{module = test1_capnp, name = 'TestTextList', id = 18323824905083102260,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestTextList">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind = #ptr{type = {list, text}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestTextType'() -> '11602115113996880377'().

'TestTextType'([]) -> '11602115113996880377'().

'11602115113996880377'() ->
    #schema_node{module = test1_capnp, name = 'TestTextType', id = 11602115113996880377,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestTextType">>,
		 kind =
		     #struct{dsize = 0, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{id = 1, name = testVar2, kind = #ptr{type = data, idx = 1, default = <<>>}}]}}.

'TestLessBoringPointer'() -> '15357896496141030843'().

'TestLessBoringPointer'([]) -> '15357896496141030843'().

'15357896496141030843'() ->
    #schema_node{module = test1_capnp, name = 'TestLessBoringPointer', id = 15357896496141030843,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestLessBoringPointer">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind = #ptr{type = {struct, 16759011188907251718}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = testVar2, kind = #data{type = int16, align = 0, default = <<0, 0>>}},
				  #field{id = 2, name = testVar3,
					 kind =
					     #ptr{type = {struct, 14598953150269889741}, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestBoringPointer'() -> '16759011188907251718'().

'TestBoringPointer'([]) -> '16759011188907251718'().

'16759011188907251718'() ->
    #schema_node{module = test1_capnp, name = 'TestBoringPointer', id = 16759011188907251718,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestBoringPointer">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind =
					     #ptr{type = {struct, 14973849609597724955}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestMultipleIntegers'() -> '14598953150269889741'().

'TestMultipleIntegers'([]) -> '14598953150269889741'().

'14598953150269889741'() ->
    #schema_node{module = test1_capnp, name = 'TestMultipleIntegers', id = 14598953150269889741,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestMultipleIntegers">>,
		 kind =
		     #struct{dsize = 4, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = testVar2, kind = #data{type = uint32, align = 64, default = <<0, 0, 0, 0>>}},
				  #field{id = 2, name = testVar3, kind = #data{type = uint16, align = 96, default = <<0, 0>>}},
				  #field{id = 3, name = testVar4, kind = #data{type = uint8, align = 112, default = <<0>>}},
				  #field{id = 4, name = testVar5, kind = #data{type = int8, align = 120, default = <<0>>}},
				  #field{id = 5, name = testVar6, kind = #data{type = int8, align = 128, default = <<0>>}},
				  #field{id = 6, name = testVar7,
					 kind = #data{type = int64, align = 192, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'TestBoringInteger'() -> '14973849609597724955'().

'TestBoringInteger'([]) -> '14973849609597724955'().

'14973849609597724955'() ->
    #schema_node{module = test1_capnp, name = 'TestBoringInteger', id = 14973849609597724955,
		 scope = 13525483159656916942, src = <<"test1.capnp:TestBoringInteger">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = testVar1,
					 kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.