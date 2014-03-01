%-type 'Void'(). % :: undefined
%-type 'Text'(). % :: io_list()
%-type 'Bool'(), % :: boolean()
%-type 'UInt16'(). % :: integer()
%-type 'UInt32'(). % :: integer()
%-type 'UInt64'(). % :: integer()
%-type 'Id'() % :: 'UInt64'(). % UInt64

% Erlang doesn't do recursive types because it's horrible.
%-type 'capnp::namespace::Node'(). % :: any()
%-type 'capnp::namespace::Node::'(). % :: any()
%-type 'capnp::namespace::Node::NestedNode'(). % :: any()

-record('capnp::namespace::Node', {
		id, % :: 'Id'()
		displayName, % :: 'Text'()
		displayNamePrefixLength, % :: 'UInt32'()
		scopeId, % :: 'Id'()
		nestedNodes, % :: list('capnp::namespace::Node::NestedNode'())
		annotations, % :: list('capnp::namespace::Annotation'())
		% TODO this atom() should be list enumerations, with the id embedded for speed!
		'' % :: {atom(), 'capnp::namespace::Node::'()}
	}).

-record('capnp::namespace::Node::NestedNode', {
		name, % :: 'Text'()
		id % :: 'Id'()
	}).

%-type 'capnp::namespace::Node::::file'. % :: 'Void'()
-record('capnp::namespace::Node::::struct', {
		dataWordCount, % :: 'UInt16'()
		pointerCount, % :: 'UInt16'()
		preferredListEncoding, % :: 'ElementSize'()
		isGroup, % :: 'Bool'()
		discriminantCount, % :: 'UInt16'()
		discriminantOffset, % :: 'UInt32'()
		fields % :: list('capnp::namespace::Field'())
	}).
-record('capnp::namespace::Node::::enum', {
		enumerants % :: list('capnp::namespace::Enumerant'())
	}).
-record('capnp::namespace::Node::::interface', {
		methods, % :: list('capnp::namespace::Method'())
		extends % :: list('Id'())
	}).
-record('capnp::namespace::Node::::const', {
		type, % :: 'capnp::namespace::Type'()
		value % :: 'capnp::namespace::Value'()
	}).
-record('capnp::namespace::Node::::annotation', {
		type, % :: 'capnp::namespace::Type'()
		targetsFile, % :: 'Bool'()
		targetsConst, % :: 'Bool'()
		targetsEnum, % :: 'Bool'()
		targetsEnumerant, % :: 'Bool'()
		targetsStruct, % :: 'Bool'()
		targetsField, % :: 'Bool'()
		targetsUnion, % :: 'Bool'()
		targetsGroup, % :: 'Bool'()
		targetsInterface, % :: 'Bool'()
		targetsMethod, % :: 'Bool'()
		targetsParam, % :: 'Bool'()
		targetsAnnotation % :: 'Bool'()
	}).

-define('capnp::namespace::Field::noDiscriminant', 65535).
-record('capnp::namespace::Field', {
		name, % :: 'Text'()
		codeOrder, % :: 'UInt16'()
		annotations, % :: list('capnp::namespace::Annotation'())
		discriminantValue, % :: 'UInt16'()
		% TODO this atom() should be list enumerations, with the id embedded for speed!
		'', % :: {atom(), 'capnp::namespace::Field::'()}
		% TODO this atom() should be list enumerations, with the id embedded for speed!
		'ordinal' % :: {atom(), 'capnp::namespace::Field::ordinal'()}
	}).
-record('capnp::namespace::Field::::slot', {
		offset, % :: 'UInt32'()
		type, % :: 'capnp::namespace::Type'()
		defaultValue, % :: 'capnp::namespace::Value'()
		hadExplicitDefault % :: 'Bool'()
	}).
-record('capnp::namespace::Field::::group', {
		typeId % :: 'UInt64'()
	}).
%-type 'capnp::namespace::Field::ordinal::implicit'. % :: 'Void'()
%-type 'capnp::namespace::Field::ordinal::explicit'. % :: 'UInt16'()

-record('capnp::namespace::', {
	}).

-record('capnp::namespace::Enumerant', {
		name, % :: 'Text'()
		codeOrder, % :: 'UInt16'()
		annotations % :: list('capnp::namespace::Annotation'())
	}).

-record('capnp::namespace::Method', {
		name, % :: 'Text'()
		codeOrder, % :: 'UInt16'()
		paramStructType, % :: 'Id'()
		resultStructType, % :: 'Id'()
		annotations % :: list('capnp::namespace::Annotation'())
	}).

-record('capnp::namespace::Type', {
		% TODO this atom() should be list enumerations, with the id embedded for speed!
		'' % :: {atom(), 'capnp::namespace::Type::'}
	}).
%-type 'capnp::namespace::Type::::void'. % :: 'Void'()
%-type 'capnp::namespace::Type::::bool'. % :: 'Void'()
%-type 'capnp::namespace::Type::::int8'. % :: 'Void'()
%-type 'capnp::namespace::Type::::int16'. % :: 'Void'()
%-type 'capnp::namespace::Type::::int32'. % :: 'Void'()
%-type 'capnp::namespace::Type::::int64'. % :: 'Void'()
%-type 'capnp::namespace::Type::::uint8'. % :: 'Void'()
%-type 'capnp::namespace::Type::::uint16'. % :: 'Void'()
%-type 'capnp::namespace::Type::::uint32'. % :: 'Void'()
%-type 'capnp::namespace::Type::::uint64'. % :: 'Void'()
%-type 'capnp::namespace::Type::::float32'. % :: 'Void'()
%-type 'capnp::namespace::Type::::float64'. % :: 'Void'()
%-type 'capnp::namespace::Type::::text'. % :: 'Void'()
%-type 'capnp::namespace::Type::::data'. % :: 'Void'()
-record('capnp::namespace::Type::::list', {
		elementType % :: 'capnp::namespace::Type'()
	}).
-record('capnp::namespace::Type::::enum', {
		typeId % :: 'Id'()
	}).
-record('capnp::namespace::Type::::struct', {
		typeId % :: 'Id'()
	}).
-record('capnp::namespace::Type::::interface', {
		typeId % :: 'Id'()
	}).
%-type 'capnp::namespace::Type::::anyPointer'. % :: 'Void'()


-record('capnp::namespace::Value', {
		% TODO this atom() should be list enumerations, with the id embedded for speed!
		'' % :: {atom(), 'capnp::namespace::Value::'}
	}).
%-type 'capnp::namespace::Value::::void'. % :: 'Void'()
%-type 'capnp::namespace::Value::::bool'. % :: 'Bool'()
%-type 'capnp::namespace::Value::::int8'. % :: 'Int8'()
%-type 'capnp::namespace::Value::::int16'. % :: 'Int16'()
%-type 'capnp::namespace::Value::::int32'. % :: 'Int32'()
%-type 'capnp::namespace::Value::::int64'. % :: 'Int64'()
%-type 'capnp::namespace::Value::::uint8'. % :: 'UInt8'()
%-type 'capnp::namespace::Value::::uint16'. % :: 'UInt16'()
%-type 'capnp::namespace::Value::::uint32'. % :: 'UInt32'()
%-type 'capnp::namespace::Value::::uint64'. % :: 'UInt64'()
%-type 'capnp::namespace::Value::::float32'. % :: 'Float32'()
%-type 'capnp::namespace::Value::::float64'. % :: 'Float32'()
%-type 'capnp::namespace::Value::::text'. % :: 'Text'()
%-type 'capnp::namespace::Value::::data'. % :: 'Data'()
%-type 'capnp::namespace::Value::::list'. % :: 'AnyPointer'()
%-type 'capnp::namespace::Value::::enum'. % :: 'UInt16'()
%-type 'capnp::namespace::Value::::struct'. % :: 'AnyPointer'()
%-type 'capnp::namespace::Value::::interface'. % :: 'Void'()
%-type 'capnp::namespace::Value::::anyPointer'. % :: 'AnyPointer'()

-record('capnp::namespace::Annotation', {
		id, % :: 'Id'()
		value % :: 'capnp::namespace::Value'()
	}).

-define('capnp::namespace::ElementSize::empty', {0, empty}).
-define('capnp::namespace::ElementSize::bit', {1, bit}).
-define('capnp::namespace::ElementSize::byte', {2, byte}).
-define('capnp::namespace::ElementSize::twoBytes', {3, twoBytes}).
-define('capnp::namespace::ElementSize::fourBytes', {4, fourBytes}).
-define('capnp::namespace::ElementSize::eightBytes', {5, eightBytes}).
-define('capnp::namespace::ElementSize::pointer', {6, pointer}).
-define('capnp::namespace::ElementSize::inlineComposite', {7, inlineComposite}).
-define('capnp::namespace::ElementSize::', {
		'capnp::namespace::ElementSize::empty',
		'capnp::namespace::ElementSize::bit',
		'capnp::namespace::ElementSize::byte',
		'capnp::namespace::ElementSize::twoBytes',
		'capnp::namespace::ElementSize::fourBytes',
		'capnp::namespace::ElementSize::eightBytes',
		'capnp::namespace::ElementSize::pointer',
		'capnp::namespace::ElementSize::inlineComposite'
	}).
%-type 'capnp::namespace::ElementSize'(). % :: {integer(), atom()}

-record('capnp::namespace::CodeGeneratorRequest', {
		nodes, % :: list('capnp::namespace::Node'())
		requestedFiles % :: list('capnp::namespace::CodeGeneratorRequest::RequestedFile'())
	}).

-record('capnp::namespace::CodeGeneratorRequest::RequestedFile', {
		id, % :: 'Id'()
		filename, % :: 'Text'()
		imports % :: list('capnp::namespace::CodeGeneratorRequest::RequestedFile::Import'())
	}).

-record('capnp::namespace::CodeGeneratorRequest::RequestedFile::Import', {
		id, % :: 'Id'()
		name % :: 'Text'()
	}).
