@0xbbb42b45cae0e7ce;
struct TestBoringInteger {
	testVar1 @0 :UInt64;
}

struct TestMultipleIntegers {
	testVar1 @0 :UInt64;
	testVar2 @1 :UInt32;
	testVar3 @2 :UInt16;
	testVar4 @3 :UInt8;
	testVar5 @4 :Int8;
	testVar6 @5 :Int8;
	testVar7 @6 :Int64;
}

struct TestBoringPointer {
	testVar1 @0 :TestBoringInteger;
}

struct TestLessBoringPointer {
	testVar1 @0 :TestBoringPointer;
	testVar2 @1 :Int16;
	testVar3 @2 :TestMultipleIntegers;
}

struct TestTextType {
	testVar1 @0 :Text;
	testVar2 @1 :Data;
}

struct TestTextList {
	testVar1 @0 :List(Text);
	testVar2 @1 :List(Data);
}

struct TestTextListList {
	testVar1 @0 :List(List(Text));
	testVar2 @1 :List(List(Data));
}

enum SimpleEnum {
	testEnum1 @0;
	testEnum2 @1;
	testEnum3 @2;
}

struct TestEnum {
	testVar1 @0 :SimpleEnum;
}

struct TestPrimitiveList {
	testVar1 @0 :List(Bool);
	testVar2 @1 :List(Int8);
	testVar3 @2 :List(Int16);
	testVar4 @3 :List(Int32);
	testVar5 @4 :List(Int64);
}

struct TestEnumList {
	testVar1 @0 :List(SimpleEnum);
}

struct SimpleShortStruct {
	testVar1 @0 :Int8;
	testVar2 @1 :Int16;
}

struct TestShortList {
	testVar1 @0 :List(TestBoringInteger); # == Int64
	testVar2 @1 :List(SimpleShortStruct); # =< Int32
}

struct TestPointerList {
	testVar1 @0 :List(TestBoringPointer); # Single pointer
}

# This guy's doesn't fit inside an Int64!
struct TestCompositeList {
	testVar1 @0 :List(TestMultipleIntegers);
	testVar2 @1 :List(TestLessBoringPointer);
}

struct TestDefaults {
	testVar1 @0 :Int64 = 53;
	testVar2 @1 :TestBoringInteger = (testVar1 = 54);
	testVar3 @2 :TestLessBoringPointer = (testVar1 = (testVar1 = (testVar1 = 55)), testVar2 = 56, testVar3 = (testVar1 = 56, testVar2 = 57));
	testVar4 @3 :List(Int64) = [58, 59, 60];
}

struct TestUnion {
	testVar1 @0 :Int32;
	union {
		union1 @1 :Int32;
		union2 @2 :Int64;
		union3 @3 :List(Int8);
		union4 @4 :Void;
	}
}

struct TestGroup {
	group1 :group {
		testVar1 @0 :Int32;
		testVar2 @1 :Int64;
	}
	testVar3 @2 :Int32;
}

struct TestGroupInUnion {
	union {
		unionVar1 :group {
			testVar1 @0 :Int32;
			testVar2 @1 :Int64;
		}
		unionVar2 @2 :Int32;
		unionVar3 @3 :Int64;
	}
	union2 :union {
		testVar1 @4 :Int32;
		testVar2 @5 :List(Int32);
	}
}
