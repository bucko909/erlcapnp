#include "erl_nif.h"
#include "test1.capnp.h"
#include <capnp/message.h>
#include <capnp/layout.h>
#include <stdio.h>
#include <new>

extern "C" {
ErlNifResourceType* TestTextType_TYPE;
ErlNifResourceType* MallocMessageBuilder_TYPE;

void MallocMessageBuilder_free(ErlNifEnv* env, void* obj)
{
	::capnp::MallocMessageBuilder *builder = (::capnp::MallocMessageBuilder *)obj;
	builder->~MallocMessageBuilder();
}

void TestTextType_Builder_free(ErlNifEnv* env, void* obj)
{
	// Don't actually need to dealloc these! Woo!
	TestTextType::Builder *builder = (TestTextType::Builder *)obj;
	builder->~Builder();
}

// There are four functions that may be called during the lifetime
// of a NIF. load, reload, upgrade, and unload. Any of these functions
// can be left unspecified by passing NULL to the ERL_NIF_INIT macro.
//
// NIFs are awesome.

// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
	const char *mod = "capnp_nif";
	const char *MallocMessageBuilder_name = "MallocMessageBuilder";
	const char *TestTextType_name = "TestTextType";
	ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

	MallocMessageBuilder_TYPE = enif_open_resource_type(env, mod, MallocMessageBuilder_name, MallocMessageBuilder_free, flags, NULL);
	if (MallocMessageBuilder_TYPE == NULL) return -1;
	TestTextType_TYPE = enif_open_resource_type(env, mod, TestTextType_name, TestTextType_Builder_free, flags, NULL);
	if (TestTextType_TYPE == NULL) return -1;

	return 0;
}

// Called when changing versions of the C code for a module's NIF
// implementation if I read the docs correctly.
//
// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#upgrade

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
	return load(env, priv, load_info);
}

// Called when the library is unloaded. Not called after a reload
// executes.
//
// No return value
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}

ERL_NIF_TERM raise_internal_error(ErlNifEnv *env, const char *text="internal_error") {
	ERL_NIF_TERM atom_error = enif_make_atom(env, "error");
	ERL_NIF_TERM atom_internal_error = enif_make_atom(env, text);
	return enif_raise_exception(env, enif_make_tuple2(env, atom_error, atom_internal_error));
}

// The actual C implementation of an Erlang function.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc
static ERL_NIF_TERM new_message_builder(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	::capnp::MallocMessageBuilder *messageBuilder = (::capnp::MallocMessageBuilder *)enif_alloc_resource(MallocMessageBuilder_TYPE, sizeof(::capnp::MallocMessageBuilder));
	if (messageBuilder == NULL) return raise_internal_error(env);

	new((void *)messageBuilder) ::capnp::MallocMessageBuilder;

	ERL_NIF_TERM x = enif_make_resource(env, messageBuilder);
	//enif_release_resource(messageBuilder);
	return x;
}

static ERL_NIF_TERM initRoot_TestTextType(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	::capnp::MallocMessageBuilder *messageBuilder;
	if (!enif_get_resource(env, argv[0], MallocMessageBuilder_TYPE, (void**)&messageBuilder)) return enif_make_badarg(env);

	TestTextType::Builder *builder = (TestTextType::Builder *)enif_alloc_resource(TestTextType_TYPE, sizeof(TestTextType::Builder));
	if (builder == NULL) return raise_internal_error(env);

	*builder = messageBuilder->initRoot<TestTextType>();

	ERL_NIF_TERM r = enif_make_resource(env, messageBuilder);
	enif_release_resource(messageBuilder);
	ERL_NIF_TERM r2 = enif_make_resource(env, builder);
	enif_release_resource(builder);
	return enif_make_tuple2(env, r, r2);
}

static ERL_NIF_TERM set_TestTextType_testVar1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	int arity;
	const ERL_NIF_TERM *tuple_terms;
	if (!enif_get_tuple(env, argv[0], &arity, &tuple_terms)) return raise_internal_error(env, "argument_1_not_tuple");
	if (arity != 2) return enif_make_badarg(env);

	TestTextType::Builder *builder;
	if (!enif_get_resource(env, tuple_terms[1], TestTextType_TYPE, (void**)&builder)) return raise_internal_error(env, "argument_1_elt_2_not_builder");

	ErlNifBinary param;
	if (!enif_inspect_iolist_as_binary(env, argv[1], &param)) return raise_internal_error(env, "argument_2_not_binary");

	::capnp::Text::Builder textBuilder = builder->initTestVar1(param.size);
	memcpy(textBuilder.begin(), &param.data[0], param.size);

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_TestTextType_testVar2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	int arity;
	const ERL_NIF_TERM *tuple_terms;
	if (!enif_get_tuple(env, argv[0], &arity, &tuple_terms)) return raise_internal_error(env, "argument_1_not_tuple");
	if (arity != 2) return enif_make_badarg(env);

	TestTextType::Builder *builder;
	if (!enif_get_resource(env, tuple_terms[1], TestTextType_TYPE, (void**)&builder)) return raise_internal_error(env, "argument_1_elt_2_not_builder");

	ErlNifBinary param;
	if (!enif_inspect_iolist_as_binary(env, argv[1], &param)) return raise_internal_error(env, "argument_2_not_binary");

	::capnp::Data::Builder dataBuilder = builder->initTestVar2(param.size);
	memcpy(dataBuilder.begin(), &param.data[0], param.size);

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	::capnp::MallocMessageBuilder *messageBuilder;
	if (!enif_get_resource(env, argv[0], MallocMessageBuilder_TYPE, (void **)&messageBuilder)) return enif_make_badarg(env);
	kj::ArrayPtr<const kj::ArrayPtr<const capnp::word>> segments = messageBuilder->getSegmentsForOutput();

	int size = sizeof(uint32_t);
	for (uint i = 0; i < segments.size(); i++) {
		size += sizeof(uint32_t);
		size += segments[i].size() * sizeof(capnp::word);
	}
	if (segments.size() % 2 == 0) {
		// Set padding byte.
		size += sizeof(uint32_t);
	}

	ErlNifBinary bin;
	if (!enif_alloc_binary(size, &bin)) return raise_internal_error(env);

	uint32_t *table = (uint32_t *)bin.data;
	*(table++) = segments.size() - 1;
	for (uint i = 0; i < segments.size(); i++) {
		*(table++) = segments[i].size();
	}
	if (segments.size() % 2 == 0) {
		// Set padding byte.
		*(table++) = 0;
	}

	char *pieces = (char *)table;
	for (uint i = 0; i < segments.size(); i++) {
		int len = segments[i].size() * sizeof(capnp::word);
		memcpy(pieces, segments[i].begin(), len);
		pieces += len;
	}

	return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM lol(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	::capnp::MallocMessageBuilder messageBuilder;
	messageBuilder.initRoot<TestTextType>();

	kj::ArrayPtr<const kj::ArrayPtr<const capnp::word>> segments = messageBuilder.getSegmentsForOutput();

	int size = sizeof(uint32_t);
	for (uint i = 0; i < segments.size(); i++) {
		size += sizeof(uint32_t);
		size += segments[i].size() * sizeof(capnp::word);
	}
	if (segments.size() % 2 == 0) {
		// Set padding byte.
		size += sizeof(uint32_t);
	}

	ErlNifBinary bin;
	if (!enif_alloc_binary(size, &bin)) return raise_internal_error(env);

	uint32_t *table = (uint32_t *)bin.data;
	*(table++) = segments.size() - 1;
	for (uint i = 0; i < segments.size(); i++) {
		*(table++) = segments[i].size();
	}
	if (segments.size() % 2 == 0) {
		// Set padding byte.
		*(table++) = 0;
	}

	char *pieces = (char *)table;
	for (uint i = 0; i < segments.size(); i++) {
		int len = segments[i].size() * sizeof(capnp::word);
		memcpy(pieces, segments[i].begin(), len);
		pieces += len;
	}

	return enif_make_binary(env, &bin);
}

static ErlNifFunc nif_funcs[] = {
    {"new_message_builder", 0, new_message_builder},
    {"initRoot_TestTextType", 1, initRoot_TestTextType},
    {"to_binary", 1, to_binary},
	{"set_TestTextType_testVar1", 2, set_TestTextType_testVar1},
	{"set_TestTextType_testVar2", 2, set_TestTextType_testVar2},
    {"lol", 0, lol}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(capnp_nif, nif_funcs, &load, NULL, &upgrade, &unload);
}
