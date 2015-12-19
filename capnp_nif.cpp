#include "erl_nif.h"
#include "test1.capnp.h"
#include <capnp/message.h>
#include <capnp/layout.h>
#include <stdio.h>
#include <new>

extern "C" {
ErlNifResourceType* TestTextType_TYPE;
ErlNifResourceType* MallocMessageBuilder_TYPE;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_internal_error;
ERL_NIF_TERM term_internal_error;

void MallocMessageBuilder_free(ErlNifEnv* env, void* obj)
{
	printf("free message %#18llx\r\n", (long long)obj);
	fflush(stdout);
	::capnp::MallocMessageBuilder *builder = (::capnp::MallocMessageBuilder *)obj;
	builder->~MallocMessageBuilder();
	printf("done free message %#18llx\r\n", (long long)obj);
	fflush(stdout);
}

void TestTextType_Builder_free(ErlNifEnv* env, void* obj)
{
	// Don't actually need to dealloc these! Woo!
	printf("free builder %#18llx\r\n", (long long)obj);
	fflush(stdout);
	TestTextType::Builder *builder = (TestTextType::Builder *)obj;
	builder->~Builder();
	printf("done free builder %#18llx\r\n", (long long)obj);
	fflush(stdout);
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
	printf("load\r\n");
	fflush(stdout);
	const char *mod = "capnp_nif";
	const char *MallocMessageBuilder_name = "MallocMessageBuilder";
	const char *TestTextType_name = "TestTextType";
	ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

	MallocMessageBuilder_TYPE = enif_open_resource_type(env, mod, MallocMessageBuilder_name, MallocMessageBuilder_free, flags, NULL);
	TestTextType_TYPE = enif_open_resource_type(env, mod, TestTextType_name, TestTextType_Builder_free, flags, NULL);
	atom_ok = enif_make_atom(env, "ok");
	atom_error = enif_make_atom(env, "error");
	atom_internal_error = enif_make_atom(env, "internal_error");
	term_internal_error = enif_make_tuple2(env, atom_error, atom_internal_error);
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
	printf("upgrade\r\n");
	fflush(stdout);
    return 0;
}

// Called when the library is unloaded. Not called after a reload
// executes.
//
// No return value
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static void
unload(ErlNifEnv* env, void* priv)
{
	printf("unload\r\n");
	fflush(stdout);
    return;
}

// The actual C implementation of an Erlang function.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc
static ERL_NIF_TERM new_message_builder(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	printf(">>>new_message_builder\r\n");
	fflush(stdout);
	::capnp::MallocMessageBuilder *messageBuilder = (::capnp::MallocMessageBuilder *)enif_alloc_resource(MallocMessageBuilder_TYPE, sizeof(::capnp::MallocMessageBuilder));
	if (messageBuilder == NULL) return enif_raise_exception(env, term_internal_error);

	printf("alloc message %#18llx\r\n", (long long)messageBuilder);
	fflush(stdout);
	new((void *)messageBuilder) ::capnp::MallocMessageBuilder;

	ERL_NIF_TERM x = enif_make_resource(env, messageBuilder);
	printf("return message %#18llx\r\n", (long long)messageBuilder);
	fflush(stdout);
	return x;
}

static ERL_NIF_TERM initRoot_TestTextType(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	printf(">>>initRoot_TestTextType\r\n");
	::capnp::MallocMessageBuilder *messageBuilder;
	if (!enif_get_resource(env, argv[0], MallocMessageBuilder_TYPE, (void**)&messageBuilder)) return enif_make_badarg(env);

	TestTextType::Builder *builder = (TestTextType::Builder *)enif_alloc_resource(TestTextType_TYPE, sizeof(TestTextType::Builder));
	if (builder == NULL) return enif_raise_exception(env, term_internal_error);

	printf("alloc builder %#18llx\r\n", (long long)builder);
	fflush(stdout);
	*builder = messageBuilder->initRoot<TestTextType>();

	ERL_NIF_TERM r = enif_make_resource(env, messageBuilder);
	enif_release_resource(messageBuilder);
	ERL_NIF_TERM r2 = enif_make_resource(env, builder);
	enif_release_resource(builder);
	printf("return builder %#18llx\r\n", (long long)builder);
	fflush(stdout);
	return enif_make_tuple2(env, r, r2);
}

static ERL_NIF_TERM to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	printf(">>>to_binary\r\n");
	::capnp::MallocMessageBuilder *messageBuilder;
	if (!enif_get_resource(env, argv[0], MallocMessageBuilder_TYPE, (void **)&messageBuilder)) return enif_make_badarg(env);
	printf("make binary %#18llx\r\n", (long long)messageBuilder);
	fflush(stdout);
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
	if (!enif_alloc_binary(size, &bin)) return enif_raise_exception(env, term_internal_error);

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

	printf("return binary %#18llx\r\n", (long long)messageBuilder);
	fflush(stdout);
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
	if (!enif_alloc_binary(size, &bin)) return enif_raise_exception(env, term_internal_error);

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
    {"lol", 0, lol}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(capnp_nif, nif_funcs, &load, NULL, &upgrade, &unload);
}
