-module(capnp_format).

-export([
		self_contained_source/2,
		source_with_include/3,
		header_only/2
]).

-compile({parse_transform, uberpt}).

format_erl(Forms) ->
	lists:flatten([ [ erl_pp:form(erl_syntax:revert(Form)), $\n ] || Form <- Forms ]).

self_contained_source({Recs, Funs}, ModuleName) ->
	Line = 0,
	ModuleFileName = atom_to_list(ModuleName) ++ ".erl",
	Forms = self_contained_source_preamble(ModuleFileName, ModuleName) ++ Recs ++ Funs ++ [{eof,Line}],
	format_erl(Forms).

-ast_forms_function(#{
		name => self_contained_source_preamble,
		params => ['ModuleFileName', 'ModuleName']
}).
	% Most of the preamble is preprocessor instructions.
	% We must use {raw, {var, ...}} because we can't pass in a variable directly...
	-uberpt_raw_file({{raw, {var, 1, 'ModuleFileName'}}, 1}).
	-uberpt_raw_module({raw, {var, 1, 'ModuleName'}}).
	-compile([export_all]).
-end_ast_forms_function([]).

source_with_include({_Recs, Funs}, ModuleName, Path) ->
	Line = 0,
	ModuleFileName = atom_to_list(ModuleName) ++ ".erl",
	IncludeFileName = Path ++ "/" ++ atom_to_list(ModuleName) ++ ".hrl",
	Forms = source_with_include_preamble(ModuleFileName, IncludeFileName, ModuleName) ++ Funs ++ [{eof,Line}],
	format_erl(Forms).

-ast_forms_function(#{
		name => source_with_include_preamble,
		params => ['ModuleFileName', 'IncludeFileName', 'ModuleName']
}).
	% Most of the preamble is preprocessor instructions.
	% We must use {raw, {var, ...}} because we can't pass in a variable directly...
	-uberpt_raw_file({{raw, {var, 1, 'ModuleFileName'}}, 1}).
	-uberpt_raw_module({raw, {var, 1, 'ModuleName'}}).
	-uberpt_raw_include_lib({raw, {var, 1, 'IncludeFileName'}}).
	-compile([export_all]).
-end_ast_forms_function([]).

header_only({Recs, _Funs}, ModuleName) ->
	IncludeFileName = atom_to_list(ModuleName) ++ ".hrl",
	Forms = header_only_preamble(IncludeFileName) ++ Recs ++ [{eof,0}],
	format_erl(Forms).

-ast_forms_function(#{
		name => header_only_preamble,
		params => ['IncludeFileName']
}).
	% Most of the preamble is preprocessor instructions.
	% We must use {raw, {var, ...}} because we can't pass in a variable directly...
	-uberpt_raw_file({{raw, {var, 1, 'IncludeFileName'}}, 1}).
-end_ast_forms_function([]).
