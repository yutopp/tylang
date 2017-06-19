-module(tylang_abstract_code).
-export([debug_info/4]).

debug_info(_Format, _Module, {none, _CompilerOpts}, _Opts) ->
	{error, missing};
debug_info(erlang_v1, Module, {AbstrCode, CompilerOpts}, Opts) ->
    case tylang_transpiler:transpile(AbstrCode) of
        {ok, ErlAbstrCode} ->
            erl_abstract_code:debug_info(erlang_v1, Module, {ErlAbstrCode, CompilerOpts}, Opts);
        {error, Reason} ->
            {error, {failed_to_extract_tylang_code, Reason}}
    end;
debug_info(core_v1, Module, {AbstrCode, CompilerOpts}, Opts) ->
    case tylang_transpiler:transpile(AbstrCode) of
        {ok, ErlAbstrCode} ->
            erl_abstract_code:debug_info(core_v1, Module, {ErlAbstrCode, CompilerOpts}, Opts);
        {error, Reason} ->
            {error, {failed_to_extract_tylang_code, Reason}}
    end;
debug_info(_, _, _, _) ->
	{error, unknown_format}.
