%%%-------------------------------------------------------------------
%% @doc tylang public API
%% @end
%%%-------------------------------------------------------------------

-module(tylang).

-behaviour(provider).

%% Application callbacks
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

%%====================================================================
%% API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    Provider0 =
        providers:create([{name, ?PROVIDER},
                          {module, ?MODULE},
                          {namespace, tylang},
                          {bare, false},
                          {deps, ?DEPS},
                          {example, "rebar3 tylang compile"},
                          {short_desc, "Extend Erlang syntax with type annotations."},
                          {desc, "Provide TYped erLANG programming language."},
                          {opts, []}]),
    State1 = rebar_state:add_provider(State0, Provider0),
    {ok, State1}.

%%--------------------------------------------------------------------
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State0) ->
    Apps = case rebar_state:current_app(State0) of
               undefined ->
                   rebar_state:project_apps(State0);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         OutDir = rebar_app_info:ebin_dir(AppInfo),

         rebar_base_compiler:run(Opts, [],
                                 SourceDir, ".trl",
                                 OutDir, ".beam",
                                 fun(Source, Target, Config) ->
                                         tylang_compile(Source, Target, Config)
                                 end,
                                 [])
     end || AppInfo <- Apps],
    {ok, State0}.

%%--------------------------------------------------------------------
-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================
tylang_compile(Source, Target, _Config) ->
    %%CompilerOptions = option(compiler_options, _Opts),
    %%io:format("~p / ~p / ~p", [Source, Target, _Config]),
    Result0 =
        case file:read_file(Source) of
            {ok, SourceCodeBin} ->
                case tylang_parse_from_string(binary_to_list(SourceCodeBin)) of
                    {ok, TylangForms} ->
                        %% TODO: parse transform
                        Opts = [
                                verbose,report_errors,
                                report_warnings
                               ],
                        ErlForms = tylang_transpiler:transpile(TylangForms),
                        ErlOpts = [{debug_info, {tylang_abstract_code, {TylangForms, Opts}}}|Opts],
                        compile:forms(ErlForms, ErlOpts);
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, {failed_to_read, Reason}}
        end,
    io:format("result0: ~p~n", [Result0]),

    Result1 =
        case Result0 of
            {ok, _ModuleName, CodeBin} when is_binary(CodeBin) ->
                file:write_file(Target, CodeBin, [binary])
        end,
    io:format("result1: ~p~n", [Result1]),

    ok.

tylang_parse_from_string(SourceCode) ->
    InitLoc = {1, 1}, % Line:1, Column: 1
    try
        Forms = tylang_parse_impl(erl_scan:tokens([], SourceCode ++ eof, InitLoc, []), []),
        {ok, Forms}
    catch
        error:Reason ->
            {error, {failed_to_parse, Reason}}
    end.

tylang_parse_impl({done, {ok, Tokens, EndLoc}, RestSourceCode}, Forms) ->
    io:format("compiling(0: raw): ~p / REST: ~p~n", [Tokens, RestSourceCode]),

    %% preprocess
    NewTokens =
        Tokens,
%%        case aleppo:process_tokens(Tokens, []) of
%%            {ok, NewTokens0} -> NewTokens0;
%%            {error, Reason0} -> error({failed_to_preprocess, Reason0, EndLoc})
%%        end,
%%    io:format("compiling(1: pp): ~p~n", [NewTokens]),

    %% parse
    Form =
        case tylang_parse:parse_form(NewTokens) of
            {ok, Form0}      -> Form0;
            {error, Reason1} -> error({failed_to_parse, Reason1, EndLoc})
            end,
    io:format("compiling(2: parse): ~p~n", [Form]),

    %% rec
    Next = erl_scan:tokens([], RestSourceCode, EndLoc, []),
    tylang_parse_impl(Next, [Form | Forms]);

tylang_parse_impl({done, {eof, _EndLoc}, _RestTokens}, Forms) ->
    lists:reverse(Forms);

tylang_parse_impl({done, {error, ErrorInfo, EndLoc}, _RestTokens}, _Forms) ->
    error({failed_to_tokenize, ErrorInfo, EndLoc});

tylang_parse_impl({more, _Conts}, _Forms) ->
    error(unexpected_cont).
