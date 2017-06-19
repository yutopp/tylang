-module(tylang_transpiler).

-export([transpile/1]).

%%
transpile(Forms) ->
    to_erl_forms_impl(Forms, [], []).

to_erl_forms_impl([Form|Rest], FormsAcc0, _Unused) ->
    ErlForm = to_erl_term(Form),
    to_erl_forms_impl(Rest, [ErlForm|FormsAcc0], _Unused);

to_erl_forms_impl([], FormsAcc, _Unused) ->
    lists:reverse(FormsAcc).

%%
to_erl_term({function, Loc, Name, Arity, Clauses}) ->
    {function, Loc, Name, Arity, [to_erl_term(C) || C <- Clauses]};
to_erl_term({clause, Loc, Params, Guards, _ReturnType, Exprs}) ->
    {clause, Loc, [to_erl_param(P) || P <- Params], Guards, Exprs};
to_erl_term(Form) ->
    Form.

to_erl_param({typed, Expr, _Type}) ->
    Expr;
to_erl_param(Expr) ->
    Expr.

%%%% Erlang/OTP 19.0
%%to_erl_forms_impl([Form|Rest], FormsAcc0) ->
%%    {FormsAcc1, AttrsAcc1} =
%%        case to_erl_term(Form) of
%%            {attribute, _, _, _} = Attr ->
%%                {FormsAcc0, [Attr|AttrsAcc0]};
%%            {function, _, _, _, _} = Func ->
%%                {[Func|FormsAcc0], AttrsAcc0}
%%        end,
%%    AttrsAcc2 = case to_erl_ext_form(Form) of
%%                    undefined -> AttrsAcc1;
%%                    ErlExtForm -> [ErlExtForm|AttrsAcc1]
%%                end,
%%    to_erl_forms_impl(Rest, FormsAcc1, AttrsAcc2);
%%
%%to_erl_forms_impl([], FormsAcc, AttrsAcc) ->
%%    lists:reverse(AttrsAcc) ++ lists:reverse(FormsAcc).
%%
%%%%
%%to_erl_term({function, Loc, Name, Arity, Clauses}) ->
%%    {function, Loc, Name, Arity, [to_erl_term(C) || C <- Clauses]};
%%to_erl_term({clause, Loc, Params, Guards, _ReturnType, Exprs}) ->
%%    {clause, Loc, [to_erl_param(P) || P <- Params], Guards, Exprs};
%%to_erl_term(Form) ->
%%    Form.
%%
%%to_erl_param({typed, Expr, _Type}) ->
%%    Expr;
%%to_erl_param(Expr) ->
%%    Expr.
%%
%%%%
%%to_erl_ext_form({function, Loc, Name, Arity, Clauses}) ->
%%    {attribute, Loc, tylang_def, {Name, Arity, Clauses}};
%%to_erl_ext_form(_) ->
%%    undefined.
