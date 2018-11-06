%% -----------------------------------------------------------------------------
%%
%% Hamcrest Erlang.
%%
%% Copyright (c) 2017 Karolis Petrauskas (k.petrauskas@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @author Karolis Petrauskas <k.petrauskas@gmail.com>
%% @copyright 2017 Karolis Petrauskas.
%% @doc Hamcrest parse transform.
%% @reference See <a href="http://code.google.com/p/hamcrest/">Hamcrest</a>
%% for more information.
%% -----------------------------------------------------------------------------
-module(hamcrest_transform).
-export([parse_transform/2]).


%%  @doc
%%  This parse transform imports the hamcrest matchers if they are not imported yet.
%%  It also expands `assertThat' and `assertThat_' calls to the corresponding assertions.
%%
parse_transform(Forms, _Opts) ->
    ok = init_var_names(),
    FormsWithImport = ensure_import(Forms),
    lists:map(fun transform_asserts_form/1, FormsWithImport).


%%  @private
%%  Check, if there exists an import for the matchers.
%%
have_import(Forms) ->
    [] =/= [ I || I = {attribute, _Line, import, {hamcrest_matchers, _Imported}} <- Forms ].


%%  @private
%%  Add import for matchers, if they are missing.
%%
ensure_import(Forms) ->
    case have_import(Forms) of
        true  -> Forms;
        false -> add_import_form(Forms)
    end.


%%  @private
%%  Add import after the module attribute.
%%
add_import_form([]) ->
    [];

add_import_form([{attribute, L, module, _Name} = Form | Other]) ->
    Imported = hamcrest_matchers:module_info(exports) -- [{module_info,0}, {module_info,1}],
    ImportForm = {attribute, L, import, {hamcrest_matchers, Imported}},
    [Form, ImportForm | Other];

add_import_form([Form | Other]) ->
    [Form | add_import_form(Other)].


%%  @private
%%  Transform `assertThat', traverse functions.
%%
transform_asserts_form({function, Line, Name, Arity, Clauses}) ->
    {function, Line, Name, Arity, lists:map(fun transform_asserts_clause/1, Clauses)};

transform_asserts_form(Form) ->
    Form.


%%  @private
%%  Transform `assertThat', traverse function clauses.
%%
transform_asserts_clause({clause, Line, Head, Guard, Exprs}) ->
    {clause, Line, Head, Guard, lists:map(fun transform_asserts_expr/1, Exprs)}.


%%  @private
%%  Transform `assertThat', traverse expressions.
%%
transform_asserts_expr({call, LineC, {atom, _LineA, assertThat}, [_Value, _Expected] = Args}) ->
    transform_assert_call(LineC, Args);

transform_asserts_expr({call, LineC, {atom, _LineA, assertThat_}, [_Value, _Expected] = Args}) ->
    {'fun', LineC, {clauses, [{clause, LineC, [], [], [
        transform_assert_call(LineC, Args)
    ]}]}};

transform_asserts_expr(Expr) when is_tuple(Expr) ->
    list_to_tuple(lists:map(fun transform_asserts_expr/1, tuple_to_list(Expr)));

transform_asserts_expr(Expr) when is_list(Expr) ->
    lists:map(fun transform_asserts_expr/1, Expr);

transform_asserts_expr(Expr) ->
    Expr.


%%  @private
%%
%%  Transforms `assertThat(Value, MatchSpec)' to
%%  ```
%%      case hamcrest:check(Value, MatchSpec) of
%%          true                      -> true;
%%          {assertion_failed, __V_X} -> erlang:error({assertion_failed, __V_X});
%%          __V_X                     -> erlang:error({assertion_failed, __V_X})
%%      end
%%  '''
%%
%%  The transformation differs from the `?assertThat(Value, MatchSpec)' macro
%%  in order to have stack trace without the temporary function on top ot it.
%%
transform_assert_call(Line, Args) ->
    VarName = next_var_name(),
    CallErlangError = {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, error}}, [
        {tuple, Line, [
            {atom, Line, assertion_failed},
            {var, Line, VarName}
        ]}
    ]},
    {'case', Line, {call,Line, {remote,Line,{atom,Line,hamcrest},{atom,Line,check}}, Args}, [
        {clause, Line, [{atom, Line, true}], [], [
            {atom, Line, true}
        ]},
        {clause, Line, [{tuple, Line, [{atom, Line, assertion_failed}, {var, Line, VarName}]}], [], [
            CallErlangError
        ]},
        {clause, Line, [{var, Line, VarName}], [], [
            CallErlangError
        ]}
    ]}.


%%  @private
%%  Reset variable counter.
%%
init_var_names() ->
    _ = erlang:put({?MODULE, var_num}, 0),
    ok.


%%  @private
%%  Create new variable name.
%%
next_var_name() ->
    VarNum = erlang:get({?MODULE, var_num}) + 1,
    erlang:put({?MODULE, var_num}, VarNum),
    list_to_atom("__V_" ++ integer_to_list(VarNum)).


