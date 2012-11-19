
%% -----------------------------------------------------------------------------
%%
%% Hamcrest Erlang.
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
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
%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2010 Tim Watson.
%% -----------------------------------------------------------------------------

%% module annotations
-module(hamcrest_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("test.hrl").
-include("qc.hrl").
-include("../include/hamcrest.hrl").

-compile(export_all).

all() -> ?CT_REGISTER_TESTS(?MODULE).

assert_that_always_passes_input_to_matcher_fun(_) ->
  P = ?FORALL(X, something(),
    begin
      M = ?MATCHER(fun(Y) -> Y == X end, equality, X),
      assert_that(X, M)
    end),
    ?EQC(P).

assert_that_ignores_test_descriptions_when_matchers_pass(_) ->
  P = ?FORALL(X, something(),
    begin
      F = fun(Y) -> Y == X end,
      assert_that(X, #'hamcrest.matchspec'{ matcher=F, desc="" })
    end),
    ?EQC(P).

assert_that_returns_true_from_match_success(_) ->
  P = ?FORALL(X, something(),
    assert_that(X, is(X))),
  ?EQC(P).

%% matchers_can_return_matchresults(_) ->

failing_assertions_throw_exceptions(_) ->
  MF = is(equal_to(2)),
  ?assertException(
     error,
     {assertion_failed,
      [{expected,2},
       {actual,1},
       {matcher,equal_to}]},
    assert_that(1, MF)).

three_arg_assert_that_always_runs_supplied_fun(_) ->
  AfterFun = fun() -> put(after_run, wasrun) end,
  catch( ?assertThat(1, is(equal_to(2)), AfterFun) ),
  WasRun = get(after_run),
  ?assertThat(WasRun, is(equal_to(wasrun))).

is_matcher(_) ->
  ?assertThat(hamcrest:is_matcher(blah), is_false()),
  ?assertThat(hamcrest:is_matcher(contains_string("1234")), is_true()),
  ?assertThat(hamcrest:is_matcher(has_same_contents_as("1234")), is_true()),
  ?assertThat(hamcrest:is_matcher(foreach(is_true())), is_true()),
  ?assertThat(hamcrest:is_matcher(has_length(10)), is_true()),
  ?assertThat(hamcrest:is_matcher(all_of([is_true(), is_false()])), is_true()),
  ?assertThat(hamcrest:is_matcher(any_of([is_true(), is_false()])), is_true()),
  ?assertThat(hamcrest:is_matcher(reverse_match_mfa(erlang, is_process_alive, [])), is_true()),
  P = ?FORALL(X, list(something()),
          begin
              [case lists:member(F, [foreach, all_of, any_of,
                                     reverse_match_mfa, has_length,
                                     has_same_contents_as, check_member,
                                     check_isempty, module_info,
                                     contains_string]) of
                  true ->
                      ok;
                  _ ->
                      Fn = fun hamcrest_matchers:F/A,
                      Matcher = apply(Fn, lists:duplicate(A, X)),
                      io:format("checking ~p for ~p~n", [Matcher, {F, A, Fn}]),
                      ?assertThat(hamcrest:is_matcher(Matcher),
                                 is_true())
               end || {F, A} <- hamcrest_matchers:module_info(exports)],
               true
          end),
  ?EQC(P).

-ifdef('eqc').
something() -> eqc_gen:oneof([int(), nat(), list(char), binary()]).
-else.
something() -> any().
-endif.
