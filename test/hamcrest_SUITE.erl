
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
-include_lib("proper/include/proper.hrl").
-include("../include/test.hrl").
-include("../include/hamcrest.hrl").

-compile(export_all).

all() -> ?CT_REGISTER_TESTS(?MODULE).

assert_that_always_passes_input_to_matcher_fun(_) ->
  P = ?FORALL(X, any(),
    begin
      F = fun(Y) -> Y == X end,
      assert_that(X, F)
    end),
    ?EQC(P).

assert_that_ignores_test_descriptions_when_matchers_pass(_) ->
  P = ?FORALL(X, any(),
    begin
      F = fun(Y) -> Y == X end,
      assert_that(X, #'hamcrest.matchspec'{ matcher=F, desc="" })
    end),
    ?EQC(P).

assert_that_returns_true_from_match_success(_) ->
  P = ?FORALL(X, any(),
    assert_that(X, is(X))),
  ?EQC(P).

%% matchers_can_return_matchresults(_) ->

failing_assertions_throw_exceptions(_) ->
  MF = is(equal_to(2)),
  ?assertException(error, {assertion_failed, "Expected a value equal to [2], but was [1]"},
    assert_that(1, MF)).

three_arg_assert_that_always_runs_supplied_fun(_) ->
  AfterFun = fun() -> put(after_run, wasrun) end,
  catch( ?assertThat(1, is(equal_to(2)), AfterFun) ),
  WasRun = get(after_run),
  ?assertThat(WasRun, is(equal_to(wasrun))).
