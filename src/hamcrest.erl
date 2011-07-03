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
%% @doc Hamcrest API
%% @reference See <a href="http://code.google.com/p/hamcrest/">Hamcrest</a>
%% for more information.
%% -----------------------------------------------------------------------------

-module(hamcrest).

-include("hamcrest_internal.hrl").

-export([match/2, match/3, check/2,
         assert_that/2, assert_that/3]).
-export([message/4
        ,describe/2
        ,describe_spec/2
        ,heckle/2]).

match(Value, MatchSpec) ->
  match(Value, MatchSpec, fun() -> ok end).

match(Value, MatchSpec, RunAfter) ->
  catch( assert_that(Value, MatchSpec, RunAfter) ) == true.

assert_that(Value, MatchSpec, RunAfter) when is_function(RunAfter, 0) ->
  %% TODO: verify that this doesn't break the semantics of assert_that *and* match!! :/
  try assert_that(Value, MatchSpec)
  after RunAfter()
  end.

assert_that(Value, MatchSpec) ->
  case check(Value, MatchSpec) of
    {assertion_failed, _}=Failure ->
      erlang:error(Failure);
    true ->
      true
  end.

check(Value, #'hamcrest.matchspec'{ matcher=MatchFunc }=MatchSpec) ->
  heckle(MatchSpec, Value),
  try check(Value, MatchFunc) of
    true -> true;
    {assertion_failed, expected_exit} ->
      {assertion_failed, describe_error(MatchSpec, {success, Value})};
    {assertion_failed, _} ->
      {assertion_failed, describe(MatchSpec, Value)};
    {assertion_override, Err} ->
      {assertion_failed, Err};
    What ->
      {assertion_failed, What}
  catch
    Class:Reason ->
      {assertion_failed, describe_error(MatchSpec, {Class, Reason})}
  end;
check(Value, MatchSpec) when is_function(MatchSpec, 1) ->
  case MatchSpec(Value) of
    false ->
      case is_function(Value, 0) of
        %% TODO: ticket #
        true  -> {assertion_failed, expected_exit};
        false -> {assertion_failed, default_describe(Value)}
      end;
    _ -> true
  end.

heckle(MatchSpec, Actual) ->
  case application:get_env(hamcrest, heckle) of
    {ok, [M,F,A]} ->
      Argv = [MatchSpec, Actual],
      apply(M, F, [Argv|A]);
    _ ->
      ok
  end.

%% default_heckle(#'hamcrest.matchspec'{ expected=Exp }=_Matcher, Actual) ->
%%default_heckle(_) ->
%%  io:format("about to apply matcher [~p] against actual input [~p]~n", [Exp, Actual]).

message(string, Def, Expected, Actual) ->
  lists:flatten(io_lib:format("Expected a string ~s [~s], but was [~s]",
    [Def, Expected, Actual]));
message(error, Desc, Expected, {Class, Reason}) ->
  Actual = io_lib:format("unexpected exit with ~p:~p", [Class, Reason]),
  Desc(Expected, Actual);
message(_Type, _Def, Expected, Actual) when is_list(Expected)
                                      andalso is_record(hd(Expected), 'hamcrest.matchspec') ->
  lists:flatten(describe_spec(Actual, Expected));
message(Type, Def, Expected, Actual) when is_list(Def) ->
  lists:flatten(io_lib:format("Expected a ~p ~s [~p], but was [~p]",
    [Type, Def, Expected, Actual])).

default_describe(Value) ->
  describe(fun(X) ->
              message(value, "matching", Value, X)
           end, undefined, Value).

describe(#'hamcrest.matchspec'{ expected={anyof, Matchers} }, Actual) ->
  describe_spec(Actual, Matchers);
describe(#'hamcrest.matchspec'{ desc=Desc, expected=Expected }, Actual) ->
  describe(Desc, Expected, Actual).

describe(Desc, Expected, Actual) when is_function(Desc, 2) ->
  Desc(Expected, Actual);
describe(Desc, _, Actual) when is_function(Desc, 1) ->
  Desc(Actual);
describe(Desc, Expected, Actual) when is_list(Desc) ->
  lists:flatten(io_lib:format(Desc, [Expected, Actual]));
describe({oneof,{exit,error,exception}}, expected_fail, _) ->
  "Expected failure, but operation succeeded!";
describe(expected_fail, {Class, Reason}, _) ->
  describe("Expected ~p due to ~p, but operation succeeded!", Class, Reason).

describe_spec(Actual, Matchers) ->
  lists:flatten([ describe(Matcher, Actual) ++ "\n" || Matcher <- Matchers ]).

describe_error(#'hamcrest.matchspec'{ desc=expected_fail,
                                      expected={Class, Reason} },
                                               {ActualClass, ActualReason}) ->
  case ActualClass of
    success ->
      Msg = "expected exit with ~p:~p, but operation succeeded",
      lists:flatten(io_lib:format(Msg, [Class, Reason]));
    _ ->
      Msg = "expected exit with ~p:~p, but was [~p:~p]",
      lists:flatten(io_lib:format(Msg, [Class, Reason, ActualClass, ActualReason]))
  end;
describe_error(#'hamcrest.matchspec'{ desc=Desc,
                                      expected=Expected },
                                      {Class, Reason}) when is_function(Desc) ->
  message(error, Desc, Expected, {Class, Reason}).
