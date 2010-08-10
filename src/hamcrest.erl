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

-export([assert_that/2, assert_that/3]).
-export([message/4
        ,describe/2
        ,describe_spec/2
        ,heckle/2
        ,default_heckle/1
        ,default_heckle/2]).

assert_that(Value, MatchSpec, RunAfter) when is_function(RunAfter, 0) ->
  try assert_that(Value, MatchSpec)
  after RunAfter()
  end.

assert_that(Value, #'hamcrest.matchspec'{ matcher=MatchFunc }=MatchSpec) ->
  heckle(MatchSpec, Value),
  try assert_that(Value, MatchFunc)
  catch
    error:{assertion_failed, expected_exit} ->
      erlang:error({assertion_failed, describe_error(MatchSpec, {success, Value})});
    error:{assertion_failed, _} ->
      erlang:error({assertion_failed, describe(MatchSpec, Value)});
    Class:Reason ->
      erlang:error({assertion_failed, describe_error(MatchSpec, {Class, Reason})})
  end;
assert_that(Value, MatchSpec) when is_function(MatchSpec, 1) ->
  case MatchSpec(Value) of
    false ->
      case is_function(Value, 0) of
        true  -> erlang:error({assertion_failed, expected_exit});
        false -> erlang:error({assertion_failed, default_describe(Value)})
      end;
    _ -> true
  end.

heckle(MatchSpec, Actual) ->
  case application:get_env(hamcrest, heckle) of
    {ok, [M,F,A]} ->
      Argv = [MatchSpec, Actual],
      apply(M, F, [Argv|A]);
    _ ->
      default_heckle(MatchSpec, Actual),
      ok
  end.

default_heckle(Args) ->
  apply(?MODULE, default_heckle, Args).

default_heckle(#'hamcrest.matchspec'{ expected=Exp }=_Matcher, Actual) ->
  io:format("about to apply matcher [~p] against actual input [~p]~n", [Exp, Actual]).

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
  lists:flatten(io_lib:format(Desc, [Expected, Actual])).

describe_spec(Actual, [#'hamcrest.matchspec'{}|Rest]=Matchers) ->
  string:join([describe(hd(Matchers), Actual)|describe_spec(Actual, Rest)], "\n");
describe_spec(_, []) -> "".

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
