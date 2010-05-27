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
%% @doc Hamcrest Matchers
%% @reference See <a href="http://code.google.com/p/hamcrest/">Hamcrest</a>
%% for more information.
%% -----------------------------------------------------------------------------

-module(hamcrest_matchers).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([
    anything/0,
    any_of/1,
    is/1,
    is_not/1,
    equal_to/1,
    exactly_equal_to/1,
    greater_than/1,
    greater_than_or_equal_to/1,
    less_than/1,
    less_than_or_equal_to/1,
    contains_string/1,
    starts_with/1,
    ends_with/1]).

-spec(anything/0 :: () -> fun((term()) -> true)).
anything() ->
    fun(_) -> true end.

-spec(any_of/1 :: (list(fun((term()) -> boolean()))) -> fun((term()) -> boolean())).
any_of(Matchers) when is_list(Matchers) ->
    fun(X) -> lists:member(true, [ M(X) || M <- Matchers ]) end.

-spec(equal_to/1 :: (Y) -> fun((Y) -> boolean())).
equal_to(Y) -> fun(X) -> X == Y end.

-spec(exactly_equal_to/1 :: (Y) -> fun((Y) -> boolean())).
exactly_equal_to(X) ->
    fun(Y) -> X =:= Y end.

-spec(is/1 :: (Y::fun((X::term()) -> boolean())) -> fun((X::term()) -> boolean());
              (Y) -> fun((Y) -> boolean())).
is(Matcher) when is_function(Matcher) ->
    Matcher;
is(Term) ->
    equal_to(Term).

-spec(is_not/1 :: (Y::fun((X::term()) -> boolean())) -> fun((X::term()) -> boolean());
                  (Y) -> fun((Y) -> boolean())).
is_not(Matcher) when is_function(Matcher, 1) ->
    fun(X) -> not(Matcher(X)) end;
is_not(Term) ->
    is_not(equal_to(Term)).

-spec(greater_than/1 :: (number()) -> fun((number()) -> boolean())).
greater_than(X) ->
    fun(Y) -> Y > X end.

-spec(greater_than_or_equal_to/1 :: (number()) -> fun((number()) -> boolean())).
greater_than_or_equal_to(X) ->
    fun(Y) -> Y >= X end.

-spec(less_than/1 :: (number()) -> fun((number()) -> boolean())).
less_than(X) ->
    fun(Y) -> Y < X end.

-spec(less_than_or_equal_to/1 :: (number()) -> fun((number()) -> boolean())).
less_than_or_equal_to(X) ->
    fun(Y) -> Y =< X end.

-spec(contains_string/1 :: (string()) -> fun((string()) -> boolean())).
contains_string([_|_]=X) ->
    fun(Y) -> string:str(Y, X) > 0 end.

-spec(starts_with/1 :: (string()) -> fun((string()) -> boolean())).
starts_with(X) ->
    fun(Y) -> string:str(Y, X) == 1 end.

-spec(ends_with/1 :: (string()) -> fun((string()) -> boolean())).
ends_with(X) ->
    fun(Y) -> string:equal(string:right(Y, length(X)), X) end.
