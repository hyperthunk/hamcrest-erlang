%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

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
    starts_with/1]).

-spec(anything/0 :: () -> fun((term()) -> true)).
anything() ->
    fun(_) -> true end.

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
