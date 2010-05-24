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
    exactly_equal_to/1]).

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
