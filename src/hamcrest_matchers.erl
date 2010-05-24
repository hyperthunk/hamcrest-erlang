%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

-module(hamcrest_matchers).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([is/1, equal_to/1]).

-spec(equal_to/1 :: (Y) -> fun((Y) -> boolean())).
equal_to(Y) -> fun(X) -> X == Y end.

-spec(is/1 :: (Y::fun((X::term()) -> boolean())) -> fun((X::term()) -> boolean());
              (Y) -> fun((Y) -> boolean())).
is(Matcher) when is_function(Matcher) ->
    Matcher;
is(Matcher) ->
    equal_to(Matcher).
