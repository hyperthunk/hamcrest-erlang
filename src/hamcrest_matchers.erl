%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

-module(hamcrest_matchers).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([equal_to/1]).

-spec(equal_to/1 :: (Y) -> fun((Y) -> boolean())).
equal_to(Y) -> fun(X) -> X == Y end.
