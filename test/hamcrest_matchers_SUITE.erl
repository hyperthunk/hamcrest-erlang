%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(hamcrest_matchers_SUITE).

%% -include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("triq/include/triq.hrl").
-include("../include/test.hrl").

-import(hamcrest_matchers, [equal_to/1]).

-compile(export_all).

all() ->
    All = ?CT_REGISTER_TESTS(?MODULE),
    ct:pal("registering ~p~n", [All]),
    All.

reflexivity_of_equal_to(_) ->
	%%X = 10,
    %%?assertMatch(true, (equal_to(X))(X)).
    P = ?FORALL(X, any(),
            ?IMPLIES(X == X,
                true == (equal_to(X))(X))),
	triq:check(P).
