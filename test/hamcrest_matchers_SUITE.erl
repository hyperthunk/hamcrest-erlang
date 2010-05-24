%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(hamcrest_matchers_SUITE).

%% -include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("triq/include/triq.hrl").
-include("../include/test.hrl").

-import(hamcrest_matchers, [is/1, equal_to/1]).

-compile(export_all).

all() -> ?CT_REGISTER_TESTS(?MODULE).

is_matches_the_same_way_as_the_underlying_matcher(_) ->
    P = ?FORALL(X, any(),
            (is(equal_to(X)))(X) == (equal_to(X))(X)),
	true = ?EQC(P).

is_provides_convenient_shortcut_for_equal_to(_) ->
    P = ?FORALL(X, any(),
            (is(X))(X) == (equal_to(X))(X)),
	true = ?EQC(P).

reflexivity_of_equal_to(_) ->
    P = ?FORALL(X, any(),
            ?IMPLIES(X == X,
                true == (equal_to(X))(X))),
	true = ?EQC(P).

symmetry_of_equal_to(_) ->
    P = ?FORALL({X, Y}, {int(), int()},
            ?IMPLIES(X == Y,
                (equal_to(Y))(X))),
	true = ?EQC(P).
