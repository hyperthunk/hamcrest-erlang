%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(hamcrest_matchers_SUITE).

%% -include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("triq/include/triq.hrl").
-include("../include/test.hrl").

-import(hamcrest_matchers, [
    anything/0,
    is/1,
    is_not/1,
    equal_to/1,
    exactly_equal_to/1]).

-compile(export_all).

all() -> ?CT_REGISTER_TESTS(?MODULE).

is_not_evaluates_to_logical_negation_of_underlying_matcher(_) ->
    P = ?FORALL(X, {any(), any()},
            ((is_not(equal_to(X)))(X) == not((equal_to(X))(X)))),
	true = ?EQC(P).

anything_should_always_match(_) ->
    P = ?FORALL(X, any(),
            true == (is(anything()))(X)),
	true = ?EQC(P).

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

exactly_equal_to_works_on_types_and_values(_) ->
    true = (exactly_equal_to(atom))(atom),
    false = (exactly_equal_to(atom))("atom"),
    true = (exactly_equal_to(1))(1),
    false = (exactly_equal_to(1))(1.0).
