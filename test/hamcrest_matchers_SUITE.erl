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
    any_of/1,
    is/1,
    is_not/1,
    equal_to/1,
    exactly_equal_to/1,
    greater_than/1,
    greater_than_or_equal_to/1,
    less_than/1,
    less_than_or_equal_to/1,
    contains_string/1]).

-compile(export_all).

all() -> ?CT_REGISTER_TESTS(?MODULE).

anything_always_matches(_) ->
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

is_not_evaluates_to_logical_negation_of_underlying_matcher(_) ->
    P = ?FORALL(X, {any(), any()},
            ((is_not(equal_to(X)))(X) == not((equal_to(X))(X)))),
	true = ?EQC(P).

is_not_provides_convenient_shortcut_for_not_equal_to(_) ->
    P = ?FORALL({X, Y}, {any(), any()},
            (is_not(X))(Y) == not((equal_to(X))(Y))),
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

any_of_checks_the_logical_disjunction_of_a_list_of_matchers(_) ->
    P = ?FORALL(XS, list(boolean()),
            begin
                M = lists:map(fun(E) -> fun(_) -> E end end, XS),
                lists:member(true, XS) == (any_of(M))(ignored)
            end),
	true = ?EQC(P).

greater_than_should_behave_like_built_in_operator(_) ->
    P = ?FORALL({X, Y},
            {oneof([int(), real()]), oneof([int(), real()])},
                (Y > X) == (greater_than(X))(Y)),
	true = ?EQC(P).

greater_than_or_equal_to_should_behave_like_built_in_operator(_) ->
    P = ?FORALL({X, Y},
            {oneof([int(), real()]), oneof([int(), real()])},
                (Y >= X) == (greater_than_or_equal_to(X))(Y)),
	true = ?EQC(P).

less_than_should_behave_like_built_in_operator(_) ->
    P = ?FORALL({X, Y},
            {oneof([int(), real()]), oneof([int(), real()])},
                (Y < X) == (less_than(X))(Y)),
	true = ?EQC(P).

less_than_or_equal_to_should_behave_like_built_in_operator(_) ->
    P = ?FORALL({X, Y},
            {oneof([int(), real()]), oneof([int(), real()])},
                (Y =< X) == (less_than_or_equal_to(X))(Y)),
	true = ?EQC(P).

contains_string_should_get_proper_subset_in_all_cases(_) ->
    P = ?FORALL({X, Y}, {string(), int()},
            ?IMPLIES(length(X) > 0 andalso
                     length(X) >= Y andalso
                     Y > 0,
            begin
                SubStr = string:left(X, Y),
                true = (contains_string(SubStr))(X)
            end)),
	true = ?EQC(P).
