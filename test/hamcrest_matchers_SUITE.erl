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

%% module annotations
-module(hamcrest_matchers_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("triq/include/triq.hrl").
-include("../include/test.hrl").
-include("../include/hamcrest.hrl").

-compile(export_all).

all() -> ?CT_REGISTER_TESTS(?MODULE).

anything_always_matches(_) ->
    P = ?FORALL(X, any(),
            true == (is(anything()))(X)),
	?assertMatch(true, ?EQC(P)).

is_matches_the_same_way_as_the_underlying_matcher(_) ->
    P = ?FORALL(X, any(),
            (is(equal_to(X)))(X) == (equal_to(X))(X)),
	?assertMatch(true, ?EQC(P)).

is_provides_convenient_shortcut_for_equal_to(_) ->
    P = ?FORALL(X, any(),
            (is(X))(X) == (equal_to(X))(X)),
	?assertMatch(true, ?EQC(P)).

is_not_evaluates_to_logical_negation_of_underlying_matcher(_) ->
    P = ?FORALL(X, {any(), any()},
            ((is_not(equal_to(X)))(X) == not((equal_to(X))(X)))),
	?assertMatch(true, ?EQC(P)).

is_not_provides_convenient_shortcut_for_not_equal_to(_) ->
    P = ?FORALL({X, Y}, {any(), any()},
            (is_not(X))(Y) == not((equal_to(X))(Y))),
	?assertMatch(true, ?EQC(P)).

reflexivity_of_equal_to(_) ->
    P = ?FORALL(X, any(),
            ?IMPLIES(X == X,
                true == (equal_to(X))(X))),
	?assertMatch(true, ?EQC(P)).

symmetry_of_equal_to(_) ->
    P = ?FORALL({X, Y}, {int(), int()},
            ?IMPLIES(X == Y,
                (equal_to(Y))(X))),
	?assertMatch(true, ?EQC(P)).

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
	?assertMatch(true, ?EQC(P)).

greater_than_should_behave_like_built_in_operator(_) ->
    P = ?FORALL({X, Y},
            {oneof([int(), real()]), oneof([int(), real()])},
                (Y > X) == (greater_than(X))(Y)),
	?assertMatch(true, ?EQC(P)).

greater_than_or_equal_to_should_behave_like_built_in_operator(_) ->
    P = ?FORALL({X, Y},
            {oneof([int(), real()]), oneof([int(), real()])},
                (Y >= X) == (greater_than_or_equal_to(X))(Y)),
	?assertMatch(true, ?EQC(P)).

less_than_should_behave_like_built_in_operator(_) ->
    P = ?FORALL({X, Y},
            {oneof([int(), real()]), oneof([int(), real()])},
                (Y < X) == (less_than(X))(Y)),
	?assertMatch(true, ?EQC(P)).

less_than_or_equal_to_should_behave_like_built_in_operator(_) ->
    P = ?FORALL({X, Y},
            {oneof([int(), real()]), oneof([int(), real()])},
                (Y =< X) == (less_than_or_equal_to(X))(Y)),
	?assertMatch(true, ?EQC(P)).

contains_string_should_get_proper_subset_in_all_cases(_) ->
    P = ?FORALL({X, Y}, {string(), int()},
            ?IMPLIES(length(X) > 0 andalso
                     length(X) >= Y andalso
                     Y > 0,
            begin
                SubStr = string:left(X, Y),
                true = (contains_string(SubStr))(X)
            end)),
	?assertMatch(true, ?EQC(P)).

contains_string_should_not_create_matcher_for_empty_strings(_) ->
    ?assertError(function_clause, contains_string([])).

contains_string_should_not_match_empty_string(_) ->
    P = ?FORALL(X, string(),
            ?IMPLIES(length(X) > 0,
            false == (contains_string("foo bar baz"))(X))),
    ?assertMatch(true, ?EQC(P)).

starts_with_should_only_match_first_portion_of_string(_) ->
    P = ?FORALL({X, Y}, {string(), int()},
            ?IMPLIES(Y > 1 andalso
                     length(X) > Y andalso
                     string:left(X, Y) /= string:right(X, Y),
            begin
                LStr = string:left(X, Y),
                RStr = string:right(X, Y),
                true = (starts_with(LStr))(X),
                Val = (starts_with(RStr))(X),
                not Val
            end)),
	?assertMatch(true, ?EQC(P)).

ends_with_should_only_match_last_portion_of_string(_) ->
    P = ?FORALL({X, Y}, {string(), int()},
            ?IMPLIES(Y > 1 andalso length(X) >= Y,
            begin
                LStr = string:left(X, Y),
                RStr = string:right(X, Y),
                case (ends_with(RStr))(X) of
                    true -> true;
                    false ->
                        ct:pal("X = ~p~n", [X]),
                        ct:pal("Y = ~p~n", [Y]),
                        ct:pal("RStr = ~p~n", [RStr]),
                        false
                end
                %%Val = (ends_with(LStr))(X),
                %%not Val
            end)),
	?assertMatch(true, ?EQC(P)).
