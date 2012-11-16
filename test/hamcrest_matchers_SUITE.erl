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
%% -----------------------------------------------------------------------------

%% module annotations
-module(hamcrest_matchers_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("qc.hrl").
-include("../include/hamcrest.hrl").

-compile(export_all).

-include("test.hrl").

all() ->
    application:set_env(hamcrest, heckle, [?MODULE, say, []]),
    ?CT_REGISTER_TESTS(?MODULE).

say(MS, Actual) ->
    io:format("checking ~p against ~p~n", [Actual, MS]).

anything_always_matches(_) ->
    P = ?FORALL(X, binary(),
            true == assert_that(X, is(anything()))),
    ?EQC(P).

is_matches_the_same_way_as_the_underlying_matcher(_) ->
    P = ?FORALL(X, binary(),
            is(equal_to(X)) == equal_to(X)),
    ?EQC(P).

is_provides_convenient_shortcut_for_equal_to(_) ->
    P = ?FORALL(X, binary(),
            is(X) == equal_to(X)),
    ?EQC(P).

is_not_evaluates_to_logical_negation_of_underlying_matcher(_) ->
    P = ?FORALL(X, {binary(), binary()},
            begin
                #'hamcrest.matchspec'{matcher=F1} = equal_to(X),
                #'hamcrest.matchspec'{matcher=F2} = is_not(equal_to(X)),
                F1(X) == not(F2(X))
            end),
    ?EQC(P).

is_not_provides_convenient_shortcut_for_not_equal_to(_) ->
    P = ?FORALL({X, Y}, {binary(), binary()},
            begin
                #'hamcrest.matchspec'{matcher=F1} = equal_to(X),
                #'hamcrest.matchspec'{matcher=F2} = is_not(X),
                F1(X) == not(F2(X))
            end),
    ?EQC(P).

reflexivity_of_equal_to(_) ->
    P = ?FORALL(X, binary(),
                begin
                    Y = X,
                    assert_that(X, equal_to(Y))
                end),
    ?EQC(P).

symmetry_of_equal_to(_) ->
    P = ?FORALL(X, int(),
                begin
                    Y = X,
                    assert_that(X, equal_to(Y)),
                    Z = Y,
                    assert_that(Z, equal_to(X))
                end),
    ?EQC(P).

exactly_equal_to_works_on_types_and_values(_) ->
    true = assert_that(atom, exactly_equal_to(atom)),
    ?assertException(
       error,
       {assertion_failed,
        [{expected,atom},
         {actual,"atom"},
         {matcher,exactly_equal_to}]},
       assert_that("atom", exactly_equal_to(atom))),
    true = assert_that(1, exactly_equal_to(1)),
    ?assertException(
       error,
       {assertion_failed,
        [{expected, 1},
         {actual, 1.0},
         {matcher, exactly_equal_to}]},
        assert_that(1.0, exactly_equal_to(1))).

any_of_checks_the_logical_disjunction_of_a_list_of_matchers(_) ->
    P = ?FORALL(XS, list(char()),
            ?IMPLIES(length(XS) > 0,
            begin
                M = lists:map(fun(_) -> fun(_) -> true end end, XS),
                assert_that(ignored, any_of(M))
            end)),
    ?EQC(P).

will_fail_asserts_failure(_) ->
    F = fun() -> erlang:error({assertion_failed, "Unexpected value"}) end,
    assert_that(F, will_fail()).

will_fail_asserts_failure_against_given_condition(_) ->
    F = fun() -> erlang:error({nomatch, "Unexpected value"}) end,
    assert_that(F, will_fail(error, {nomatch, "Unexpected value"})).

%% TODO: check will_fail during 'failure' conditions
will_fail_should_fail_if_the_operation_succeeds(_) ->
  P = ?FORALL(X, binary(),
      begin
        F = fun() -> X end,
        FI = erlang:fun_info(F),
        ok ==
        ?assertException(error,
            {assertion_failed,
              [{expected,
                {error,{nomatch,unexpected_value}}},
               {actual, FI},
               {matcher,will_fail}]},
            assert_that(F, will_fail(error, {nomatch, unexpected_value})))
      end),
  ?EQC(P).

greater_than_should_behave_like_built_in_operator(_) ->
  P = ?FORALL({X, Y}, {oneof([int(), real()]), oneof([int(), real()])},
        ?IMPLIES(Y > X,
        assert_that(Y, greater_than(X)))),
    ?EQC(P).

greater_than_should_fail_with_error_unlike_built_in_operator(_) ->
  P = ?FORALL({X, Y}, {oneof([int(), real()]), oneof([int(), real()])},
      ?IMPLIES(Y < X,
      begin
        try (assert_that(Y, greater_than(X))) of
          Term ->
            ct:pal("Term = ~p", [Term]), false
        catch error:Reason ->
          assert_that(Reason,
                equal_to({assertion_failed,
                            [{expected, X},
                             {actual, Y},
                             {matcher, greater_than}]}))
        end
      end)),
    ?EQC(P).

greater_than_or_equal_to_should_behave_like_built_in_operator(_) ->
  P = ?FORALL({X, Y},
      {oneof([int(), real()]), oneof([int(), real()])},
        begin
          #'hamcrest.matchspec'{matcher=M} = greater_than_or_equal_to(X),
          (Y >= X) == M(Y)
        end),
    ?EQC(P).

less_than_should_behave_like_built_in_operator(_) ->
  P = ?FORALL({X, Y},
        {oneof([int(), real()]), oneof([int(), real()])},
        begin
          #'hamcrest.matchspec'{matcher=M} = less_than(X),
          (Y < X) == M(Y)
        end),
    ?EQC(P).

less_than_or_equal_to_should_behave_like_built_in_operator(_) ->
  P = ?FORALL({X, Y},
        {oneof([int(), real()]), oneof([int(), real()])},
        begin
          #'hamcrest.matchspec'{matcher=M} = less_than_or_equal_to(X),
          (Y =< X) == M(Y)
        end),
    ?EQC(P).

contains_string_should_get_proper_subset_in_all_cases(_) ->
  P = ?FORALL(X, noshrink(non_empty(list(char()))),
        ?IMPLIES(length(X) > 0,
        begin
          Y = round(length(X) / 2),
          SubStr = string:left(X, Y),
          true = assert_that(X, contains_string(SubStr))
        end)),
  ?EQC(P).

contains_string_should_not_create_matcher_for_empty_strings(_) ->
  ?assertException(error, function_clause, contains_string([])).

contains_string_should_not_match_empty_string(_) ->
  P = ?FORALL(X, list(char()),
        ?IMPLIES(length(X) > 0,
            assert_that(fun() ->
                            assert_that(X, contains_string("foo bar baz"))
                        end, will_fail())
        )),
  ?EQC(P).

starts_with_should_only_match_first_portion_of_string(_) ->
  P = ?FORALL(Xs, noshrink(non_empty(list(char()))),
        ?IMPLIES(length(Xs) > 1,
        begin
          Y = erlang:max(round(length(Xs) / 2), 2),
          LStr = string:left(Xs, Y),
          RStr = string:right(Xs, Y),
          true = assert_that(Xs, starts_with(LStr)),
          if RStr /= LStr ->
                  assert_that(match(Xs, starts_with(RStr)),
                              is_false());
             true -> true
          end
        end)),
    ?EQC(P).

ends_with_should_only_match_last_portion_of_string(_) ->
  P = ?FORALL(Xs, noshrink(non_empty(list(char()))),
        ?IMPLIES(length(Xs) > 0,
        begin
          Y = round(length(Xs) / 2),
          LStr = string:left(Xs, Y),
          RStr = string:right(Xs, Y),
          case (assert_that(Xs, ends_with(RStr))) of
            true -> true;
            false ->
                ct:pal("X = ~p~n", [Xs]),
                ct:pal("Y = ~p~n", [Y]),
                ct:pal("RStr = ~p~n", [RStr]),
                false
          end
          %%Val = (ends_with(LStr))(X),
          %%not Val
        end)),
    ?EQC(P).

has_length_should_match_length(_) ->
  P = ?FORALL(XS, list(int()),
      begin
        assert_that(XS, has_length(length(XS)))
      end),
    ?EQC(P).

match_mfa(_) ->
  P = ?FORALL(X, list(char()),
        ?IMPLIES(length(X) > 0,
        begin
          assert_that(hd(X), reverse_match_mfa(lists, member, [X]))
        end)),
    ?EQC(P).

reverse_match_mfa_should_flip_its_arguments(_) ->
    P = ?FORALL(X, list(char()),
        ?IMPLIES(length(X) > 0,
        begin
            Head = hd(X),
            assert_that(X, contains_member(Head)),
            assert_that(sets:from_list(X), contains_member(Head)),
            assert_that(gb_sets:from_list(X), contains_member(Head)),
            assert_that(ordsets:from_list(X), contains_member(Head))
        end)),
    ?EQC(P).

has_same_contents_as_should_ignore_order(_) ->
    P = ?FORALL(X, non_empty(list(int())),
        ?IMPLIES(length(X) > 0,
        assert_that(lists:reverse(X), has_same_contents_as(X)))),
    ?EQC(P).

has_same_contents_as_should_recognise_singular_errors(_) ->
    P = ?FORALL(X, noshrink(non_empty(list(int()))),
        ?IMPLIES(length(sets:to_list(sets:from_list(X))) > 0,
        begin
            Xs = sets:to_list(sets:from_list(X)),
            assert_that(
              fun() -> assert_that(tl(Xs), has_same_contents_as([a, b, c])) end,
              will_fail())
        end)),
    ?EQC(P).

has_same_contents_as_should_work_for_empty_lists(_) ->
    ?assertThat([], has_same_contents_as([])).

match_mfa_should_fail_if_mf_is_invalid(_) ->
    NoSuchMod = non_existing, NoSuchFunc = nor_i,
    #'hamcrest.matchspec'{matcher=M} = match_mfa(NoSuchMod, NoSuchFunc, []),
    M(any_input) == false.

match_mfa_should_fail_if_func_is_invalid(_) ->
    NoSuchFunc = this_function_doesnt_exist,
    #'hamcrest.matchspec'{matcher=M} = match_mfa(lists, NoSuchFunc, []),
    M(any_input) == false.

match_is_alive_should_identify_correct_process_status(_) ->
    Loop = fun(L) -> L(L) end,
    OkPid = spawn(fun() -> Loop(Loop) end),
    Sender = self(),
    BadPid = spawn(fun() -> ct:pal("~p dying...", [self()]), Sender ! ready, exit(normal) end),
    receive ready -> ok end,
    ?assertThat(OkPid, isalive()),
    ?assertThat(BadPid, isdead()).

is_empty_works_for_lists(_) ->
    ?assertThat([], isempty()),
    Empty = fun hamcrest:isempty/0,
    ?assertThat([1,2,3], is_not(Empty)).

is_empty_works_for_tuples(_) ->
    ?assertThat({}, isempty()),
    Empty = fun hamcrest:isempty/0,
    ?assertThat({ok, server_id}, is_not(Empty)).

is_empty_works_for_sets(_) ->
    ?assertThat(sets:new(), isempty()),
    Empty = fun hamcrest:isempty/0,
    ?assertThat(sets:from_list([1,2,3]), is_not(Empty)).

is_empty_works_for_gb_sets(_) ->
    ?assertThat(gb_sets:new(), isempty()),
    Empty = fun hamcrest:isempty/0,
    ?assertThat(gb_sets:from_list([1,2,3]), is_not(Empty)).

is_empty_pukes_for_other_inputs(_) ->
    ?assertThat(
        fun() -> ?assertThat(10, isempty()) end,
        will_fail()
    ).
