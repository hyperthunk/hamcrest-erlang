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

-module(hamcrest_matchers).
-author('Tim Watson <watson.timothy@gmail.com>').

-include("hamcrest_internal.hrl").

-import(hamcrest, [message/4]).

-export([
    all_of/1,
    anything/0,
    any_of/1,
    foreach/1,
    is/1,
    is_true/0,
    is_false/0,
    is_not/1,
    equal_to/1,
    exactly_equal_to/1,
    greater_than/1,
    greater_than_or_equal_to/1,
    less_than/1,
    less_than_or_equal_to/1,
    contains_string/1,
    starts_with/1,
    ends_with/1,
    will_fail/0,
    will_fail/2,
    has_length/1,
    has_same_contents_as/1,
    contains_member/1,
    matches_regex/1,
    match_mfa/3,
    match_mfa/4,
    reverse_match_mfa/3,
    reverse_match_mfa/4,
    isalive/0,
    isdead/0,
    isempty/0,
    check_isempty/1,
    check_member/2]).

-spec(will_fail/0 :: () -> matchspec(fun(() -> boolean()))).
will_fail() ->
    %% Matcher :: fun((fun(() -> any())) -> boolean())
    Matcher = fun(F) ->
        try F() of
            _ -> false
        catch _:_ -> true end
    end,
    #'hamcrest.matchspec'{
        matcher     = Matcher,
        desc        = will_fail,
        expected    = {oneof,{exit,error,exception}}
    }.

-spec(will_fail/2 :: (atom(), term()) -> matchspec(fun(() -> boolean()))).
will_fail(Type, Reason) ->
    %% Matcher :: fun((fun(() -> any())) -> boolean())
    Matcher = fun(F) ->
        try F() of
            _ -> false
        catch Type:Reason -> true end
    end,
    #'hamcrest.matchspec'{
        matcher     = Matcher,
        desc        = will_fail,
        expected    = {Type, Reason}
    }.

-spec(anything/0 :: () -> matchspec(term())).
anything() ->
    ?MATCHER(fun(_) -> true end, any, anything).

-spec(foreach/1 :: (matchspec(term())) -> matchspec(term())).
foreach(M) ->
    ?MATCHER(fun(L) -> drop_matches(M, L) == [] end, M,
             {foreach, M#'hamcrest.matchspec'.desc}).

drop_matches(Match, []) ->
    case hamcrest:match([], Match) of
        true -> [];
        _    -> false
    end;
drop_matches(Match, L) ->
    lists:dropwhile(fun(E) -> hamcrest:match(E, Match) end, L).

-spec(any_of/1 :: (list(matchfun(term())))  -> matchspec(term());
                  (list(matchspec(term()))) -> matchspec(term())).
any_of(Matchers) when is_list(Matchers) ->
    MatchFun =
    fun(M) when is_function(M) -> M;
     (#'hamcrest.matchspec'{matcher=F}) -> F
    end,
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> lists:member(true,
                                    [ (MatchFun(M))(X) || M <- Matchers ]) end,
        desc        = any_of,
        expected    = {any, Matchers}
    }.

%% TODO: older syntax for type specifications - we need to support
%% >= R13B for the most part...
-spec all_of(list(matchfun(A)))  -> matchspec(A) when A :: term();
            (list(matchspec(A))) -> matchspec(A) when A :: term().
all_of(Matchers) when is_list(Matchers) ->
    MatchFun = fun(M) when is_function(M) -> M;
       (#'hamcrest.matchspec'{matcher=F}) -> F
    end,
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> not(lists:member(false,
                                    [ (MatchFun(M))(X) || M <- Matchers ])) end,
        desc        = all_of,
        expected    = {all, Matchers}
    }.

-spec(equal_to/1 :: (A) -> matchspec(A) when A :: term()).
equal_to(Y) ->
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> X == Y end,
        desc        = equal_to,
        expected    = Y
    }.

-spec(exactly_equal_to/1 :: (term()) -> matchspec(term())).
exactly_equal_to(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> X =:= Y end,
        desc        = exactly_equal_to,
        expected    = X
    }.

-spec(is/1 :: (matchfun(term()))  -> matchspec(term());
              (matchspec(term())) -> matchspec(term());
              (any())             -> matchspec(any())).
is(Matcher) when is_record(Matcher, 'hamcrest.matchspec') ->
    Matcher;
is(Term) ->
    equal_to(Term).

-spec(is_true/0 :: () -> matchspec(boolean())).
is_true() ->
    equal_to(true).

-spec(is_false/0 :: () -> matchspec(boolean())).
is_false() ->
    is_not(equal_to(true)).

-spec(is_not/1 :: (matchfun(term()))  -> matchspec(term());
                  (matchspec(term())) -> matchspec(term());
                  (term())            -> matchspec(term())).
is_not(#'hamcrest.matchspec'{ matcher=MatchFun }=MatchSpec)
    when is_record(MatchSpec, 'hamcrest.matchspec') ->
  MatchSpec#'hamcrest.matchspec'{ matcher = (fun(X) -> not(MatchFun(X)) end) };
is_not(Term) ->
  is_not(equal_to(Term)).

-spec(greater_than/1 :: (number()) -> matchspec(number())).
greater_than(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> Y > X end,
        desc        = greater_than,
        expected    = X
    }.

-spec(greater_than_or_equal_to/1 :: (number()) -> matchspec(number())).
greater_than_or_equal_to(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> Y >= X end,
        desc        = greater_than_or_equal_to,
        expected    = X
    }.

-spec(less_than/1 :: (number()) -> matchspec(number())).
less_than(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> Y < X end,
        desc        = less_than,
        expected    = X
    }.

-spec(less_than_or_equal_to/1 :: (number()) -> matchspec(number())).
less_than_or_equal_to(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> Y =< X end,
        desc        = {less_than_or_equal_to, X},
        expected    = X
    }.

%% fun((string()) -> boolean()) matchers...

-spec(contains_string/1 :: (string()) -> matchspec(string())).
contains_string([_|_]=X) ->
    ?MATCHER(fun(Y) -> string:str(Y, X) > 0 end, X, {contains_string, X}).

-spec(starts_with/1 :: (string()) -> matchspec(string())).
starts_with(X) ->
    ?MATCHER(fun(Y) -> string:str(Y, X) == 1 end, X, {starts_with, X}).

-spec(ends_with/1 :: (string()) -> matchspec(string())).
ends_with(X) ->
    ?MATCHER(fun(Y) -> string:equal(string:right(Y, length(X)), X) end,
             X, {ends_with, X}).

-spec(matches_regex/1 :: (string()) -> matchspec(string())).
matches_regex(Rx) ->
    #'hamcrest.matchspec'{
               matcher     =
                   fun(X) ->
                           case re:run(X, Rx) of
                               {match,_} -> true;
                               _         -> false
                           end
                   end,
               desc        = {regex_match, Rx},
               expected    = Rx
              }.

-spec(match_mfa/3 :: (module(), atom(), list(term())) -> matchspec(term())).
match_mfa(Mod, Func, Args) ->
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> catch(apply(Mod, Func, Args ++ [X])) == true end,
        desc        = {eval, [Mod, Func, Args]},
        expected    = true
    }.

-spec(match_mfa/4 :: (module(), atom(),
                      list(term()), term()) -> matchspec(term())).
match_mfa(Mod, Func, Args, Desc) ->
    MS = match_mfa(Mod, Func, Args),
    MS#'hamcrest.matchspec'{desc=Desc}.

-spec(reverse_match_mfa/3 :: (module(), atom(),
                              list(term())) -> matchspec(term())).
reverse_match_mfa(Mod, Func, Args) when is_list(Args) ->
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> catch(apply(Mod, Func, [X|Args])) == true end,
        desc        = {eval, [Mod, Func, lists:reverse(Args)]},
        expected    = true
    }.

-spec(reverse_match_mfa/4 :: (module(), atom(),
                              list(term()), term()) -> matchspec(term())).
reverse_match_mfa(Mod, Func, Args, Desc) ->
    MS = reverse_match_mfa(Mod, Func, Args),
    MS#'hamcrest.matchspec'{desc=Desc}.

-spec(isalive/0 :: () -> matchspec(pid())).
isalive() ->
    MS = match_mfa(erlang, is_process_alive, []),
    MS#'hamcrest.matchspec'{desc=is_process_alive, expected=true}.

-spec(isdead/0 :: () -> matchspec(pid())).
isdead() ->
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> not erlang:is_process_alive(X) end,
        desc        = is_process_alive,
        expected    = false
    }.

-spec(has_length/1 :: (number()) -> matchspec(container_t())).
has_length(Size) when is_number(Size) ->
  ?MATCHER(fun(XS) -> length(XS) == Size end,
           Size, {length, Size}).

-spec(has_same_contents_as/1 :: (container_t()) -> matchspec(container_t())).
has_same_contents_as(Container) when is_list(Container) ->
    MS = foreach(match_mfa(?MODULE, check_member, [Container])),
    MS#'hamcrest.matchspec'{desc=has_same_contents_as, expected=Container}.

-spec(contains_member/1 :: (term()) -> matchspec(container_t())).
contains_member(E) ->
    reverse_match_mfa(?MODULE, check_member, [E]).

check_member([], []) ->
    true;
check_member(Container, E) when is_list(Container) ->
    lists:member(E, Container);
check_member(Container, E) ->
    case sets:is_set(Container) of
        true ->
            sets:is_element(E, Container);
        false ->
            case gb_sets:is_set(Container) of
                true -> gb_sets:is_element(E, Container);
                false ->
                    case ordsets:is_set(Container) of
                        true ->
                            ordsets:is_element(E, Container);
                        false ->
                            case is_tuple(Container) of
                                true -> check_member(tuple_to_list(Container), E);
                                false -> false
                            end
                    end
            end
    end.

-spec(isempty/0 :: () -> matchspec(container_t())).
isempty() ->
    match_mfa(?MODULE, check_isempty, []).

check_isempty([]) ->
    true;
check_isempty({}) ->
    true;
check_isempty(X) ->
    case sets:is_set(X) of
        true ->
            sets:size(X) == 0;
        _ ->
            case gb_sets:is_set(X) of
                true -> gb_sets:is_empty(X);
                _ ->
                    case ordsets:is_set(X) of
                        true -> ordsets:size(X) == 0;
                        false -> false
                    end
            end
    end.
