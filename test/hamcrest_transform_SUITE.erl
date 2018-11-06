%% -----------------------------------------------------------------------------
%%
%% Hamcrest Erlang.
%%
%% Copyright (c) 2017 Karolis Petrauskas (k.petrauskas@gmail.com)
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
%% @author Karolis Petrauskas <k.petrauskas@gmail.com>
%% @copyright 2017 Karolis Petrauskas.
%% -----------------------------------------------------------------------------

-module(hamcrest_transform_SUITE).
-compile([{parse_transform, hamcrest_transform}]).  % NOTE: This line is being tested.
-export([all/0, test_success/1, test_failed_assert/1, test_generator/1]).


all() ->
    [test_success, test_failed_assert, test_generator].


test_success(_Config) ->
    true = assertThat(1, is(1)),
    ok.


test_failed_assert(_Config) ->
    true = try assertThat(1, is(2)) of
        true ->
            error_expected
    catch
        error:{assertion_failed, _AssertionDetails} ->
            [{?MODULE, _Function, _Arity, _FileLine} | _] = erlang:get_stacktrace(),
            true
    end,
    ok.


test_generator(_Config) ->
    true = (assertThat_(1, is(1)))(),
    ok.


