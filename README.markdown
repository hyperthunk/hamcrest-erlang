Hamcrest Erlang
=============================

This is an implementation of Hamcrest for the [Erlang programming language](http://www.erlang.org/).

Hamcrest is a framework for writing matcher objects allowing 'match' rules to be defined declaratively.
There are a number of situations where matchers are invaluable, such as UI validation, or data filtering,
but it is in the area of writing flexible tests that matchers are most commonly used.

This tutorial shows you how to use Hamcrest for unit testing. When writing tests it is sometimes difficult
to get the balance right between over specifying the test (and making it brittle to changes), and not
specifying enough (making the test less valuable since it continues to pass even when the thing being tested
is broken). Having a tool that allows you to pick out precisely the aspect under test and describe the values
it should have, to a controlled level of precision, helps greatly in writing tests that are "just right".
Such tests fail when the behaviour of the aspect under test deviates from the expected behaviour, yet continue
to pass when minor, unrelated changes to the behaviour are made.

My first Hamcrest test
------------------------

We'll start be writing a very simple EUnit test, but instead of using EUnit's ?assertEqual macro, we use
Hamcrest's assert_that construct and the standard set of matchers:

    -module(demo1_tests).
	-compile(export_all).

    -import(hamcrest, [assert_that/2]).

	-include_lib("hamcrest/include/hamcrest.hrl").

	using_assert_that_test() ->
		assert_that(10, is(greater_than(2))).  %% returns true


The assert_that function is a stylised sentence for making a test assertion. In this example, the subject of the
assertion is the number 10 that is the first method parameter. The second method parameter is a matcher
for numbers, here a matcher that checks one number is greater than another using the standard > operator.
If the test passes (as it will in this case) then assert_that returns the atom 'true' by default.

Writing custom matchers
----------------------------

Hamcrest comes bundled with a few useful matchers, but you'll probably find that you need to create your own from
time to time. This commonly occurs when you find a fragment of code that tests the same set of properties over and
over again (and in different tests), and you want to bundle the fragment into a single assertion. By writing your
own matcher you'll eliminate code duplication and make your tests more readable!

Let's write our own matcher for testing if a string is comprised of only digits. This is the test we want to write:

    string_is_only_digits_test() ->
        assert_that("12345", is(only_digits())).

And here's the implementation:

    -module(custom_matchers).

	-export([only_digits/0]).

	only_digits() ->
		fun only_digits/1.

	only_digits(X) ->
		case re:run(X, "^[\\d]*$") of
			{match,_} -> true;
			_ -> false
		end.

The zero arity factory function we exported is responsible for creating the matcher fun, which should take 1 argument
and return the atom 'true' if it succeeds, otherwise 'false'. Although returning a fun is a simple enough way to define
a matcher, there is a another way that allows you to specify the expected versus actual input, the matcher fun and a
textual description that will be evaluated by hamcrest:assert_that/2 in case of match failures. Here's the only_digits/0
example rewritten using the alternative API:

    -module(custom_matchers).

	-export([only_digits/0]).

	only_digits() ->
		matches_regex("^[\\d]*$").

	matches_regex(Rx) ->
		#'hamcrest.matchspec'{
            matcher     =
                        fun(X) ->
                            case re:run(X, "^[\\d]*$") of
                                {match,_} -> true;
                                _ -> false
                            end
                        end,
            desc        = fun(Expected,Actual) ->
                            message(string, "matching the regex", Expected, Actual)
                          end,
            expected    = Rx
        }.

First of all, note that we're now calling the generic matches_regex/1 function which will operate over any supplied regex
compatible with the 're' module. This function is returning an instance of the 'hamcrest.matchspec' record, defined in
include/hamcrest.hrl. The matcher field points to the actual match function (which needs to return true or false as before).
The expected field contains the 'expected' value, which in this case is the supplied regex. The desc field can contain a
format string to be evaluated against the expected and actual values, or a function that returns a textual description instead.
In the former case, the format string will be evaluated as `lists:flatten(io_lib:format(Desc, [Expected, Actual]))` and the
hamcrest:message/4 function exists to provide a simple wrapper around that feature, allowing you to specify the whether the
input is a string, an error tuple or another (arbitrary) term. It evaluates its arguments to produce a string that looks
something like `expected a <TYPE> <DESC> <EXPECTED>, but was <ACTUAL>` when called.
