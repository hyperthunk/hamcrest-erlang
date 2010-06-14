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

Tagging assertions and describing matchers
--------------------------------------------

If you have more than one assertion in your test you can include an identifier for the tested value in the assertion:

    assert_that(ChocolateChipCount, equal_to(10), 'chocolate chips'),
    assert_that(HazelnutCount, equal_to(3), 'hazelnuts').

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

The zero arity factory function we exported is responsible for creating the matcher fun, which should take 1 argument and return the atom 'true' if it succeeds, otherwise 'false'.  

