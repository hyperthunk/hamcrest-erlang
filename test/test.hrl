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
%% @copyright 2008 Tim Watson.
%% -----------------------------------------------------------------------------

-define(CT_REGISTER_TESTS(Mod),
	All = [ FName || {FName, _} <- lists:filter(
            fun ({module_info,_}) -> false ;
                ({all,_}) -> false ;
                ({init_per_suite,1}) -> false ;
                ({end_per_suite,1}) -> false ;
                ({_,1}) -> true ;
                ({_,_}) -> false
            end,
            Mod:module_info(exports)
        )
    ],
    ct:log("registering ~p~n", [All]),
    All).

%% NB: copied verbatim from eunit.hrl because proper_common.hrl redefined the LET macro, 
%%	   which causes rebar's ct task to puke on test code compilation. :/ 
-define(assertException(Class, Term, Expr),
	((fun () ->
	    try (Expr) of
	        __V -> erlang:error({assertException_failed,
				      [{module, ?MODULE},
				       {line, ?LINE},
				       {expression, (??Expr)},
				       {expected,
					"{ "++(??Class)++" , "++(??Term)
					++" , [...] }"},
				       {unexpected_success, __V}]})
	    catch
		Class:Term -> ok;
	        __C:__T ->
		    erlang:error({assertException_failed,
				   [{module, ?MODULE},
				    {line, ?LINE},
				    {expression, (??Expr)},
				    {expected,
				     "{ "++(??Class)++" , "++(??Term)
				     ++" , [...] }"},
				    {unexpected_exception,
				     {__C, __T,
				      erlang:get_stacktrace()}}]})
	    end
	  end)())).

-define(EQC(P),
    begin
    Mod = case code:lib_dir(eqc, include) of
              {error, bad_name} -> proper;
              _                 -> eqc
          end,
    true = Mod:quickcheck(P)
    end).
