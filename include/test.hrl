%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

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
    ct:pal("registering ~p~n", [All]),
    All).

-define(EQC(P),
    case code:lib_dir(eqc, include) of
        {error, bad_name} ->
            triq:check(P);
        _ ->
            eqc:check(P)
    end).
