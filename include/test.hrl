%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

-define(CT_REGISTER_TESTS(Mod),
	[ FName || {FName, _} <- lists:filter(
            fun ({module_info,_}) -> false ;
                ({all,_}) -> false ;
                ({init_per_suite,1}) -> false ;
                ({end_per_suite,1}) -> false ;
                ({_,1}) -> true ;
                ({_,_}) -> false
            end,
            Mod:module_info(exports)
        )
    ].
