%% This is the application resource file (.app file) for the triq,
%% application.
{application, triq, 
  [{description, "Trifork QuickCheck 0.1.0"},
   {vsn, "0.1.0"},
   {modules, [triq,triq_app,triq_autoexport,triq_domain,triq_simplify,triq_sup,triq_tests]},
   {registered,[]},
   {applications, [kernel,stdlib]},
   {build_dependencies, []},
   {env, []}
]}.

