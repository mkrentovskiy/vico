{application, vico,
 [{description, "vico"},
  {vsn, "0.01"},
  {modules, [
    vico,
    vico_app,
    vico_sup,
    vico_web,
    vico_deps
  ]},
  {registered, []},
  {mod, {vico_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
