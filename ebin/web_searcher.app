{application, web_searcher,
 [
  {description, "Web searcher"},
  {vsn, "1.0"},
  {id, "web_searcher"},
  {modules,      [ws_worker, ws_job]},
  {registered,   [ws_worker, ws_job, ws_job_server]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {web_searcher, []}},
  {env, [
        {config_file,  "/home/ashim/research/Git/Obeerl/t/test.cfg"},
        {ums_root,     "/var/lib/ums" },
        {listen_port,  2223},
        {listen_port_tls, false},
        {control_port, 2225},
        {control_port_tls, false}
        ]}
 ]
}.