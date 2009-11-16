%%%-------------------------------------------------------------------
%%% File    : web_searcher.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(web_searcher).

-behaviour(application).

-export([start/2, stop/1]).
-export([init/1, start_client/1, start_client2/1]).


start_client(Opts) ->
    supervisor:start_child(ws_com_sup, [Opts]).

start_client2(Opts) ->
    supervisor:start_child(ws_com_sup, [Opts]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [50, ws_worker]).

stop(_S) ->
    ok.



    
%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([MaxWorkers, Module]) ->
    io:format("supervisor init ~p ~n", [self()]),
   {ok, {
            {one_for_one, 1, 60},
            [
              { ws_com_sup,
                {supervisor,start_link,[{local, ws_com_sup}, ?MODULE, [Module]]},
                permanent,                               % Restart  = permanent | transient | temporary
                infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                supervisor,                              % Type     = worker | supervisor
                []                                       % Modules  = [Module] | dynamic
              },
              { ws_job_server,
                {ws_job,start_link,[MaxWorkers]},
                permanent,                               
                2000,                                
                worker,                              
                [ws_job]                                      
              },
              { wsworker0,
                {ws_worker, start_link, [ wsworker0 ]},
                temporary,
                2000,
                worker,
                []
              }
            ]
        }
    };
init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, 1, 60},
            [
              % worker
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
    
    


