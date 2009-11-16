-module(ws_job).
-author('mijkenator@gmail.com').

-export([start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([do_this_once/0, do/1, check_not_exists/1]).

-behaviour(gen_server).

-record(job_state, {
        maxworkers,
        workbase}).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jobrec.hrl").
%-record(jobrec, {url, state, time}).

start_link(MaxWorkers) ->
    io:format("ws jobber started ~p ~n", [MaxWorkers]),
    gen_server:start_link({local, ws_job}, ?MODULE, #job_state{maxworkers = MaxWorkers}, []).
    
init(Args) ->
  io:format("ws job init callback launched ~p ~n", [Args]),

  inets:start(),
%  do_this_once(),
  mnesia:start(),
  ssl:start(),
  mnesia:wait_for_tables([jobrec], 20000),
  set_first_job(),
  {ok, Args}.
  

handle_cast({ping, _Pid, _Url}, State=#job_state{maxworkers = MaxWorkers}) ->
    %gen_server:cast(Pid, {job, Url}),
    worker_checkout(MaxWorkers),
    { memory, M } = erlang:process_info (self (), memory),
    io:format("MEMORY JOB SERVER: ~p~n", [M]),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("WS job unknown cast !!!! ~p ~p ~n", [Msg, State]),
    {noreply, State}. 
  
% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


% mnesia init
do_this_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    Ret = mnesia:create_table(jobrec, [{attributes, record_info(fields, jobrec)},
        {type, set}, {disc_only_copies, [node()]}, {ram_copies, []}]),
    io:format("DTHO!!!! ~p ~n", [Ret]),
    mnesia:stop().

set_first_job() ->
    mnesia:clear_table(jobrec),
    Row = #jobrec{url=list_to_binary("http://perl.com"), state=new},
    F = fun() -> mnesia:write(Row) end,
    Ret = mnesia:transaction(F),
    io:format("SFJ ~p ~n", [Ret]).
    
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

check_not_exists(Url) ->
    Ans = mnesia:transaction(fun() -> mnesia:select(jobrec, [{#jobrec{state='$1', url=list_to_binary(Url)}, [], ['$1']}], 1, read) end ),
    case Ans of
        {atomic, {[], _}}-> true;
        {atomic, {_, _}} -> false;
        _                -> true
    end.

worker_checkout(MaxWorkers) ->
    ChildListLength = erlang:length(supervisor:which_children(ws_com_sup)),
    io:format("Current worker count: ~p ~n", [ChildListLength]),
    %NewJobCount = erlang:length(do(qlc:q([X || X <- mnesia:table(jobrec), X#jobrec.state == new]))),
    NewJobCount = 500,
    lists:map(fun(Elem) -> web_searcher:start_client(list_to_atom(string:concat("wsworker",integer_to_list(Elem)))) end,
        additional_worker_list(MaxWorkers, NewJobCount, ChildListLength)).

additional_worker_list(MaxWorkers, JobCount, WorkerCount)
    when MaxWorkers < JobCount, MaxWorkers > WorkerCount ->
        io:format("start additional clients ~p ~p ~p ~n", [MaxWorkers,WorkerCount,JobCount]),
        lists:map(fun(Elem)-> random:uniform(Elem+100000000000) end,
            lists:seq(1, 1 + (MaxWorkers - WorkerCount)));
additional_worker_list(MaxWorkers, JobCount, WorkerCount)
    when MaxWorkers > WorkerCount, JobCount > WorkerCount ->
        io:format("start additional clients ~p ~p ~p ~n", [MaxWorkers,WorkerCount,JobCount]),
        lists:map(fun(Elem)-> random:uniform(Elem+100000000000) end,
            lists:seq(1, 1 + (JobCount - WorkerCount)));
additional_worker_list(_MaxWorkers, _JobCount, _WorkerCount)  -> [].
