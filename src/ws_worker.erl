-module(ws_worker).
-author('mijkenator@gmail.com').

-export([start_link/1]).
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([do_job/2]).

%% FSM States
-export([
    'WAIT_FOR_JOB'/2,
    'WAIT_FOR_WORK'/2
]).
%-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jobrec.hrl").

-behaviour(gen_fsm).

-record(worker_state, {
        workername,
        wokrnumber,
        configitem,
        url,
        counter=0}).

start_link(WorkerName) ->
    io:format("wsworker started ~p ~n", [WorkerName]),
    Result = gen_fsm:start_link({local, WorkerName},?MODULE, #worker_state{workername = WorkerName, counter=0}, []),
    io:format("wsworker started Result ~p ~p ~n", [Result, WorkerName]),
    Result.
    
init(Args) ->
  io:format("ws work init callback launched ~p ~n", [Args]),
  {ok, 'WAIT_FOR_JOB', Args, 10}.
  
'WAIT_FOR_JOB'(_Other, State) ->
    case ws_mnesia:get_job()  of
        {atomic, {ok, NewUrl}} ->
            io:format("'WAIT_FOR_JOB' ~p ~n", [NewUrl]),
            gen_server:cast(ws_job, {ping, self(), NewUrl}),
            {next_state, 'WAIT_FOR_WORK', State#worker_state{url=NewUrl}, 10};
        R ->
            io:format("'WAIT_FOR_JOB' no work ~p ~n", [R]),
            {next_state, 'WAIT_FOR_JOB', State, 5000}
    end.
    
'WAIT_FOR_WORK'(_Data, #worker_state{workername = WorkerName, counter=Counter, url=U} = State) ->
    Url = binary_to_list(U),
    { memory, M } = erlang:process_info (self (), memory),
    io:format("MEMORY0:~p ~p~n", [WorkerName, M]),
    case do_job(Url, State) of
        {ok, _}       ->
            { memory, M1 } = erlang:process_info (self (), memory),
            io:format("MEMORY1:~p ~p~n", [WorkerName, M1]),
            mnesia:transaction(fun() ->  mnesia:write(#jobrec{url=list_to_binary(Url), state=done}) end);
        {error, Reason} ->
            mnesia:transaction(fun() ->  mnesia:write(#jobrec{url=list_to_binary(Url), state=fail}) end),
            io:format("job error: ~p~n",  [Reason])
    end,
    case Counter of
        Counter when Counter=:=5; Counter=:=15;Counter=:=25;Counter=:=35 ->
            {ok, S} = file:open("memlog.log", [append]),
            io_format_wrap(S, "DEBUG", erlang:memory()),
            file:close(S),
            {next_state, 'WAIT_FOR_JOB', State#worker_state{workername = WorkerName, counter=Counter+1}, 100};
        Counter when Counter < 50 ->
            {next_state, 'WAIT_FOR_JOB', State#worker_state{workername = WorkerName, counter=Counter+1}, 100};
        _ ->
            {ok, S} = file:open("memlog.log", [append]),
            io_format_wrap(S, "DEBUG", erlang:memory()),
            file:close(S),
            {stop, normal, State}
            %{next_state, 'WAIT_FOR_JOB', State#worker_state{workername = WorkerName, counter=Counter+1}, 10}
    end.
  
% These are just here to suppress warnings.

handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
handle_info(Info, StateName, StateData) ->
    io:format("nexpected message ~p ~p ~p ~n", [Info, StateName, StateData]),
    {stop, normal, StateData}.
terminate(_Reason, _StateName, _State) ->
    io:format("Terminate ~n"),
    ok.

do_job(Url, State) ->
    io:format("Do job -> ~p ~n", [State]),
    case http:request(get, {Url, []}, [{timeout, 20000}, {autoredirect, true}], [{body_format, binary}]) of
        {ok, {_Status, _Headers, Body}} ->
            %io:format("job ~p result status -> ~p  ~n", [Url, Status]),
            %o:format("job ~p headers -> ~p bytes ~n",  [Url, Headers]),
            %io:format("job ~p body length -> ~p bytes ~n", [Url, string:len(Body)]),
            io:format("job ~p body length -> ~p bytes ~n", [Url, size(Body)]),
            case ws_html:get_urls(Body, Url, binregex) of
            %case get_urls(mochiweb_html:parse(Body), Url) of
                {ok, M} -> {ok, M};
                {error, M} -> {error, M}
            end;
        {error, Reason} ->
            io:format("job ~p failed -> ~p ~n", [Url, Reason]),
            {error, Reason}
    end.

io_format_wrap(S, Type, Message) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
    io:format(S, "~-15w ~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B  ~p ~n",
    [Type, Month, Day, Year, Hour, Min, Sec, Message]).


