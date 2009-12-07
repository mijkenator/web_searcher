-module(ws1).
-author('mijkenator@gmail.com').

-export([start/0, worker/3, prepare1/0, start/2, starter/2]).

start() ->
    lists:foreach(fun(X) -> Pid = spawn(ws1, worker, [<<"http://www.perl.com">>, X, 0]),
    io:format("Pid -> ~p ~n", [Pid]), register(toProf, Pid) end, lists:seq(1,1)).

start(Url, CountWorker) ->
    Pid = spawn(ws1, starter, [Url, CountWorker]),
    register(ws1Prof, Pid),
    Pid.

starter(Url, CountWorker) ->
    spawn(ws1, worker, [Url, 0, 0]),
    ws_mnesia:msleep(20),
    lists:foreach(fun(X) -> spawn(ws1, worker, [<<>>, X, 0]) end, lists:seq(1, CountWorker)).

worker(U, Number, Counter) when U =:= <<>> ->
    case ws_mnesia:get_job() of
        {atomic, {ok, NewUrl}} ->
            io:format("get job OK -> ~p ~n", [NewUrl]),
            worker(NewUrl, Number, Counter+1);
        _R   ->
            io:format("Exit -> ~p ~n", [_R]),
            true
    end;
worker(U, Number, Counter) when Counter < 200 ->
    Url = binary_to_list(U),
    io:format("Worker number ~p (~p) with ~p called -> ~n", [Number, Counter, Url]),
    case http:request(get, {Url, []}, [{timeout, 20000}, {autoredirect, true}], [{body_format, binary}]) of
        {ok, {_Status, _Headers, Body}} ->
            io:format("job ~p body length -> ~p bytes ~n", [Url, size(Body)]),
            ws_html:get_urls(Body, Url, binregex);
        {error, Reason} ->
            io:format("job ~p failed -> ~p ~n", [Url, Reason])
    end,
    case ws_mnesia:get_job() of
        {atomic, {ok, NewUrl}} ->
            io:format("get job OK -> ~p ~n", [NewUrl]),
            worker(NewUrl, Number, Counter+1);
        _R   ->
            io:format("Exit -> ~p ~n", [_R]),
            true
    end;    
worker(_U, _Number, _Counter) ->
    io:format("Work down -> ~p ~n", [_Counter]),
    true.
    
prepare1() ->
    inets:start(),
    mnesia:start(),
    ssl:start(),
    mnesia:wait_for_tables([jobrec], 20000),
    mnesia:clear_table(jobrec).