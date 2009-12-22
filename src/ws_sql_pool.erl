-module(ws_sql_pool).
-author('mijkenator@gmail.com').

-export([start/0]).

-define(SPOOL_COUNT,    10).

start() -> register(ws_sql_wrap, spawn(fun() -> init() end)).

init() ->
    ConnectionPool = lists:map(fun(X)->
        ws_sql_wrap:start(list_to_atom(string:concat("wssqlworker",integer_to_list(X)))) end,
            lists:seq(1,?SPOOL_COUNT)),
    loop(ConnectionPool).
    
loop(ConnectionPool) ->
    receive
        {From, {save, Url}} -> get_connector(ConnectionPool) ! {From, {save, Url}}, loop(ConnectionPool);
        {From, {get_job}}   -> get_connector(ConnectionPool) ! {self(), {From, get_job}}, loop(ConnectionPool);
        {gj, From, Ret}  -> From ! {gj, Ret}, loop(ConnectionPool)
    end.
    
get_connector(ConnectionPool) -> lists:nth(random:uniform(length(ConnectionPool)), ConnectionPool).