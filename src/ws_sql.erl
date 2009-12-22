-module(ws_sql).
-author('mijkenator@gmail.com').

-export([save_url/1, get_job/0, msleep/1]).


save_url(Url) when is_binary(Url) ->
    %io:format("ws_sql save url: ~p ~n", [Url]),
    ws_sql_wrap ! { self(), {save, Url}};
save_url(Url) ->
    io:format("ws_sql save NON BIN url: ~p ~n", [Url]).

get_job() ->
    ws_sql_wrap ! { self(), {get_job}},
    receive
        {gj, {atomic, {ok, Url}}}  -> {atomic, {ok, Url}}
    end.
    
msleep(T) when is_integer(T), T > 0 ->
    receive
    after T * 1000 -> true
    end.