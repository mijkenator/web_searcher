-module(ws_mnesia).
-author('mijkenator@gmail.com').

-export([save_url/1, check_not_exists/1, get_job/0, msleep/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jobrec.hrl").

save_url(Url) ->
    case check_not_exists(Url) of
        true ->     mnesia:transaction(fun() -> mnesia:write(#jobrec{url=Url, state=new}) end), true;
        _    ->     false
    end.

check_not_exists(Url) ->
    case mnesia:dirty_read({jobrec, Url}) of
        []    -> true;
        _     -> false
    end.
    
get_job() ->
    Ans = mnesia:transaction(fun() -> mnesia:select(jobrec, [{#jobrec{state=new, url='$1'}, [], ['$1']}], 1, write) end),
    case Ans of
        {atomic, {[Url|_], _}} -> mnesia:transaction(fun() -> mnesia:write(#jobrec{url=Url, state=processing}) end),
                                  {atomic, {ok, Url}};
        _                      -> msleep(5), get_job()
    end.
    
msleep(T) when is_integer(T), T > 0 ->
    receive
    after T * 1000 -> true
    end.