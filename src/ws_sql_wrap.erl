-module(ws_sql_wrap).
-author('mijkenator@gmail.com').

-export([start/1]).

start(RegName) -> register(RegName, spawn(fun() -> init() end)), RegName.

init() ->
    {ok, Ref} = odbc:connect("DSN=test;UID=root;PWD=", []),
    loop(Ref).

loop(Ref) ->
    receive
        {From, {save, Url}}            -> save_url(Ref, Url), loop(Ref);
        {From, {Requester, get_job}}   -> From ! {gj, Requester, get_job(Ref)}, loop(Ref)
    end.
    
save_url(Ref, Url) ->
    %io:format("ws_sql WRAP save url -> ~p ~n", [Url]),
    case check_not_exists(Ref, Url) of
        true ->     odbc:param_query(Ref, "insert into erljob (id, url, state, dtime) values (NULL, ?, 'new', NOW())",
                    [{{sql_varchar, 255}, [binary_to_list(Url)]}]), true;
        _    ->     false
    end.

check_not_exists(Ref, Url) ->
    %io:format("check url -> ~p ~n", [Url]),
    if
        size(Url) > 255 -> false;
        size(Url) < 256 ->
            Ret = odbc:param_query(Ref, "select count(*) from erljob where url=?",
                    [{{sql_varchar, 255}, [binary_to_list(Url)]}]),
            case  Ret of
                {selected, _, [{"0"}]} -> true;
                _ -> false
            end
    end.    

get_job(Ref) ->
    %io:format("get Job ~n"),
    Ret = odbc:sql_query(Ref, "select id, url from erljob where state='new' limit 1"),
    %io:format("get Job ret: ~p ~n", [Ret]),
    case Ret of
        {selected,["id","url"],[{Id,Url}]} ->
             odbc:param_query(Ref, "update erljob set state='processing' where id=?",[{sql_integer,[Id]}]),
             {atomic, {ok, list_to_binary(Url)}};
        _ -> ws_sql:msleep(5), get_job(Ref)
    end.