-module(ws1).
-author('mijkenator@gmail.com').

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jobrec.hrl").
-export([start/0, worker/3, get_urls/3, trim_slash/1, trim_last_slash/1,
        save_url/1, check_not_exists/1, get_job/0, msleep/1, prepare1/0]).

start() ->
    lists:foreach(fun(X) -> spawn(ws1, worker, [<<"http://www.perl.com">>, X, 0]) end, lists:seq(1,50)).

worker(U, Number, Counter) when Counter < 10 ->
    Url = binary_to_list(U),
    io:format("Worker number ~p with ~p called -> ~n", [Number, Url]),
    case http:request(get, {Url, []}, [{timeout, 20000}, {autoredirect, true}], [{body_format, binary}]) of
        {ok, {_Status, _Headers, Body}} ->
            io:format("job ~p body length -> ~p bytes ~n", [Url, size(Body)]),
            case get_urls(Body, Url, binregex) of
                {ok, _M} ->
                    case mnesia:transaction(fun() -> get_job() end) of
                        {atomic, {ok, NewUrl}} -> io:format("get job OK -> ~p ~n", [NewUrl]), worker(NewUrl, Number, Counter+1);
                        _R   -> true
                    end;
                {error, _M} -> true
            end;
        {error, Reason} ->
            io:format("job ~p failed -> ~p ~n", [Url, Reason]),
            case mnesia:transaction(fun() -> get_job() end) of
                {atomic, {ok, NewUrl}} -> io:format("get job OK -> ~p ~n", [NewUrl]), worker(NewUrl, Number, Counter+1);
                _R   -> true
            end
    end;
worker(_U, _Number, _Counter) -> true.
    
get_urls(HtmlBinary, MainUrl, Type) when Type =:= binregex ->
    MainUrlBin = list_to_binary(MainUrl),
    MainUrl1 = trim_slash(MainUrlBin),
    Complement = fun(Url) ->
        case re:run(Url, "^http(s)*://", [{capture,[1]}]) of
            nomatch ->
                Url1 = trim_slash(Url),
                <<MainUrl1/binary,"/",Url1/binary>>;
            _ -> Url
        end
    end,
    CheckUrl = fun(Url) ->
        case re:run(Url, "\.(pdf|mp3|doc|tar|rar|zip|tgz|tar.gz|js|css|tar.bz2|bz2)$", [{capture,[1]}]) of
            {match, _} -> false;
            _          -> true
        end
    end,
    GrepHttp = fun(Url) ->
        case re:run(Url, "^http(s)*://", [{capture,[1]}]) of
            {match, _} -> CheckUrl(Url);
            _          -> false
        end
    end,
    ProcessUrl = fun([{Start, Length}]) ->
        <<_:Start/binary,Url:Length/binary,_/binary>> = HtmlBinary,
        U1 = Complement(Url),
        case GrepHttp(U1) of
            true -> save_url(U1);
            _    -> false
        end   
    end,
    Reg = "<a.*?href=['\"]*([^'\"\s]+)['\"]*.*?>",
    case re:run(HtmlBinary, Reg, [global, {capture,[1]}, {newline,any}]) of
        {match, A} when is_list(A) -> lists:map(fun(Url) -> ProcessUrl(Url) end, A), {ok, done};
        _   -> {error, get_url_error}
    end.

trim_slash(Str) when is_binary(Str), Str =:= <<"/">> -> <<>>;
trim_slash(Str) when is_binary(Str) ->
    <<A:1/binary, B/binary>> = Str,
    case A of
        <<"/">> -> trim_last_slash(B);
        _ -> trim_last_slash(Str)
    end;
trim_slash(Str) ->
    {_,LS,_} = regexp:sub(Str, "\/*$", ""),
    {_,RS,_} = regexp:sub(LS, "^\/*", ""),
    RS.
    
trim_last_slash(Bin) ->
    Len = size(Bin) - 1,
    <<A:Len/binary, B/binary>> = Bin,
    case B of
        <<"/">> -> A;
        _ -> Bin
    end.

save_url(Url) ->
    case check_not_exists(Url) of
        true ->     Row = #jobrec{url=Url, state=new},
                    F = fun() -> mnesia:write(Row) end,
                    mnesia:transaction(F), true;
        _    ->     false
    end.

check_not_exists(Url) ->
    Ans = mnesia:transaction(fun() -> mnesia:select(jobrec, [{#jobrec{state='$1', url=Url}, [], ['$1']}], 1, read) end ),
    case Ans of
        {atomic, {[], _}}-> true;
        {atomic, {_, _}} -> false;
        _                -> true
    end.
    
get_job() ->
    Ans = mnesia:select(jobrec, [{#jobrec{state=new, url='$1'}, [], ['$1']}], 1, write),
    case Ans of
        {[Url|_], _} -> mnesia:write(#jobrec{url=Url, state=processing}), {ok, Url};
        _                      ->
            %io:format("GET JOB failed -> ~p ~n", [Ans]),
            %{error, wait_for_job}
            msleep(5),
            get_job()
    end.

msleep(T) when is_integer(T), T > 0 ->
    receive
    after T * 1000 -> true
    end.
    
prepare1() ->
    inets:start(),
    mnesia:start(),
    ssl:start(),
    mnesia:wait_for_tables([jobrec], 20000),
    mnesia:clear_table(jobrec).