-module(ws1).
-author('mijkenator@gmail.com').

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jobrec.hrl").
-export([start/0, worker/3, get_urls/3, trim_slash/1, trim_last_slash/1,
        save_url/1, check_not_exists/1, get_job/0, msleep/1, prepare1/0, start/2, starter/2]).

start() ->
    lists:foreach(fun(X) -> Pid = spawn(ws1, worker, [<<"http://www.perl.com">>, X, 0]),
    io:format("Pid -> ~p ~n", [Pid]), register(toProf, Pid) end, lists:seq(1,1)).

start(Url, CountWorker) ->
    Pid = spawn(ws1, starter, [Url, CountWorker]),
    register(ws1Prof, Pid),
    Pid.

starter(Url, CountWorker) ->
    spawn(ws1, worker, [Url, 0, 0]),
    msleep(20),
    lists:foreach(fun(X) -> spawn(ws1, worker, [<<>>, X, 0]) end, lists:seq(1, CountWorker)).

worker(U, Number, Counter) when U =:= <<>> ->
    case mnesia:transaction(fun() -> get_job() end) of
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
            get_urls(Body, Url, binregex);
        {error, Reason} ->
            io:format("job ~p failed -> ~p ~n", [Url, Reason])
    end,
    case mnesia:transaction(fun() -> get_job() end) of
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
    
get_urls(HtmlBinary, MainUrl, Type) when Type =:= binregex ->
    MainUrlBin = list_to_binary(MainUrl),
    MainUrl1 = trim_slash(MainUrlBin),
    Complement = fun(Url) ->
        case checkHttp(Url) of
            false ->
                Url1 = trim_slash(Url),
                <<MainUrl1/binary,"/",Url1/binary>>;
            _ -> Url
        end
    end,
    ProcessUrl = fun([{Start, Length}]) ->
        <<_:Start/binary,Url:Length/binary,_/binary>> = HtmlBinary,
        case checkUrl(Url) of
            true -> save_url(Complement(clear_url(Url)));
            _    -> false
        end   
    end,
    Reg = "<a.*?href=['\"]*([^'\"\s]+)['\"]*.*?>",
    case re:run(HtmlBinary, Reg, [global, {capture,[1]}, {newline,any}]) of
        {match, A} when is_list(A) -> lists:foreach(fun(Url) -> ProcessUrl(Url) end, A), {ok, done};
        _   -> {error, get_url_error}
    end.

trim_slash(Str) when is_binary(Str), Str =:= <<>> -> <<>>;
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
        true ->     mnesia:transaction(fun() -> mnesia:write(#jobrec{url=Url, state=new}) end), true;
        _    ->     false
    end.

check_not_exists(Url) ->
    case mnesia:dirty_read({jobrec, Url}) of
        []    -> true;
        _     -> false
    end.

    
get_job() ->
    Ans = mnesia:select(jobrec, [{#jobrec{state=new, url='$1'}, [], ['$1']}], 1, write),
    case Ans of
        {[Url|_], _} -> mnesia:write(#jobrec{url=Url, state=processing}), {ok, Url};
        _                      -> msleep(5), get_job()
    end.

clear_url(Url) ->
    S = size(Url), S1 = S-1, S2 = S-2,
    case Url of
        <<R:S1/binary,"\r">> -> R;
        <<R:S1/binary,"\n">> -> R;
        <<R:S2/binary,"\n\r">> -> R;
        <<R:S2/binary,"\r\n">> -> R;
        _ -> Url
    end.

checkHttp(Url) ->
    case Url of
        <<"http://", _/binary>>  -> true;
        <<"https://", _/binary>> -> true;
        _ -> false
    end.

checkUrl(Url) ->
    S = size(Url),
    S4 = S - 4, S3 = S - 3,  
    case Url of
        <<_:S4/binary, ".pdf">>  -> false;
        <<_:S4/binary, ".mp3">>  -> false;
        <<_:S4/binary, ".doc">>  -> false;
        <<_:S4/binary, ".rar">>  -> false;
        <<_:S4/binary, ".tgz">>  -> false;
        <<_:S3/binary, ".js">>   -> false;
        <<_:S4/binary, ".css">>  -> false;
        <<_:S4/binary, ".bz2">>  -> false;
        <<_:S3/binary, ".gz">>   -> false;
        <<"#", _/binary>>        -> false;
        <<"/">>                  -> false;
        <<"/#">>                 -> false;
        _ -> true
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