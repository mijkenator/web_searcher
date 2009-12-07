-module(ws_html).
-author('mijkenator@gmail.com').

-export([get_urls/3, trim_slash/1, trim_last_slash/1,
        clear_url/1, checkHttp/1, checkUrl/1]).

get_urls(Html, MainUrl, Type) when Type =:= mochi ->
    try mochiweb_html:parse(Html) of
        Tree -> finding(<<"a">>,<<"href">>, Tree, MainUrl), {ok, done}
    catch
        _:_  -> {error, parser_error}
    end;
get_urls(HtmlBinary, MainUrl, Type) when Type =:= regex ->
    Html = binary_to_list(HtmlBinary),
    Complement = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> string:join([trim_slash(MainUrl), trim_slash(Url)], "/");
            _ -> Url
        end
    end,
    GrepHttp = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> false;
            {match, _A} -> case regexp:matches(Url, "\.(pdf|mp3|doc|tar|rar|zip|tgz|tar.gz|js|css|tar.bz2|bz2)$") of
                                {match, []} -> true;
                                {match, _A} -> false;
                                _           -> true
                           end;
            _           -> false
        end
    end,
    ProcessUrl = fun([{Start, Length}]) ->
        Url = string:substr(Html, Start+1, Length),
        U1 = Complement(Url),
        case GrepHttp(U1) of
            true -> ws_mnesia:save_url(list_to_binary(U1));
            _    -> false
        end   
    end,
    Reg = "<a\s+\.*?href=['\"]*([^'\"\s]+)['\"]*\s*\.*>",
    case re:run(Html, Reg, [global, {capture,[1]}, {newline,any}]) of
        {match, A} when is_list(A) -> lists:map(fun(Url) -> ProcessUrl(Url) end, A), {ok, done};
        _   -> {error, get_url_error}
    end;    
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
            true -> ws_mnesia:save_url(Complement(clear_url(Url)));
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
    
finding(Pattern, Attribute, Tree, MainUrl) when is_binary(Attribute)->
    Complement = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> string:join([trim_slash(MainUrl), trim_slash(Url)], "/");
            _ -> Url
        end
    end,
    GrepHttp = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> false;
            {match, _A} -> case regexp:matches(Url, "\.(pdf|mp3|doc|tar|rar|zip|tgz|tar.gz|js|css|tar.bz2|bz2)$") of
                                {match, []} -> true;
                                {match, _A} -> false;
                                _           -> true
                           end;
            _           -> false
        end
    end,
    ProcessUrl = fun(Url) ->
        U1 = Complement(Url),
        case GrepHttp(U1) of
            true -> ws_mnesia:save_url(list_to_binary(U1))
        end   
    end,
    GetAttr = fun(Found) ->
        {Pattern, Attributes, _} = Found,
        M = lists:filter(fun(Attr) -> case Attr of {Attribute, _} -> true; _ -> false end end, Attributes),
        case M of
            [{Attribute, FoundAttribute} | _] -> ProcessUrl(binary_to_list(FoundAttribute)), {ok};
            _ -> {error}
        end
    end,  
    lists:map(fun(X) -> GetAttr(X) end, finding(Pattern, [Tree], [])).
  
finding(_, [], Collected) ->  
  Collected;  
  
finding(Pattern, [Next | Siblings], Collected) ->  
    case Next of  
      {Element, _, Children} ->  
      case Element of  
        Pattern ->  
          finding(Pattern, Siblings ++ Children, Collected ++ [Next]);  
        _ ->  
          finding(Pattern, Siblings ++ Children, Collected)  
      end;  
    _ ->  
      finding(Pattern, Siblings, Collected)  
    end.