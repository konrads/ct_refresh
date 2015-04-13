-module(ct_refresh).

-include("ct_refresh.hrl").

-export([ct_refresh/1]).

-define(DEFAULT_CHROME_PORT, 9222).


%% API
ct_refresh(LogDir) ->
    % programatically define params, so that CI can easily avoid browser interactions
    ChromeStart = case os:getenv("CT_REFRESH_CHROME_START") of
        "true" -> true;
        _ -> false
    end,
    ChromeDebugPort = case os:getenv("CT_REFRESH_CHROME_PORT") of
        false -> ?DEFAULT_CHROME_PORT;
        PortStr -> list_to_integer(PortStr) 
    end,
    ChromeExec = os:getenv("CT_REFRESH_CHROME_EXEC"),
    
    IndexFile = LogDir ++ "/index.html",
    LatestIndexFile = LogDir ++ "/latest_index.html",
    case filelib:is_regular(IndexFile) of
        true ->
            {ok, IndexFileContentsBin} = file:read_file(IndexFile),
            IndexFileContents = binary_to_list(IndexFileContentsBin),

            % analyse latest test report
            ?log("Relinking test report ~s -> ~s", [IndexFile, LatestIndexFile]),
            LinkAnalysis = analyse_links(IndexFileContents),
            
            % update latest_index.html
            CurrIndexFileContents = replace(IndexFileContents, LinkAnalysis),
            file:write_file(LatestIndexFile, CurrIndexFileContents), 
            
            % re-create the links
            DirAnalysis = analyse_dirs(LogDir, LinkAnalysis),
            relink_dirs(DirAnalysis),

            % refresh chrome tabs
            Chrome = ct_refresh_chrome:new(ChromeExec, ChromeDebugPort),
            
            case ct_refresh_chrome:get_tabs(Chrome) of
                {ok, AllTabs} ->
                    TestTabs = lists:filter(fun is_debug_tab/1, AllTabs),
                    case TestTabs of
                        [] ->
                            ?log("No chrome test tabs to refresh...");
                        _ ->
                            reload_tabs(Chrome, TestTabs)
                            % io:format(success_msg("Refreshing tabs:~n    " ++ string:join(TabTitles, "~n    ") ++ "~n"))
                    end;
                {error, {econnrefused, Reason}} ->
                    case ChromeStart of
                        true ->
                            ?log("Chrome debug unresponsive, attempting to start it manually..."),
                            ct_refresh_chrome:start_chrome(Chrome, LogDir++"/latest_index.html"),
                            {ok, TestTabs2} = ct_refresh_chrome:get_tabs(Chrome),
                            reload_tabs(Chrome, TestTabs2);
                        false ->
                            ?log("Error getting chrome tabs: ~s", [Reason])
                    end;
                {error, {SubError, Reason}} ->
                    ?log("Error getting chrome tabs: ~p, ~s", [SubError, Reason])
            end,
            ok;
        false -> 
            ?log("Failed to find test report ~s", [IndexFile]),
            {error, {invalid_test_report, IndexFile}}
    end.


%% internals
analyse_links(Contents) ->
    case re:run(Contents, "<a +href *= *\"(.*)\">(.*)</a>", [global]) of
        {match, Matches} ->
            Analysis = [
                begin
                    Href = lists:sublist(Contents, HrefStart+1, HrefLength),
                    Label = lists:sublist(Contents, LabelStart+1, LabelLength),
                    HrefLinked = to_latest_link(Label, Href),
                    {Href, HrefLinked}
                end
                || [_, {HrefStart, HrefLength}, {LabelStart, LabelLength}] <- Matches
            ],
            % filter out unchanged hrefs
            [ A || {Href, HrefLinked}=A <- Analysis, Href =/= HrefLinked ];
        _ -> []
    end.

analyse_dirs(LogDir, LinkAnalysis) ->
    LinkMap = lists:foldl(
        fun({Org, Repl}, Map) ->
            OrgDir = filename:dirname(Org),
            OrgDirSplit = filename:split(OrgDir),
            ReplDir = filename:dirname(Repl),
            ReplDirSplit = filename:split(ReplDir),
            {_, Map2} = lists:foldl(
                fun({PathElem, PathElem}, {PathSoFar, M}) ->
                    % same elements, no need to replace
                    {PathSoFar ++ "/" ++ PathElem, M};
                   ({OrgPathElem, ReplPathElem}, {PathSoFar, M}) ->
                    PathSoFar2 = PathSoFar ++ "/" ++ ReplPathElem,
                    M2 = dict:store(PathSoFar2, OrgPathElem, M),
                    {PathSoFar2, M2}
                end,
                {"", Map},
                lists:zip(OrgDirSplit, ReplDirSplit)),
            Map2
        end,
        dict:new(),
        LinkAnalysis),
    Links = lists:usort(dict:to_list(LinkMap)),
    [ {LogDir ++ File, Link} || {File, Link} <- Links ].

to_latest_link(Label, Uri) when Label =/= "<", Label =/= ">", Label =/= "CT Log" ->
    Uri2 = re:replace(Uri, "ct_run\\..*@.*/", ?format("latest_~s/", [Label]), [ungreedy, {return, list}]),
    Uri3 = re:replace(Uri2, "run\\.[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", "latest_run", [ungreedy, {return, list}]),
    Uri3;
to_latest_link(_Label, Uri) -> Uri.

replace(Str, Replacements) ->
    lists:foldl(
        fun({Org, Repl}, S) ->
            replace(S, Org, Repl)
        end,
        Str,
        Replacements).

replace(Str, Org, Repl) ->
    case string:str(Str, Org) of
        0 -> Str;
        N ->
            Str2 = lists:sublist(Str, N-1) ++ Repl ++ lists:sublist(Str, N+length(Org), length(Str)),
            replace(Str2, Org, Repl)
    end.

relink_dirs(DirAnalysis) ->
    % delete the links
    [ file:delete(QfLink) || {QfLink, _} <- lists:reverse(DirAnalysis) ],
    % relink
    [ file:make_symlink(BaseDir, QfLink) || {QfLink, BaseDir} <- DirAnalysis ].

is_debug_tab(TabJson) ->
    WsUrl = proplists:get_value(<<"webSocketDebuggerUrl">>, TabJson),  % probably dev tools tab
    Url = binary_to_list(proplists:get_value(<<"url">>, TabJson)),
    AsTokens = string:tokens(Url, "/"),
    IsFile = hd(AsTokens) == "file:",
    WasLinked = lists:member("latest_run", AsTokens),
    IsIndex = re:run(Url, "/latest_index.html$") =/= nomatch,
    HasWsUrl = WsUrl =/= undefined,
    HasWsUrl andalso IsFile andalso (IsIndex orelse WasLinked).

reload_tabs(Chrome, TestTabs) ->
    ct_refresh_chrome:reload_tabs(Chrome, TestTabs),
    TabTitles = [ binary_to_list(proplists:get_value(<<"title">>, Tab)) || Tab <- TestTabs ],
    ?log("Refreshing tabs:~n    " ++ string:join(TabTitles, "~n    ")).
