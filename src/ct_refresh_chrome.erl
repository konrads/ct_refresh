-module(ct_refresh_chrome).

-include("ct_refresh.hrl").

% public API
-export([
    new/2,
    start_chrome/2,
    get_tabs/1,
    reload_tabs/2
    ]).

-include_lib("lhttpc/include/lhttpc.hrl").

% websocket_client API
-export([
    init/1,
    onconnect/2,
    ondisconnect/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3]).

-record(ct_refresh_chrome_state, {client_pid, exec, debug_port}).

-define(REQUIRED_APPS, [asn1, crypto, public_key, ssl, lhttpc]).

new(Exec, DebugPort) ->
    Exec2 = case Exec of
        L when is_list(L) -> Exec;
        _ ->
            % invalid, guess from uname -s
            case os:cmd("uname -s") of
                "Darwin\n" -> "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome";
                _ -> "chrome"
            end
    end,
    ?log("Starting required apps: ~p", ?REQUIRED_APPS),
    [ ok = application:ensure_started(App) || App <- ?REQUIRED_APPS ],
    #ct_refresh_chrome_state{client_pid=self(), exec=Exec2, debug_port=DebugPort}.

start_chrome(#ct_refresh_chrome_state{exec=Exec, debug_port=DebugPort}, Uri) ->
    Cmd = ?format("~s --remote-debugging-port=~b --user-data-dir=remote-profile ~s", [Exec, DebugPort, Uri]),
    spawn(fun() -> os:cmd(Cmd) end),
    % await chrome startup
    timer:sleep(5000).

get_tabs(#ct_refresh_chrome_state{debug_port=DebugPort}) ->
    Url = ?format("http://localhost:~b/json/list", [DebugPort]),
    #lhttpc_url{host=Host, path=Path, is_ssl=UsesSsl} = lhttpc_lib:parse_url(Url),
    case lhttpc:request(Host, DebugPort, UsesSsl, Path, "GET", [], <<>>, 1000, []) of
        {_, {{Code, Reason}, _RespHeaders, RespBody}} ->
            case Code of
                200 ->
                    DebugTabs = lists:reverse(jsx:decode(RespBody)),
                    {ok, DebugTabs};
                _ -> {error, {invalid_status_code, ?format("unexpected response from chrome, code: ~p, reason: ~p, body: ~p", [Code, Reason, RespBody])}}
            end;
        {error, {econnrefused, _}} ->
            {error, {econnrefused, ?format("cannot connect to chrome on port ~b", [DebugPort])}}
    end.

reload_tabs(#ct_refresh_chrome_state{}=State, TabsJson) ->
    [
        begin
            WsUrl = binary_to_list(proplists:get_value(<<"webSocketDebuggerUrl">>, TabJson)),
            {ok, WsPid} = websocket_client:start_link(WsUrl, ?MODULE, State, []),
            receive {ws_connected, WsUrl} -> ok
            after 5000 -> error({ws_timeout, WsUrl}) end,
            websocket_client:send(WsPid, {text, <<"{\"id\":1,\"method\":\"Page.reload\"}">>})
        end
        || TabJson <- TabsJson
    ].

%% websocket_client API
init(State) ->
    {once, State}.

onconnect(Req, #ct_refresh_chrome_state{client_pid=P}=State) ->
    WsUrl = ?format("~p://~s:~b~s", [websocket_req:protocol(Req), websocket_req:host(Req), websocket_req:port(Req), websocket_req:path(Req)]),
    P ! {ws_connected, WsUrl},
    {ok, State}.

ondisconnect(_, State) -> {ok, State}.
websocket_handle(_Msg, _Req, State) -> {ok, State}.
websocket_info(_Info, _Reg, State) -> {ok, State}.
websocket_terminate(_, _Req, _State) -> ok.
