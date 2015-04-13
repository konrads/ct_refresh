%% Fire report relink and browser refresh.
%% FIXME: could not find a better trigger than second rewrite of **/all_runs.html
-module(ct_refresh_events).

-behaviour(gen_event).

-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {pass=0}).

init(_Params) ->
    {ok, #state{}}.

handle_event({event,finished_write_file,_,Filename}, #state{pass=0}=State) ->
    case lists:suffix("/all_runs.html", Filename) of
        true -> {ok, State#state{pass=1}};
        false -> {ok, State}
    end;
handle_event({event,finished_write_file,_,Filename}, #state{pass=1}=State) ->
    case lists:suffix("/all_runs.html", Filename) of
        true ->
            ct_refresh:ct_refresh("."),
            {ok, State#state{pass=2}};
        false -> {ok, State}
    end;
handle_event(_E, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
