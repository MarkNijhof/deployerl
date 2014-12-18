-module(wkr___client_application_manager).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/1]).
-export([application_removed/2]).

-record(state, {
          application,
          clients = []
         }).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link(Application) ->
    gen_server:start_link({local, Application}, ?MODULE, [Application], []).

application_removed(Pid, Application) ->
    Pid ! {application_removed, Application}.

%% --------------------------------------------------%%
%% GEN SERVER
%% --------------------------------------------------%%

init([Application]) ->
    lager:info("New client application manager started for ~p", [Application]),
    {ok, #state{application = Application}}.

handle_info({register_application, Application}, State) ->
    {noreply, do_register_application(Application, State)};

handle_info({application_removed, Application}, State) ->
    {noreply, do_application_removed(Application, State)}.

handle_call(not_implemented, _From, State) ->
    {reply, not_implemented, State}.

handle_cast(not_implemented, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------%%
%% PRIVATE
%% --------------------------------------------------%%

do_register_application(Application, State = #state{application = Application}) ->
    State;

do_register_application(Application, State) ->
    process_application(Application, State).

process_application(Application, State) ->
    State#state{application = Application}.

do_application_removed(_Application, State) ->
    %% Remove app and stop self()
    State.
