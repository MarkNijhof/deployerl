-module(deployerl_sup).
-behaviour(supervisor).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-export([start_link/0]).
-export([init/1]).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------------------%%
%% SUPERVISOR
%% --------------------------------------------------%%

init([]) ->
    Procs = case mod___config:get_mode() of
                server ->
                    get_processes_for_server_mode();
                client ->
                    get_processes_for_client_mode()
            end,
    {ok, {{one_for_one, 10, 10}, Procs}}.

%% --------------------------------------------------%%
%% PRIVATE
%% --------------------------------------------------%%

get_processes_for_server_mode() ->
    [
     ?CHILD(sup___server, supervisor),
     get_processes_for_deployer()
    ].

get_processes_for_client_mode() ->
    [
     ?CHILD(sup___client, supervisor),
     get_processes_for_deployer()
    ].

get_processes_for_deployer() ->
    ?CHILD(sup___deployer, supervisor).
