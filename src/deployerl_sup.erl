-module(deployerl_sup).
-behaviour(supervisor).

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
     {sup___server,
      {sup___server, start_link, []},
      transient, brutal_kill, supervisor, [sup___server]},

     get_processes_for_deployer()
    ].

get_processes_for_client_mode() ->
    [
     {sup___client,
      {sup___client, start_link, []},
      transient, brutal_kill, supervisor, [sup___client]},

     get_processes_for_deployer()
    ].

get_processes_for_deployer() ->
    {sup___deployer,
     {sup___deployer, start_link, []},
     transient, brutal_kill, supervisor, [sup___deployer]}.
