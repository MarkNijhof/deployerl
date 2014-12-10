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
    Procs = case config:get_mode() of
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
     {worker___communicator_server,
      {worker___communicator_server, start_link, []},
      transient, brutal_kill, worker, [worker___communicator_server]}
    ].

get_processes_for_client_mode() ->
    [
     {worker___communicator_client,
      {worker___communicator_client, start_link, []},
      transient, brutal_kill, worker, [worker___communicator_client]}
    ].
