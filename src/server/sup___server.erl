-module(sup___server).
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
    Procs = get_processes(),
    {ok, {{one_for_one, 10, 10}, Procs}}.

%% --------------------------------------------------%%
%% PRIVATE
%% --------------------------------------------------%%

get_processes() ->
    [
     {wkr___connector_server,
      {wkr___connector_server, start_link, []},
      transient, brutal_kill, worker, [wkr___connector_server]},

     {wkr___communicator_server,
      {wkr___communicator_server, start_link, []},
      transient, brutal_kill, worker, [wkr___communicator_server]}
    ].
