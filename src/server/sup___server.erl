-module(sup___server).
-behaviour(supervisor).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-export([start_link/0]).
-export([init/1]).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

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
     ?CHILD(sup___server_role_managers, supervisor),
     ?CHILD(wkr___server_connector, worker),
     ?CHILD(wkr___server_manifest_loader, worker)
    ].
