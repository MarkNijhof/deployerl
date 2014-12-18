-module(sup___client).
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
    Procs = get_processes(),
    {ok, {{one_for_one, 10, 10}, Procs}}.

%% --------------------------------------------------%%
%% PRIVATE
%% --------------------------------------------------%%

get_processes() ->
    [
     ?CHILD(sup___client_role_managers, supervisor),
     ?CHILD(wkr___client_communicator, worker),
     ?CHILD(wkr___client_connector, worker)
    ].
