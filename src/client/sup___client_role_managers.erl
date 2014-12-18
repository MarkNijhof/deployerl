-module(sup___client_role_managers).
-behavior(supervisor).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, brutal_kill, Type, [I]}).

-export([start_link/0]).
-export([init/1]).

-export([get_role_manager_pid/1]).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_role_manager_pid(Role) ->
    ClientRole = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Role)),
    case whereis(ClientRole) of
        undefined ->
            {ok, Pid} = supervisor:start_child(?MODULE, [ClientRole]),
            Pid;
        Pid ->
            Pid
    end.

%% --------------------------------------------------%%
%% SUPERVISOR
%% --------------------------------------------------%%

init([]) ->
    ChildSpec = ?CHILD(wkr___client_role_manager, worker),
    Children = [ChildSpec],

    MaxRestart = 6,
    MaxTime = 3600,

    RestartStrategy = {simple_one_for_one, MaxRestart, MaxTime},

    {ok, {RestartStrategy, Children}}.

%% --------------------------------------------------%%
%% PRIVATE
%% --------------------------------------------------%%
