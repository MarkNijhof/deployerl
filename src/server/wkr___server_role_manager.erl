-module(wkr___server_role_manager).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/1]).
-export([register_client/2]).
-export([register_applications/2]).

-record(state, {
          role,
          applications = [],
          applications_diff = [],
          clients = []
         }).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link(Role) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role], []).

register_client(_, []) ->
    ok;

register_client(Name, [{Role, RolePid} | Roles]) ->
    RoleManagerPid = sup___server_role_managers:get_role_manager_pid(Role),
    RoleManagerPid ! {register_client, Name, Role, RolePid},
    register_client(Name, Roles).

register_applications(Role, Applications) ->
    RoleManagerPid = sup___server_role_managers:get_role_manager_pid(Role),
    RoleManagerPid ! {register_applications, Applications}.

%% --------------------------------------------------%%
%% GEN SERVER
%% --------------------------------------------------%%

init([Role]) ->
    lager:info("New server role manager started for ~p", [Role]),
    process_flag (trap_exit, true),
    {ok, #state{role = Role}}.

handle_info({register_client, Name, Role, RolePid}, State) ->
    {noreply, do_register_client(Name, Role, RolePid, State)};

handle_info({register_applications, Applications}, State) ->
    {noreply, do_register_applications(Applications, State)};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    {noreply, do_remove_client(Pid, State)}.

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


do_register_client(Name, Role, RolePid, State = #state{clients = Clients}) ->
    erlang:monitor(process, RolePid),
    State#state{clients = [{RolePid, Role, Name}|Clients]}.

do_remove_client(Pid, State = #state{clients = Clients}) ->
    State#state{clients = lists:keydelete(Pid, 1, Clients)}.

do_register_applications(Applications, State = #state{applications = Applications}) ->
    Diff = [{unchanged, Application} || Application <- Applications],
    update_client(State#state{applications_diff = Diff});

do_register_applications(Applications, State = #state{applications = []}) ->
    Diff = [{new, Application} || Application <- Applications],
    update_client(State#state{applications_diff = Diff});

do_register_applications(Applications, State = #state{applications = PrevApplications}) ->
    Diff = [diff_application(Application, PrevApplications) || Application <- Applications],
    Removed = [{removed, Application} || Application <- Applications, is_not_member_of(Application, PrevApplications)],
    update_client(State#state{applications_diff = [Diff|Removed]}).

is_not_member_of(Application, Applications) ->
    Key = maps:get(<<"application">>, Application),
    Found = [App || App <- Applications, maps:get(<<"application">>, Application) =:= Key],
    length(Found) =:= 0.

diff_application(Application, []) ->
    {new, Application};
diff_application(Application, [PA|PrevApplications]) ->
    AppName = maps:get(<<"application">>, Application),
    PrevAppName = maps:get(<<"application">>, PA),
    case AppName of
        PrevAppName ->
            if
                Application =:= PA ->
                    {unchanged, Application};
                true ->
                    {changed, Application}
            end;
        _ ->
            diff_application(Application, PrevApplications)
    end.

update_client(State) ->
    State.
    %% RolePid ! {register_applications, Applications}.
