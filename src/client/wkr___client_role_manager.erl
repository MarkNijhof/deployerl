-module(wkr___client_role_manager).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/1]).

-record(state, {
          role,
          applications = [],
          clients = []
         }).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link(Role) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role], []).

%% --------------------------------------------------%%
%% GEN SERVER
%% --------------------------------------------------%%

init([ Role ]) ->
    lager:info("New client role manager started for ~p", [Role]),
    {ok, #state{role = Role}}.

handle_info({register_applications, Applications}, State) ->
    {noreply, do_register_applications(Applications, State)}.

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

do_register_applications(Applications, State = #state{applications = Applications}) ->
    State;

do_register_applications(Applications, State = #state{applications = _PrevApplications}) ->
    %% remove_applications(Applications, PrevApplications),

    State#state{applications = Applications}.

%% remove_applications(Applications, PrevApplications) ->
%%     ToBeRemoved = [Application || Application <- PrevApplications,
%%                                   is_not_member_of(Application, Applications)],
%%     [begin
%%          ApplicationName = maps:get(<<"application">>, Application),
%%          Pid = sup___client_application_managers:get_application_manager_pid(ApplicationName),
%%          wkr___client_application_manager:application_removed(Pid, Application)
%%      end
%%      || Application <- ToBeRemoved],
%%     ok.

%% is_not_member_of(Application, Applications) ->
%%     Key = maps:get(<<"application">>, Application),
%%     Found = [Application || Application <- Applications, maps:get(<<"application">>, Application) =:= Key],
%%     length(Found) =:= 0.

%% process_applications(Applications, State) ->
