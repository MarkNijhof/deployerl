-module(wkr___communicator_server).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/0]).
-export([register_client/3]).

-record(state, {
          clients = []
         }).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link() ->
    gen_server:start_link({local, communicator}, ?MODULE, [], []).

register_client(Pid, Name, Roles) ->
    whereis(communicator) ! {register_client, Pid, Name, Roles}.

%% --------------------------------------------------%%
%% GEN SERVER
%% --------------------------------------------------%%

init([]) ->
    process_flag (trap_exit, true),
    %% gproc:reg({p, g, server}, test),
    {ok, #state{}}.

handle_info({register_client, Pid, Name, Roles}, State) ->
    {noreply, do_register_client(Pid, Name, Roles, State)};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    {noreply, do_remove_client(Pid, State)};

handle_info({nodedown, Node}, State) ->
    io:format(user, "NODE DOWN ~p~n", [Node]),
    {noreply, State}.

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

do_register_client(Pid, Name, Roles, State = #state{clients = Clients}) ->
    erlang:monitor(process, Pid),
    monitor_node(Name, true),
    %% gproc_dist:get_leader(),
    register_self_at_remote_node(Pid),
    State#state{clients = [{Pid, Name, Roles}|Clients]}.

register_self_at_remote_node(Pid) ->
    Pid ! {register_server, self(), node(), get_ip_from_node_name()},
    ok.

get_ip_from_node_name() ->
    [_, Ip] = binary:split(atom_to_binary(node(), utf8), <<"@">>, [global]),
    Ip.

do_remove_client(Pid, State = #state{clients = Clients}) ->
    State#state{clients = lists:keydelete(Pid, 1, Clients)}.
