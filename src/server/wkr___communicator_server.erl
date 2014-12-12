-module(wkr___communicator_server).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/0]).
-export([register_client/2]).

-record(state, {
          clients = []
         }).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link() ->
    gen_server:start_link({local, communicator}, ?MODULE, [], []).

register_client(Pid, Name) ->
    whereis(communicator) ! {register_client, Pid, Name}.

%% --------------------------------------------------%%
%% GEN SERVER
%% --------------------------------------------------%%

init([]) ->
    process_flag (trap_exit, true),
    {ok, #state{}}.

handle_info({register_client, Pid, Name}, State) ->
    {noreply, do_register_client(Pid, Name, State)};

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

do_register_client(Pid, Name, State = #state{clients = Clients}) ->
    erlang:monitor(process, Pid),
    register_self_at_remote_node(Pid),
    State#state{clients = [{Pid, Name}|Clients]}.

register_self_at_remote_node(Pid) ->
    Pid ! {register_server, self(), node(), get_ip_from_node_name()},
    ok.

get_ip_from_node_name() ->
    [_, Ip] = binary:split(atom_to_binary(node(), utf8), <<"@">>, [global]),
    Ip.

do_remove_client(Pid, State = #state{clients = Clients}) ->
    State#state{clients = lists:keydelete(Pid, 1, Clients)}.
