-module(wkr___communicator_client).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/0]).

-record(state, {
          server_pid,
          server_name,
          server_ip_address
         }).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link() ->
    gen_server:start_link({local, communicator}, ?MODULE, [], []).

%% --------------------------------------------------%%
%% GEN SERVER
%% --------------------------------------------------%%

init([]) ->
    process_flag (trap_exit, true),
    mod___collectd:clear_any_collectd_server_ip_addresses(),
    {ok, #state{}}.

handle_info({register_server, Pid, Name, IpAddress}, State) ->
    {noreply, process_connected_server(Pid, Name, IpAddress, State)};

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, process_disconnected_server(State)}.

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

process_connected_server(Pid, Name, IpAddress, State) ->
    lager:info("Deployerl connected to server: ~p~n", [Name]),
    erlang:monitor(process, Pid),
    mod___collectd:add_collectd_server_ip_address(IpAddress),
    wkr___connector_client:connected_to_server(),
    State#state{server_pid = Pid,
                server_name = Name,
                server_ip_address = IpAddress}.

process_disconnected_server(State = #state{server_name = Name,
                                           server_ip_address = IpAddress}) ->
    lager:info("Deployerl disconnected from server: ~p~n", [Name]),
    mod___collectd:remove_collectd_server_ip_address(IpAddress),
    wkr___connector_client:disconnected_from_server(),
    State#state{server_pid = undefined}.
