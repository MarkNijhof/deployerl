-module(wkr___server_connector).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-record(state, {
          udp_port,
          receiver_socket
         }).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------%%
%% GEN SERVER
%% --------------------------------------------------%%

init([]) ->
    self() ! start_listening,
    {ok, #state{udp_port = mod___config:get_udp_port()}}.

handle_info(start_listening, State) ->
    {noreply, udp_start_listening(State)};

handle_info({udp, _Socket, _IP, _InPortNo, Message}, State) ->
    {noreply, udp_process_packet(binary_to_term(Message), State)}.

handle_call(not_implemented, _From, State) ->
    {reply, not_implemented, State}.

handle_cast(not_implemented, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{receiver_socket = ReceiverSocket}) ->
    gen_udp:close(ReceiverSocket).

%% --------------------------------------------------%%
%% PRIVATE
%% --------------------------------------------------%%

udp_start_listening(State = #state{udp_port = UdpPort}) ->
    lager:info("Deployerl is listening on UDP port: ~p~n", [UdpPort]),
    {ok, ReceiverSocket} = gen_udp:open(UdpPort, [binary, {active, true}]),
    gen_udp:controlling_process(ReceiverSocket, self()),
    State#state{receiver_socket = ReceiverSocket}.

udp_process_packet({_, _, OwnNodeName}, State)
  when OwnNodeName =:= node() ->
    State;

udp_process_packet({register_client, Pid, Name, Roles}, State) ->
    lager:info("Broadcast received from client: ~p~n", [Name]),
    case net_kernel:connect_node(Name) of
        true ->
            register_self_at_remote_node(Pid),
            wkr___server_role_manager:register_client(Name, Roles);
        false ->
            lager:error("New remote node ~p but failed to connect~n", [Name])
    end,
    State.

register_self_at_remote_node(Pid) ->
    Pid ! {register_server, self(), node(), get_ip_from_node_name()},
    ok.

get_ip_from_node_name() ->
    [_, Ip] = binary:split(atom_to_binary(node(), utf8), <<"@">>, [global]),
    Ip.
