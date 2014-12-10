-module(worker___communicator_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-record(state, {
          udp_port,
          receiver_socket,
          nodes = [],
          own_ip_address
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
    process_flag (trap_exit, true),
    self() ! start_listening,
    [_, OwnIpAddress] = get_name_and_ip_from_node_name(node()),
    {ok, #state{udp_port = config:get_udp_port(),
                own_ip_address = OwnIpAddress}}.

handle_info(start_listening, State) ->
    {noreply, udp_start_listening(State)};

handle_info({udp, _Socket, _IP, _InPortNo, Message}, State) ->
    {noreply, udp_process_packet(binary_to_term(Message), State)};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    {noreply, handle_disconnected_node(Pid, State)}.

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

get_name_and_ip_from_node_name(NodeName) ->
    binary:split(atom_to_binary(NodeName, utf8), <<"@">>, [global]).

udp_start_listening(State = #state{udp_port = UdpPort}) ->
    {ok, ReceiverSocket} = gen_udp:open(UdpPort, [binary, {active, true}]),
    gen_udp:controlling_process(ReceiverSocket, self()),
    lager:info("Deployerl is listening on UDP port: ~p~n", [UdpPort]),
    State#state{receiver_socket = ReceiverSocket}.

udp_process_packet({_, _, OwnNodeName}, State)
  when OwnNodeName =:= node() ->
    State;

udp_process_packet({register_client, Pid, Name}, State) ->
    connect_to_remote_node(Pid, Name, State).

connect_to_remote_node(Pid, Name, State) ->
    lager:info("Broadcast received from client: ~p~n", [Name]),
    case net_adm:ping(Name) of
        pong ->
            process_new_node(Pid, Name, State);
        pang ->
            lager:error("New remote node ~p but failed to connect~n", [Name]),
            State
    end.

process_new_node(Pid, Name, State = #state{nodes = Nodes}) ->
    case lists:member({Pid, Name}, Nodes) of
        true ->
            State;
        false ->
            erlang:monitor(process, Pid),
            register_self_at_remote_node(Pid, State),
            State#state{nodes = Nodes ++ [{Pid, Name}]}
    end.

register_self_at_remote_node(Pid, #state{own_ip_address = OwnIpAddress}) ->
    Pid ! {register_server, self(), node(), OwnIpAddress},
    ok.

handle_disconnected_node(Pid, State = #state{nodes = Nodes}) ->
    NewNodes = process_disconnected_node(Pid, Nodes),
    State#state{nodes = NewNodes}.

process_disconnected_node(Pid, Nodes) ->
    case lists:keyfind(Pid, 1, Nodes) of
        Node = {Pid, _} ->
            NewNodes = lists:delete(Node, Nodes),
            if
                Nodes =:= NewNodes -> ok;
                true ->
                    ok
            end,
            NewNodes;
        _ ->
            Nodes
    end.
