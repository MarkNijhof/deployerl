-module(worker___communicator_client).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-record(state, {
          udp_port,
          server,
          send_socket
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
    self() ! broadcast,
    clear_any_collectd_server_ip_addresses(),
    {ok, #state{udp_port = config:get_udp_port()}}.

handle_info(broadcast, State) ->
    {noreply, udp_broadcast(State)};

handle_info({register_server, Pid, Name, IpAddress}, State) ->
    add_collectd_server_ip_address(IpAddress),
    {noreply, process_server(Pid, Name, IpAddress, State)};

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

udp_broadcast(State = #state{server = undefined,
                             send_socket = undefined,
                             udp_port = UdpPort}) ->
    lager:info("Deployerl is broadcasting to UDP port: ~p~n", [UdpPort]),
    {ok, SendSocket} = gen_udp:open(0, [binary, {broadcast, true}]),
    udp_broadcast(State#state{send_socket = SendSocket});

udp_broadcast(State = #state{server = undefined,
                             send_socket = SendSocket,
                             udp_port = UdpPort}) ->
    ok = gen_udp:send(SendSocket,
                      {255, 255, 255, 255},
                      UdpPort,
                      term_to_binary({register_client, self(), node()})),
    erlang:send_after(1000, self(), broadcast),
    State;

udp_broadcast(State = #state{send_socket = SendSocket}) ->
    ok = gen_udp:close(SendSocket),
    State#state{send_socket = undefined}.

process_server(Pid, Name, IpAddress, State) ->
    erlang:monitor(process, Pid),
    lager:info("Deployerl talked to server: ~p~n", [Name]),
    State#state{server = {Pid, Name, IpAddress}}.

process_disconnected_server(State = #state{server = {_, _, IpAddress}}) ->
    self() ! broadcast,
    remove_collectd_server_ip_address(IpAddress),
    State#state{server = undefined}.

add_collectd_server_ip_address(IpAddress) ->
    case filelib:is_file("/etc/collectd/collectd.conf") of
        false ->
            ok;
        true ->
            Cmd = ""
                ++"sudo sed -i.bak "
                ++"'s/servers_do_not_remove"
                ++"/servers_do_not_remove\\n    Server \""
                ++ binary_to_list(IpAddress)
                ++"\" \"25826\"/g' "
                ++"/etc/collectd/collectd.conf"
                ++" && "
                ++"sudo /etc/init.d/collectd restart",

            os:cmd(Cmd),
            ok
    end.

remove_collectd_server_ip_address(IpAddress) ->
    case filelib:is_file("/etc/collectd/collectd.conf") of
        false ->
            ok;
        true ->
            [Ip1, Ip2, Ip3, Ip4] = binary:split(IpAddress, <<".">>, [global]),

            Cmd = ""
                ++"sudo sed -ni.bak "
                ++"'/    Server \""
                ++ binary_to_list(Ip1) ++"\."
                ++ binary_to_list(Ip2) ++"\."
                ++ binary_to_list(Ip3) ++"\."
                ++ binary_to_list(Ip4) ++"\" \"25826\"/!p' "
                ++"/etc/collectd/collectd.conf"
                ++" && "
                ++"sudo /etc/init.d/collectd restart",

            os:cmd(Cmd),
            ok
    end.

clear_any_collectd_server_ip_addresses() ->
    case filelib:is_file("/etc/collectd/collectd.conf") of
        false ->
            ok;
        true ->
            Cmd = ""
                ++"sudo sed -i.bak "
                ++"'/^    Server \"/d' "
                ++"/etc/collectd/collectd.conf"
                ++" && "
                ++"sudo /etc/init.d/collectd restart",

            os:cmd(Cmd),
            ok
    end.
