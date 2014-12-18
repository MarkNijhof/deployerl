-module(wkr___client_connector).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/0]).
-export([connected_to_server/0]).
-export([disconnected_from_server/0]).

-record(state, {
          udp_port,
          connected_to_server = false,
          roles
         }).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connected_to_server() ->
    ?MODULE ! connected_to_server.

disconnected_from_server() ->
    ?MODULE ! disconnected_from_server.

%% --------------------------------------------------%%
%% GEN SERVER
%% --------------------------------------------------%%

init([]) ->
    {ok, do_init(#state{})}.

handle_info(timeout, State = #state{connected_to_server = false}) ->
    {noreply, udp_broadcast(State), 5000};

handle_info(start_role_managers, State) ->
    {noreply, start_role_managers(State), 10};

handle_info(disconnected_from_server, State) ->
    {noreply, State#state{connected_to_server = false}, 5000};

handle_info(connected_to_server, State) ->
    {noreply, State#state{connected_to_server = true}}.

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

do_init(State) ->
    self() ! start_role_managers,
    State#state{udp_port = mod___config:get_udp_port()}.

start_role_managers(State) ->
    Roles = [{Role, sup___client_role_managers:get_role_manager_pid(Role)} || Role <- mod___config:get_roles()],
    State#state{roles = Roles}.

udp_broadcast(State = #state{connected_to_server = false,
                             udp_port = UdpPort,
                             roles = Roles}) ->
    lager:info("Deployerl is broadcasting to UDP port: ~p~n", [UdpPort]),
    {ok, SendSocket} = gen_udp:open(0, [binary, {broadcast, true}]),
    ok = gen_udp:send(SendSocket,
                      {255, 255, 255, 255},
                      UdpPort,
                      term_to_binary({register_client, whereis(communicator), node(), Roles})),
    ok = gen_udp:close(SendSocket),
    State;

udp_broadcast(State) ->
    State.
