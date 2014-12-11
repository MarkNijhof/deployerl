-module(deployerl_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    setup_exometer(),
%%    io:format(user, "ROLES: ~p~n", [config:get_roles()]),
    deployerl_sup:start_link().

stop(_State) ->
    lager:warning("Application ~p exited with reason: stopped~n", [node()]),
    ok.

setup_exometer() ->
    exometer:new([erlang, system_info],
                 {function, erlang, system_info, ['$dp'], value, [port_count, process_count, schedulers_online, thread_pool_size]}),
    ok.
