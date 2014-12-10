-module(config).

-export([get_mode/0]).
-export([get_udp_port/0]).

get_mode() ->
    case os:getenv("DEPLOYERL_MODE") of
        false ->
            get_config(mode, client);
        Mode ->
            list_to_atom(Mode)
    end.

get_udp_port() ->
    get_config(udp_port, 9999).



get_config(Key, Default) ->
    case application:get_env(deployerl, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.
