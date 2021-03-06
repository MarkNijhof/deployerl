-module(mod___config).

-export([get_cookie/0]).
-export([get_mode/0]).
-export([get_roles/0]).
-export([get_udp_port/0]).
-export([get_manifest/0]).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

get_cookie() ->
    get_config(cookie, 's0m3aw3s0m3c00kie').

get_mode() ->
    get_config(mode, client, atom).

get_roles() ->
    get_config(roles, [], atom_list).

get_udp_port() ->
    get_config(udp_port, 9999, int).

get_manifest() ->
    get_config(manifest, [], term).


%% --------------------------------------------------%%
%% PRIVATE
%% --------------------------------------------------%%

get_env_name(Key) ->
    "DEPLOYERL_" ++ string:to_upper(atom_to_list(Key)).


get_config(Key, Default) ->
    case application:get_env(deployerl, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.


get_config(Key, Default, term) ->
    get_config(Key, Default);
get_config(Key, Default, list) ->
    case os:getenv(get_env_name(Key)) of
        false ->
            get_config(Key, Default);
        Value ->
            Value
    end;
get_config(Key, Default, atom_list) ->
    case os:getenv(get_env_name(Key)) of
        false ->
            get_config(Key, Default);
        Value ->
            Parts = re:split(Value,"[,]", [{return,list},trim]),
            [list_to_atom(string:strip(Part)) || Part <- Parts]
    end;
get_config(Key, Default, atom) ->
    case os:getenv(get_env_name(Key)) of
        false ->
            get_config(Key, Default);
        Value ->
            list_to_atom(Value)
    end;
get_config(Key, Default, int) ->
    case os:getenv(get_env_name(Key)) of
        false ->
            get_config(Key, Default);
        Value ->
            list_to_integer(Value)
    end.
