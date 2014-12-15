-module(wrk___manifest_loader).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-record(state, {
          method,
          type,
          url,
          roles = [],
          ttl = 5000
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
    {ok, parse_config(#state{}), 10}.

handle_info(timeout, State = #state{ttl = Ttl}) ->
    {noreply, load_manifest(State), Ttl}.

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

parse_config(State) ->
    Manifest = mod___config:get_manifest(),
    Method   = proplists:get_value(method, Manifest, tcp),
    Type     = proplists:get_value(type, Manifest, json),
    Url      = proplists:get_value(url, Manifest),
    State#state{method = Method,
                type = Type,
                url = Url}.

load_manifest(State = #state{url = undefined}) ->
    lager:error("No manifest url provided in the config~n", []),
    State;

load_manifest(State = #state{method = tcp, url = Url}) ->
    case hackney:request(get, Url, [], <<>>, []) of
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            parse_manifest(Body, State);
        {error, _Reason} ->
            State
    end.

parse_manifest(Body, State)
  when is_binary(Body) ->
    parse_manifest(jiffy:decode(Body, [return_maps]), State);

parse_manifest(Body, State) ->
    Ttl = maps:get(<<"ttl">>, Body, 5000),
    Roles = maps:get(<<"roles">>, Body, []),
    diff_roles(Roles, State#state{ttl = Ttl}).

diff_roles(Roles, State = #state{roles = Roles}) ->
    io:format(user, "ROLES: No change~n", []),
    State;

diff_roles(NewRoles, State = #state{roles = OldRoles}) ->
    AddedRoles = [Role || Role <- NewRoles, has_role(maps:get(<<"role">>, Role), OldRoles)],
    State#state{roles = NewRoles}.

has_role(Key, Roles) ->
    FoundRoles = [Role || Role <- Roles, maps:get(<<"role">>, Role) =:= Key],
    length(FoundRoles) > 0.
