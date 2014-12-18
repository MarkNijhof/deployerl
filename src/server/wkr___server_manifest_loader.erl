-module(wkr___server_manifest_loader).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-record(state, {
          method,
          type,
          url,
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
            maybe_redirect(jiffy:decode(Body, [return_maps]), State);
        {error, _Reason} ->
            State
    end.

maybe_redirect(Body, State)
  when is_binary(Body) ->
    maybe_redirect(jiffy:decode(Body, [return_maps]), State);
maybe_redirect(Body, State) ->
    case maps:get(<<"redirect_to_url">>, Body, no_redirect_defined) of
        no_redirect_defined ->
            parse_manifest(Body, State);
        NewManifestUrl ->
            Method = list_to_atom(maps:get(<<"method">>, Body, "tcp")),
            Type = list_to_atom(maps:get(<<"type">>, Body, "json")),
            load_manifest(State#state{method = Method,
                                      type = Type,
                                      url = NewManifestUrl})
    end.

parse_manifest(Body, State) ->
    Ttl = maps:get(<<"ttl">>, Body, 5000),
    Roles = maps:get(<<"roles">>, Body, []),
    publish_roles(Roles),
    State#state{ttl = Ttl}.

publish_roles([]) ->
    ok;
publish_roles([Role | Roles]) ->
    RoleName = binary_to_atom(maps:get(<<"role">>, Role), utf8),
    Applications = maps:get(<<"applications">>, Role),
    wkr___server_role_manager:register_applications(RoleName, Applications),
    publish_roles(Roles).
