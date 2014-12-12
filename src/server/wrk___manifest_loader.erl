-module(wrk___manifest_loader).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-record(state, {
          method,
          type,
          url,
          roles = []
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
    {ok, parse_config(#state{}), 5000}.

handle_info(timeout, State) ->
    {noreply, load_manifest(State), 5000}.

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
    Roles = maps:get(<<"roles">>, Body, []),
    diff_roles(Roles, State).

diff_roles(Roles, State) ->
    io:format(user, "ROLES: ~p~n~n", [Roles]),
    State#state{roles = Roles}.
