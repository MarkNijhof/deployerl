-module(handler___websocket).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
    worker___communicator:connect_web_socket_client(),
    {cowboy_websocket, Req, Opts}.

websocket_handle(_IgnorIncommingMessage, Req, State) ->
    {ok, Req, State}.

websocket_info({reply, Message}, Req, State) ->
    JsonMessage = jiffy:encode(Message),
    {reply, {text, JsonMessage}, Req, State}.
