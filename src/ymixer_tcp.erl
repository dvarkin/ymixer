-module(ymixer_tcp).

-export([send/3, send/2, send/1]).

%-include("http_internal.hrl").

send(Message) ->
    send({172,16,45,45}, Message).

send(Host, Message) ->
    send(Host, 49280, Message).

send(Host, PortNo, Message) ->
    {ok,Sock} = gen_tcp:connect(Host,PortNo,[binary, {packet, 0}, {active,false}]),
    gen_tcp:send(Sock,Message),
    A = gen_tcp:recv(Sock,0),
    gen_tcp:close(Sock),
    A.
