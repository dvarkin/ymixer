-module(ymixer_tcp).

-export([send/3]).

send(Host, PortNo, Message) ->
    {ok,Sock} = gen_tcp:connect(Host,PortNo,[{active,false},
                                                    {packet,2}]),
    gen_tcp:send(Sock,Message),
    A = gen_tcp:recv(Sock,0),
    gen_tcp:close(Sock),
    A.
