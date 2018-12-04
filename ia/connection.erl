-module(connection).
-export([start/0,pong/0]).
     
pong() ->
    receive
        stop ->
            io:format("Pong finished...~n",[]);         
        {PingId,ping} ->
            io:format("Ping~n",[]),
            PingId ! {self(),pong},
            pong()
    end.
 
start() ->
        register(pong,spawn(?MODULE,pong,[])).