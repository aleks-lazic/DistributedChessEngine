-module(connection).
% -export([start/0,pong/0]).
     
% pong() ->
%     receive
%         stop ->
%             io:format("Pong finished...~n",[]);         
%         {PingId,ping} ->
%             io:format("Ping~n",[]),
%             PingId ! {self(),pong},
%             pong()
%     end.
 
% start() ->
%         register(pong,spawn(?MODULE,pong,[])),
%         pong().


-export([count/0]).

count() ->
	PidJava = counterserver,
	{PidJava, 'server@aleks'} ! {self(), "count"},
    io:format("Counter is at value: test count après envoi message~n", []),
	receive
		{ok, Counter} ->
			io:format("Counter is at value: ~p~n", [Counter]),
            count()
	end.