%%%-------------------------------------------------------------------
%%% @author aleks
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2018 16:03
%%%-------------------------------------------------------------------
-module(ia).
%% API
-export([master/1, nextMove/3, nextStep/5, spawnProcesses/3]).

master(TimeToPlay) ->
  %Save the master pid to pass messages
  PidMaster = self(),
  io:format("PID MASTER : ~p~n", [PidMaster]),

  %Save the java pid to pass messages
  PidJava = java,

  %Fen for the initial board's state
  CurrentFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",

  %Send message to start the game
  self() ! {whiteToPlay},

  %start listening for the messages
  nextMove(PidMaster, PidJava, CurrentFen).

nextMove(PidMaster, PidJava, CurrentFen) ->
  receive
    %Beginning of the game, white starts
    {whiteToPlay} ->
      {ok, Term} = io:read("white to play, enter a move : "),
      io:format("White play : ~p~n", [Term]),
      {PidJava, 'master@RICC-SP3'} ! {self(), whiteMove, {CurrentFen, Term}},
      nextMove(PidMaster, PidJava, CurrentFen);

    %The white cannot play that move (illegal move)
    %TODO : specify errors
    {_, error, {TextError}} ->
      io:format("~p~n", [TextError]),
      self() ! {whiteToPlay},
      nextMove(PidMaster, PidJava, CurrentFen);

    %The white just moved
    {_, whiteMove, {Fen}} ->
      %get the legal moves for the black to play
      {PidJava, 'master@RICC-SP3'} ! {self(), getLegalMoves, {Fen}},
      nextMove(PidMaster, PidJava, Fen);

    %Recived legal moves for the black to play
    {_, getLegalMoves, {ListFen}} ->
      io:format("LIST FEN RECU : ~p~n", [ListFen]),
      spawnProcesses(PidMaster, PidJava, ListFen)
  end.

nextStep(PidMaster, PidJava, BaseFen, Value, Counter) ->
  receive
    {start} ->
      {PidJava, 'master@RICC-SP3'} ! {self(), getLegalMoves, {BaseFen}},
      nextStep(PidJava, BaseFen, Value, Counter-1);
    %Received legal moves for the black to play
    {Pid, getLegalMoves, {ListFen}} ->
      io:format("LIST FEN RECU : ~p~n", [ListFen])
  end.  

spawnProcesses(PidMaster, PidJava,[]) -> ok;
spawnProcesses(PidMaster, PidJava, [H|T]) ->
  Process = spawn(?MODULE, nextStep, [PidMaster, PidJava, H, 0, 3]),
  Process ! {start},
  io:format("Process spawned.~n", []),
  spawnProcesses(PidMaster,PidJava, T).


