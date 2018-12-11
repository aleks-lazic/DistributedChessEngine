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
-export([master/1]).

master(TimeToPlay) ->

  %Register the java pid to pass messages
  PidJava = java,

  %Fen for the initial board's state
  CurrentFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",

  %Send message to start the game
  self() ! {whiteToPlay},

  %start listening for the messages
  nextMove(PidJava, CurrentFen).

nextMove(PidJava, CurrentFen) ->
  receive
    %Beginning of the game, white starts
    {whiteToPlay} ->
      {ok, Term} = io:read("white to play, enter a move : "),
      io:format("White play : ~p~n", [Term]),
      {PidJava, 'master@RICC-SP3'} ! {self(), move, {CurrentFen, Term}},
      nextMove(PidJava, CurrentFen);

    %The white cannot play that move (illegal move)
    {Pid, error, {TextError}} ->
      io:format("~p~n", [TextError]),
      self() ! {whiteToPlay},
      nextMove(PidJava, CurrentFen);

    %The white can move
    {Pid, whiteMove, {Fen}} ->
      %get the legal moves for the black to play
      {PidJava, 'master@RICC-SP3'} ! {self(), getLegalMoves, {Fen}},
      nextMove(PidJava, Fen);

    %Get legal moves for the white to play
    {Pid, getLegalMoves, {ListFen}} ->
      io:format("LIST FEN RECU : ~p~n", [ListFen])
  end.



