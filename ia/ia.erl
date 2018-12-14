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
-import(lists, [max/1, nth/2]).
-export([master/1, nextMove/4, nextStep/6, spawnProcesses/4, evaluate/1]).

master(TimeToPlay) ->
  %Save the master pid to pass messages
  PidMaster = self(),

  %Save the java pid to pass messages
  PidJava = java,
  HostJava = 'master@aleks',

  %Fen for the initial board state
  CurrentFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",

  %Send message to start the game
  self() ! {whiteToPlay},

  %start listening for the messages
  nextMove(PidMaster, PidJava, HostJava, CurrentFen).

nextMove(PidMaster, PidJava, HostJava, CurrentFen) ->
  receive
    %Beginning of the game, white starts
    {whiteToPlay} ->
      {ok, Term} = io:read("white to play, enter a move : "),
      io:format("White play : ~p~n", [Term]),
      {PidJava, HostJava} ! {self(), whiteMove, {CurrentFen, Term}},
      nextMove(PidMaster, PidJava, HostJava,CurrentFen);

    %The white cannot play that move (illegal move)
    %TODO : specify errors
    {_, error, {TextError}} ->
      io:format("~p~n", [TextError]),
      self() ! {whiteToPlay},
      nextMove(PidMaster, PidJava, HostJava,CurrentFen);

    %The white just moved
    {_, whiteMove, {Fen}} ->
      %get the legal moves for the black to play
      {PidJava, HostJava} ! {self(), getLegalMoves, {Fen}},
      nextMove(PidMaster, PidJava, HostJava,Fen);

    %Received legal moves for the black to play
    {_, getLegalMoves, {ListFen}} ->
      io:format("LIST FEN RECU : ~p~n", [ListFen]),
      spawnProcesses(PidMaster, PidJava, HostJava,ListFen)
  end.

nextStep(PidMaster, PidJava, HostJava,BaseFen, Value, Counter) ->
  receive
    %The black player can start searching for the best move
    {start} ->
      {PidJava, HostJava} ! {self(), getLegalMoves, {BaseFen}},
      nextStep(PidMaster, PidJava, HostJava, BaseFen, Value, Counter-1);

    %Received legal moves for the black to play
    {Pid, getLegalMoves, {ListFen}} ->
      %Calculate evaluation depending on FEN
      Moves = [{Fen, evaluate(Fen)} || Fen <- ListFen],
      MaxEval = max([Eval || {_, Eval} <- Moves]),
      GoodFens = [Fen || {Fen, Eval} <- Moves, Eval == MaxEval],
      RandIndex = rand:uniform(length(GoodFens)),
      FinalFen = nth(RandIndex, GoodFens),
      io:format("Final Move : ~p~n", [{FinalFen, MaxEval}])
  end.

spawnProcesses(_, _, _, []) -> ok;
spawnProcesses(PidMaster, PidJava, HostJava,[H|T]) ->
  Process = spawn(?MODULE, nextStep, [PidMaster, PidJava, HostJava,H, 0, 3]),
  Process ! {start},
  io:format("Process spawned.~n", []),
  spawnProcesses(PidMaster,PidJava,HostJava, T).

% Evaluate the balance of a given board
% Positive result means that the black player have advantage
% Negative result means that the white player have advantage
% Null (=zero) result means that the board is balanced
evaluate([32|_]) -> 0;
evaluate([H|T]) -> pieceValue(H) + evaluate(T).

% Arbitral values of chess pieces used for evaluation
pieceValue($p) -> 1;
pieceValue($n) -> 3;
pieceValue($b) -> 3;
pieceValue($r) -> 5;
pieceValue($q) -> 9;
pieceValue($P) -> -1;
pieceValue($N) -> -3;
pieceValue($B) -> -3;
pieceValue($R) -> -5;
pieceValue($Q) -> -9;
pieceValue(_) -> 0.
