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
-import(lists, [max/1, min/1, nth/2]).
-export([master/1, nextMove/5, nextStep/6, spawnProcesses/4, evaluate/1, determinateFinalMove/7, calculateNextMove/6]).

master(TimeToPlay) ->
  %Save the master pid to pass messages
  PidMaster = self(),

  %Save the java pid to pass messages
  PidJava = java,
  HostJava = 'master@aleks',

  %Fen for the initial board state
  CurrentFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",

  %Send message to start the game
  PidMaster ! {whiteToPlay},

  %start listening for the messages
  nextMove(PidMaster, PidJava, HostJava, CurrentFen, 0).

nextMove(PidMaster, PidJava, HostJava, CurrentFen, Seconds) ->
  receive
  %Beginning of the game, white starts
    {whiteToPlay} ->
      io:format("Current Fen : ~p~n", [CurrentFen]),
      {ok, Term} = io:read("white to play, enter a move : "),
      {PidJava, HostJava} ! {self(), whiteMove, {CurrentFen, Term}},
      nextMove(PidMaster, PidJava, HostJava, CurrentFen, Seconds);

  %The white cannot play that move (illegal move)
    {_, error, {TextError}} ->
      io:format("~p~n", [TextError]),
      self() ! {whiteToPlay},
      nextMove(PidMaster, PidJava, HostJava, CurrentFen, Seconds);

  %The white just moved
    {_, whiteMove, {Fen}} ->
      %get the legal moves for the black to play:$

      {Mega, Sec, Micro} = os:timestamp(),
      BeginTimeMillis = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
      getLegalMoves(PidJava, HostJava, self(), Fen),
      nextMove(PidMaster, PidJava, HostJava, CurrentFen, BeginTimeMillis);

  %Received legal moves for the black to play
    {_, getLegalMoves, {ListFen}} ->
      spawnProcesses(PidMaster, PidJava, HostJava, ListFen),
      determinateFinalMove(PidMaster, PidJava, HostJava, length(ListFen), 0, [], Seconds)
  end.



nextStep(PidMaster, PidJava, HostJava, TreeFen, Value, Counter) ->
  receive
  %The black player can start searching for the best move
    {start} ->
      {PidJava, HostJava} ! {self(), getLegalMoves, {nth(Counter + 1, TreeFen)}},
      nextStep(PidMaster, PidJava, HostJava, TreeFen, Value, Counter + 1);

  %Received legal moves for the black to play (white FEN)
    {Pid, getLegalMoves, {ListFen}} when Counter == 1 ->
      %Calculate evaluation depending on FEN
      Moves = [{Fen, evaluate(Fen)} || Fen <- ListFen],
      MinEval = min([Eval || {_, Eval} <- Moves]) + Value,
      GoodFens = [Fen || {Fen, Eval} <- Moves, Eval == MinEval],
      RandIndex = rand:uniform(length(GoodFens)),
      FinalFen = nth(RandIndex, GoodFens),
      {PidJava, HostJava} ! {self(), getLegalMoves, {FinalFen}},
      nextStep(PidMaster, PidJava, HostJava, TreeFen ++ [FinalFen], MinEval, Counter + 1);

  %Received legal moves for the black to play (black Fen)
    {Pid, getLegalMoves, {ListFen}} when Counter == 2 ->
      %Calculate evaluation depending on FEN
      Moves = [{Fen, evaluate(Fen)} || Fen <- ListFen],
      MaxEval = max([Eval || {_, Eval} <- Moves]) + Value,
      GoodFens = [Fen || {Fen, Eval} <- Moves, Eval == MaxEval],
      RandIndex = rand:uniform(length(GoodFens)),
      FinalFen = nth(RandIndex, GoodFens),
      PidMaster ! {self(), blackFinalMove, {TreeFen ++ [FinalFen], MaxEval}}
%%      {PidJava, HostJava} ! {self(), getLegalMoves, {FinalFen}},
%%      nextStep(PidMaster, PidJava, HostJava, FinalFen, MaxEval, Counter - 1)

  end.

calculateNextMove(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter) ->
  receive
  % Determine final move
    {Pid, getLegalMoves, {ListFen}} when Counter == 1 ->
      sendResultsToMaster(PidMaster, CurrentFenHistory, Value, ListFen, self());

  % Determine white move
    {Pid, getLegalMoves, {ListFen}} when Counter rem 2 == 0 ->
      calculateBestMoveWhite(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter - 1, ListFen, self());

% Determine black move
    {Pid, getLegalMoves, {ListFen}} when Counter rem 2 /= 0 ->
      calculateBestMoveBlack(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter - 1, ListFen, self())
  end.

determinateFinalMove(PidMaster, PidJava, HostJava, NbProcesses, MessagesReceived, ListFen, Time) ->
  receive
    {Pid, blackFinalMove, {FinalFen, Eval}} when NbProcesses - 1 /= MessagesReceived ->
      determinateFinalMove(PidMaster, PidJava, HostJava, NbProcesses, MessagesReceived + 1, ListFen ++ [{nth(1, FinalFen), Eval}], Time);

    {Pid, blackFinalMove, {FinalFen, Eval}} when NbProcesses - 1 == MessagesReceived ->
      FinalListFen = ListFen ++ [{nth(1, FinalFen), Eval}],
      MaxEval = max([Eval || {_, Eval} <- FinalListFen]),
      GoodFens = [Fen || {Fen, Eval} <- FinalListFen, Eval == MaxEval],
      RandIndex = rand:uniform(length(GoodFens)),
      FenPlayed = nth(RandIndex, GoodFens),

      {Mega, Sec, Micro} = os:timestamp(),
      EndTimeMillis = (Mega*1000000 + Sec)*1000 + round(Micro/1000),

      TotalTimeToPlay = (EndTimeMillis - Time)/1000,
      io:format("Time used to calculate black move : ~p seconds ~n", [TotalTimeToPlay]),

      %Send message for the white to play
      PidMaster ! {whiteToPlay},

      %start listening for the messages
      nextMove(PidMaster, PidJava, HostJava, FenPlayed, 0)

  end.

getLegalMoves(PidJava, HostJava, Process, Fen) ->
  {PidJava, HostJava} ! {Process, getLegalMoves, {Fen}}.


calculateBestMoveBlack(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter, ListFen, Process) ->
  Moves = [{Fen, evaluate(Fen)} || Fen <- ListFen],
  MaxEval = max([Eval || {_, Eval} <- Moves]),
  GoodFens = [Fen || {Fen, Eval} <- Moves, Eval == MaxEval],
  RandIndex = rand:uniform(length(GoodFens)),
  FinalFen = nth(RandIndex, GoodFens),
  getLegalMoves(PidJava, HostJava, Process, FinalFen),
  calculateNextMove(PidMaster, PidJava, HostJava, CurrentFenHistory ++ [FinalFen], MaxEval + Value, Counter).

calculateBestMoveWhite(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter, ListFen, Process) ->
  Moves = [{Fen, evaluate(Fen)} || Fen <- ListFen],
  MinEval = min([Eval || {_, Eval} <- Moves]),
  GoodFens = [Fen || {Fen, Eval} <- Moves, Eval == MinEval],
  RandIndex = rand:uniform(length(GoodFens)),
  FinalFen = nth(RandIndex, GoodFens),
  getLegalMoves(PidJava, HostJava, Process, FinalFen),
  calculateNextMove(PidMaster, PidJava, HostJava, CurrentFenHistory ++ [FinalFen], MinEval + Value, Counter).


sendResultsToMaster(PidMaster, CurrentFenHistory, Value, ListFen, Process) ->
  Moves = [{Fen, evaluate(Fen)} || Fen <- ListFen],
  MaxEval = max([Eval || {_, Eval} <- Moves]),
  GoodFens = [Fen || {Fen, Eval} <- Moves, Eval == MaxEval],
  RandIndex = rand:uniform(length(GoodFens)),
  FinalFen = nth(RandIndex, GoodFens),
  PidMaster ! {Process, blackFinalMove, {CurrentFenHistory ++ [FinalFen], MaxEval + Value}}.

spawnProcesses(_, _, _, []) -> ok;
spawnProcesses(PidMaster, PidJava, HostJava, [H | T]) ->
  Process = spawn(?MODULE, calculateNextMove, [PidMaster, PidJava, HostJava, [] ++ [H], 0, 10]),
  getLegalMoves(PidJava, HostJava, Process, H),
  io:format("Process spawned : ~p~n", [Process]),
  spawnProcesses(PidMaster, PidJava, HostJava, T).


% Evaluate the balance of a given board
% Positive result means that the black player have advantage
% Negative result means that the white player have advantage
% Null (=zero) result means that the board is balanced
evaluate([32 | _]) -> 0;
evaluate([H | T]) -> pieceValue(H) + evaluate(T).

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
