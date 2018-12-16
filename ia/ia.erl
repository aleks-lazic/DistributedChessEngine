%%%-------------------------------------------------------------------
%%% CDC PROJECT UNIFR SA18
%%% DISTRIBUTED CHESS ENGINE
%%% Aleksandar Lazic, David Bucher, Riccardo Zappoli
%%% 17.12.2018
%%%-------------------------------------------------------------------

-module(ia).

-import(lists, [max/1, min/1, nth/2]).
-export([master/2, master/1, whiteTurn/6, nextMove/6, determinateFinalMove/8]).

% Starter function
master(CurrentFen, DepthSearch) ->
  % Save the master pid to pass messages
  PidMaster = self(),
  % Save the java pid to pass messages
  PidJava = java,
  HostJava = 'master@aleks',
  % Send message to start the game
  PidMaster ! {self(), whiteToPlay, {}},
  % Start listening for the messages
  whiteTurn(PidMaster, PidJava, HostJava, CurrentFen, 0, DepthSearch).

master(DepthSearch) ->
  % Forsyth–Edwards Notation (FEN) for the initial board state
  master("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -", DepthSearch).

whiteTurn(PidMaster, PidJava, HostJava, CurrentFen, Seconds, DepthSearch) ->
  receive
    % White player turn, get avalabile moves
    {_, whiteToPlay, {}} ->
      io:format("Current Fen : ~p~n", [CurrentFen]),
      {PidJava, HostJava} ! {PidMaster, getLegalMoves, {CurrentFen}},
      whiteTurn(PidMaster, PidJava, HostJava, CurrentFen, Seconds, DepthSearch);
    % White player turn, print avalabile moves and reads player input
    {_, getLegalMoves, {ListMoves}} ->
      io:format("White player turn, avalible moves : ~p~n", [ListMoves]),
      if % If no moves are allowed = checkmate
        length(ListMoves) == 0 ->
          io:format("Checkmate, you lost.~n", []);
        true ->
          {ok, Term} = io:read("Enter a move : "),
      {PidJava, HostJava} ! {PidMaster, whiteMove, {CurrentFen, Term}},
      whiteTurn(PidMaster, PidJava, HostJava, CurrentFen, Seconds, DepthSearch)
      end;
    % The white player just moved
    {_, whiteMove, {Fen}} ->
      % Timer
      {Mega, Sec, Micro} = os:timestamp(),
      BeginTimeMillis = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
      % Get the legal moves (as FENs) for the black player
      {PidJava, HostJava} ! {PidMaster, getLegalFens, {Fen}},
      whiteTurn(PidMaster, PidJava, HostJava, CurrentFen, BeginTimeMillis, DepthSearch);
    % Error, the white player cannot play that move (illegal move)
    {_, error, {TextError}} ->
      io:format("~p~n", [TextError]),
      PidMaster ! {self(), whiteToPlay, {}},
      whiteTurn(PidMaster, PidJava, HostJava, CurrentFen, Seconds, DepthSearch);
    % Received legal moves (as FENs) for the black player
    {_, getLegalFens, {ListFen}} ->
      if % If no moves are allowed = checkmate
        length(ListFen) == 0 ->
          io:format("Checkmate, you win.~n", []);
        true ->
          spawnProcesses(PidMaster, PidJava, HostJava, ListFen, DepthSearch),
          determinateFinalMove(PidMaster, PidJava, HostJava, length(ListFen), 0, [], Seconds, DepthSearch)
      end
  end.

% Spawns as many processes as there are differents blacks legal moves
spawnProcesses(_, _, _, [], _) -> ok;
spawnProcesses(PidMaster, PidJava, HostJava, [H | T], DepthSearch) ->
  Process = spawn(?MODULE, nextMove, [PidMaster, PidJava, HostJava, [] ++ [H], 0, DepthSearch]),
  {PidJava, HostJava} ! {Process, getLegalFens, {H}},
  spawnProcesses(PidMaster, PidJava, HostJava, T, DepthSearch).

nextMove(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter) ->
  receive
    % Determine the final move
    {_, getLegalFens, {_}} when Counter == 0 ->
      PidMaster ! {self(), blackFinalMove, {CurrentFenHistory, Value}};
    % Evaluate a white move
    {_, getLegalFens, {ListFen}} when Counter rem 2 == 0 ->
      evaluatewhiteTurn(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter - 1, ListFen);
    % Evaluate a black move
    {_, getLegalFens, {ListFen}} when Counter rem 2 /= 0 ->
      evaluateBlackMove(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter - 1, ListFen)
  end.

evaluateBlackMove(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter, ListFen) ->
  % Evaluation
  Moves = [{Fen, evaluate(Fen)} || Fen <- ListFen],
  if % No moves allowed = checkmate
    length(Moves) == 0 ->
      MaxEval = 5 + (-5 * Counter * Counter),
      PidMaster ! {self(), blackFinalMove, {CurrentFenHistory, MaxEval + Value}};
    true ->
      % Get the best play for the black player, chooses first by evaluation, then randomly
      MaxEval = max([Eval || {_, Eval} <- Moves]),
      GoodFens = [Fen || {Fen, Eval} <- Moves, Eval == MaxEval],
      RandIndex = rand:uniform(length(GoodFens)),
      FinalFen = nth(RandIndex, GoodFens),
      {PidJava, HostJava} ! {self(), getLegalFens, {FinalFen}},
      nextMove(PidMaster, PidJava, HostJava, CurrentFenHistory ++ [FinalFen], MaxEval + Value, Counter)
  end.

evaluatewhiteTurn(PidMaster, PidJava, HostJava, CurrentFenHistory, Value, Counter, ListFen) ->
  % Evaluation
  Moves = [{Fen, evaluate(Fen)} || Fen <- ListFen],
  if % No moves allowed = checkmate
    length(Moves) == 0 ->
      MinEval = 5 + (5 * Counter * Counter),
      PidMaster ! {self(), blackFinalMove, {CurrentFenHistory, MinEval + Value}};
    true ->
      % Get the worst play for the black player, chooses first by evaluation, then randomly
      MinEval = min([Eval || {_, Eval} <- Moves]),
      GoodFens = [Fen || {Fen, Eval} <- Moves, Eval == MinEval],
      RandIndex = rand:uniform(length(GoodFens)),
      FinalFen = nth(RandIndex, GoodFens),
      {PidJava, HostJava} ! {self(), getLegalFens, {FinalFen}},
      nextMove(PidMaster, PidJava, HostJava, CurrentFenHistory ++ [FinalFen], MinEval + Value, Counter)
  end.

determinateFinalMove(PidMaster, PidJava, HostJava, NbProcesses, MessagesReceived, ListFen, Time, DepthSearch) ->
  receive
    % Not all processes have replied
    {_, blackFinalMove, {FinalFen, FinalEval}} when NbProcesses - 1 /= MessagesReceived ->
      determinateFinalMove(PidMaster, PidJava, HostJava, NbProcesses, MessagesReceived + 1, ListFen ++ [{nth(1, FinalFen), FinalEval}], Time, DepthSearch);
    % All processes have replied
    {_, blackFinalMove, {FinalFen, FinalEval}} when NbProcesses - 1 == MessagesReceived ->
      % Chooses the best branch to play
      FinalListFen = ListFen ++ [{nth(1, FinalFen), FinalEval}],
      io:format("FinalListFen : ~p~n", [FinalListFen]),
      MaxEval = max([Eval || {_, Eval} <- FinalListFen]),
      GoodFens = [Fen || {Fen, Eval} <- FinalListFen, Eval == MaxEval],
      RandIndex = rand:uniform(length(GoodFens)),
      FenPlayed = nth(RandIndex, GoodFens),
      % Timer
      {Mega, Sec, Micro} = os:timestamp(),
      EndTimeMillis = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
      TotalTimeToPlay = (EndTimeMillis - Time)/1000,
      io:format("Time used to calculate black move : ~p seconds ~n", [TotalTimeToPlay]),
      % It's white player turn, with the new fen
      PidMaster ! {self(), whiteToPlay, {}},
      whiteTurn(PidMaster, PidJava, HostJava, FenPlayed, 0, DepthSearch)
  end.

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
