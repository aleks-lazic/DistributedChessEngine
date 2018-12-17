%%%-------------------------------------------------------------------
%%% CDC PROJECT UNIFR SA18
%%% DISTRIBUTED CHESS ENGINE
%%% Aleksandar Lazic, David Bucher, Riccardo Zappoli
%%% 17.12.2018
%%%-------------------------------------------------------------------

-module(explorer).
-export([master/3, explorator/5, evaluation/3, cleanTree/2, synchro/1]).

%% ----------------------------------------------------------------------------------------------
%% master is the part of the program playing the move for the computer (playing the black pieces)
%% ----------------------------------------------------------------------------------------------

% FEN the current position to play from, PGN the moves already played
% N is the number of process in charge of calculation
master(FEN, N, Time_per_move) ->

  % master is the chess player
  ChessIA = self(),

  % it needs to communicate with a java program
  JavaMaster = 1,
  HostJava = 2,

  % use a linda tuple space
  linda_kernel:start(),
  ChessTree = linda_kernel:create_ts(),

  %% Spawn the different process
  % spawn a process and allow him to interacte with the tuplespace
  Cleaner = linda_kernel:spawn(ChessTree, ?MODULE, cleanTree, [ChessIA, ChessTree]),
  Evaluator = linda_kernel:spawn(ChessTree, ?MODULE, evaluation(), [ChessIA, ChessTree])
% Todo spawn N explorator using the tuplespace
% Brain : must be a list[{Explorator_Pid, 0}]
% DepthController = spawn(?MODULE, synchro,[Brain]),




%% start to play
%%

%% self() ! {white_to_move, FEN},
%% player(FEN, Time_per_move, Brain)
.



%% ----------------------------------------------------------------------------------------------
%% player is a sub part of the master process that handle the game itself after initialisation.
%% ----------------------------------------------------------------------------------------------

%% previous verison that need to be redo
player(FEN, Timer, Expl) ->
  receive

    {white_to_move, Newfen} ->
      {masterjava, ''} ! {self(), "legalmove", Newfen}, %% get legal move from FEN
      player(Newfen, Timer, Expl);

    {white_legal_move, ListOfMove} ->
      %% print legal move
      self() ! {wait_white_move, ListOfMove},
      player(FEN, Timer, Expl);

    {wait_white_move, ListOfMove} ->
      %% read the standard output in Move

      %% if Move is not a legal move
      self() ! {white_legal_move, ListOfMove},
      player(FEN, Timer, Expl),

      %% if legal move
      %% update FEN and PGN
      {master_java, 'server@aleks'} ! {self(), "updateposition", FEN}, %Move},
      player(FEN, Timer, Expl)

  %%{black_to_move, Newfen, Newpgn} ->
  %%Evaluation ! {calcul_from, NewFEN, NewPDF},
  %%erlang:send_after(Timer, Cleaning, {choose_my_move}),
  %%player(NewFEN, NewPGN, Timer);

  %%{move_to_play, My_move} ->
  %%{masterjava, 'lkj'} ! {self(), "updateposition", FEN, PGN, Move},
  %%player(FEN, PGN, Timer);

  end.


%% ----------------------------------------------------------------------------------------------
%% explorator are process calculate the possible move from a reference position
%% ----------------------------------------------------------------------------------------------

%   JavaProcess: the chess move genrator
%   Host:        the server handling the java/erlang passing msgs
%   MasterPID:   is the pid of the main process playing chess
%   DSearch:     is the current depth search in the chess position tree stored in the tuple space
%   TupleSpace:  where to store positions


explorator(JavaProcess, Host, MasterPID, DSearch, TupleSpace) ->
  receive
    {Pid, search, DS} when DS > DSearch ->
      % update the depth search
      self() ! {search, DS+1},
      explorator(JavaProcess, Host, MasterPID, DSearch+1, TupleSpace);

    {Pid, search, DS} when DS == DSearch ->
      % computation of the chess position tree by taking a position to extend
      {Father_fen, Move, Rank, Fen, Eval} = tuple_space:rd(TupleSpace, {string, string, integer, string, boolean}),
      % send java a request about move generation {pid_sender, task, {args}}
      {JavaProcess, Host} ! {self(), extend, {Father_fen, Move, Rank, Fen, Eval}},
      explorator(JavaProcess, Host, MasterPID, DSearch+1, TupleSpace);

    {Pid, extended, {GFather_fen, PreviousMove, Father_fen, Rank, Eval, [{Move, FEN}|T]}} ->
      % this msg is send by the java process
      % GFather_fen, Father_fen, PreviousMove, Rank, EVal are the args send by explorator() to the java move generator

      % the first new position is put in the linda
      linda_kernel:out(TupleSpace, {Father_fen, Move, Rank+1, FEN}),
      % "recursive" put the rest in the tuple space
      self() ! {self(), extended, {GFather_fen, PreviousMove, Father_fen, Rank, Eval, [T]}},
      explorator(JavaProcess, Host, MasterPID, DSearch, TupleSpace);

    {Pid, extended, {GFather_fen, PreviousMove, Father_fen, Rank, Eval, []}} ->
      % when the list is empty, all possible position from Father_fen have been calculated and put in the tupleSpace
      % put back the tuple that was taken out of the tuple space, now the position has been extended !
      linda_kernel:out(TupleSpace, {GFather_fen, PreviousMove, Rank, Father_fen, Eval, true}),
      % Now we can and do the same for other position
      self() ! {Pid, search, DSearch},
      explorator(JavaProcess, Host, MasterPID, DSearch, TupleSpace)
  end.



%% ----------------------------------------------------------------------------------------------
%% evaluation is the process in charge of judging the quality of a position
%% ----------------------------------------------------------------------------------------------


evaluation(MasterPID, TupleSpace, ABrank) ->
  receive
    {Pid, evaluate} ->
      % take a tuple to evaluate
      {Father_fen, Move, Rank, Fen} = tuple_space:rd(TupleSpace, {string, string, integer, string}),
      % associate a value to it
      Eval = evaluate(Fen),
      % put back with evaluation in the tuplespace
      linda_kernel:out(TupleSpace, {Father_fen, Move, Rank, Fen, Eval}),
      evaluation(MasterPID, TupleSpace);

    {Pid, rank, Rank} when Rank == ABrank ->
      %% TODO : use Alpha-beta to stop bad branch
      evaluation(MasterPID, TupleSpace, ABrank);

    {Pid, myMove} ->
      %% TODO : use of Minimax
      %% Send result to ChessIA

      evaluation(MasterPID, TupleSpace)
  end.



% evaluation function that takes a string (FEN) and return a value (integer)
evaluate([32|_]) -> 0;
evaluate([H|T]) -> pieceValue(H) + evaluate(T).

% Arbitral values of chess pieces used for evaluation
% black piece are positive (computer pieces)
% white piece are negative (oponent pieces)
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




%% ----------------------------------------------------------------------------------------------
%% logical time synchronisation based on the search rank
%% ----------------------------------------------------------------------------------------------

% List is of the form [{explorator pid, rank search}]
synchro(List = [{ExplPid, ExplRank}|T]) ->
  receive
    {Pid, done, Rank} ->
      NewList = updatelist({Pid, Rank}, List),
      self() ! {Pid, check, Rank},
      synchro(NewList);

    {Pid, check, Rank} ->
      if
      % allReady(Rank, List)  ->
      %    sendAll(List),
      %    synchro(List);

        true ->
          % not all ready to go on next rank
          synchro(List)
      end
  end.



updatelist({MyPid, MyRank}, []) -> [].
updatelist({MyPid, MyRank}, [{Pid, Rank}|T]) ->
  if
    {MyPid, MyRank} == {Pid, Rank} ->
      {MyPid, MyRank} ++ T;

    true ->
      {Pid, Rank} ++ udpatelist(T)
  end.


allReady(Rank, []) -> true.
allReady(Rank, [{Pid, LT}|T]) ->
  if
    Rank == LT ->
      true and allReady(Rank, T);

    true ->
      false
  end.

sendAll([]) -> true.
sendAll([{Pid, Rank}|T]) ->
  Pid ! {self(), search, Rank},
  sendAll(T).



%% ----------------------------------------------------------------------------------------------
%% Cleaning is the process in charge of removing from the tuplespace, useless position stored
%% ----------------------------------------------------------------------------------------------


% ChessIA:   the pid of the process playing chess
% TupleSpace: to clean

cleanTree(ChessIA, TupleSpace) ->
  receive
    {Pid, clean, FenToSave} ->
      % take a tuple form the memory
      {Father_fen, Move, Rank, FEN, Eval, Ext} = linda_kernel:rd(TupleSpace, {string, string, integer, string, float, boolean}),
      if
        FEN == FenToSave ->
          % put it back
          linda_kernel:in(TupleSpace, {Father_fen, Move, Rank, FEN, Eval, Ext}),
          self() ! {self(), clean, FenToSave},
          cleanTree(ChessIA, TupleSpace);

        true ->
          self() ! {self(), clean, FenToSave},
          cleanTree(ChessIA, TupleSpace)
      end
  end.
