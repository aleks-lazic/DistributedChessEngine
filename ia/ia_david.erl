%%% Bucher David
%%% 11-827-995

%%% CDC PROJECT UNIFR SA18
%%% DISTRIBUTED CHESS ENGINE



-module(ia_david).
-export([master/3]).


%% master is the part of the program playing the move for the computer (playing the black pieces)
%% ----------------------------------------------------------------------------------------------

%% FEN the current position to play from, PGN the moves already played
master(FEN, PGN, Time_per_move) ->

  %% spawn the process needed
  %%

  Evaluation = spawn(chessIA, evaluation, [args]),
  Cleaning = spawn(chessIA, evaluation, [args]),

  Explorator_1 = spawn(chessIA, explorator, [args]),
  Explorator_2 = spawn(chessIA, explorator, [args]),
  Explorator_3 = spawn(chessIA, explorator, [args]),
  Explorator_4 = spawn(chessIA, explorator, [args]),
  Explorator_5 = spawn(chessIA, explorator, [args]),


  Evaluation ! {exploratorpid, [Explorator_1, Explorator_2, Explorator_3, Explorator_4, Explorator_5]},

  %% start to play
  %%

  self() ! {white_to_move, FEN, PGN},
  player(FEN, PGN, Time_per_move).


%% ----------------------------------------------------------------------------------------------
%% ----------------------------------------------------------------------------------------------


%% player is a sub part of the master process that handle the game itself after initialisation.
%% ----------------------------------------------------------------------------------------------
player(FEN, PGN, Timer) ->
  receive

    {white_to_move, Newfen, Newpgn} ->
      {masterjava, ''} ! {self(), "legalmove", Newfen}, %% get legal move from FEN
      player(Newfen, Newpgn, Timer);

    {white_legal_move, ListOfMove} ->
      %% print legal move
      self() ! {wait_white_move, ListOfMove},
      player(FEN, PGN, Timer);

    {wait_white_move, ListOfMove} ->
      %% read the standard output in Move

      %% if Move is not a legal move
      self() ! {white_legal_move, ListOfMove},
      player(FEN, PGN, Timer),

    %% if legal move
    %% update FEN and PGN
    {master_java, 'server@aleks'} ! {self(), "updateposition", FEN, PGN}, %Move},
    player(FEN, PGN, Timer)

%%{black_to_move, Newfen, Newpgn} ->
%%Evaluation ! {calcul_from, NewFEN, NewPDF},
%%erlang:send_after(Timer, Cleaning, {choose_my_move}),
%%player(NewFEN, NewPGN, Timer);

%%{move_to_play, My_move} ->
%%{masterjava, 'lkj'} ! {self(), "updateposition", FEN, PGN, Move},
%%player(FEN, PGN, Timer);

end .

%% ----------------------------------------------------------------------------------------------
%% ----------------------------------------------------------------------------------------------


%% explorator are the process in charge to calculate the possible moves from a position
%% ----------------------------------------------------------------------------------------------

%% use of the linda tuples space
%% {Father_fen, Move ,Rank, FEN, PGN, Value, Ext, Eval}
%% FEN -string- is the position stored in the tuple
%% PGN -string- is the game played to reach this position
%% Father_fen -string- is the FEN of the tuple from wich this tuple is affiliated
%% Rank -integer- is the number of move to play from the current position (chessboard of the game) to reach this position
%% Move -string- the move to play from Father_fen to reach the position FEN
%% Value -double- represent how good/bad the position is
%% Ext -boolean- if there are in the tuples position after this one
%% Eval -boolean- if Value has been calculated
%% Myturn -boolean- if it is a position where the computer is the one to make a move


explorator(MasterPID, EvalPID, DSearch, TupleSpace) ->
receive
  {search, DS} when DS > DSearch ->
    self() ! {search, DS},
    explorator(MasterPID, EvalPID, DS, TupleSpace);% ...);

    {search, DS} when DS == DSearch ->
    %% take from the tuplespace a tuple not extended
    {Father_fen, Rank, FEN, PGN, Value, Ext, Eval, Myturn} = tuple_space:in(TupleSpace, {string, DSearch, string, string, double, false, boolean, boolean}),
    {masterjava, ''} ! {self(), "extend", Father_fen, Rank, FEN, PGN, Value, Ext, Eval, Myturn},
    explorator(MasterPID, EvalPID, DSearch, TupleSpace) %, ...);

%%    {extended, DS, Father_fen, Rank, FEN, PGN, Value, Ext, Eval, ListOfNewPosition} ->
%%    tuple_space:out(TupleSpace, {{Father_fen, Move, Rank, FEN, PGN, Value, true, Eval, Myturn}}),
%%
%%    %% for all pos in ListOfNewPosition (form is [ (NextFEN, NextPGN) ]
%%    tuple_space:out(TupleSpace, {{FEN, Rank + 1, NextFEN, NextPGN, Value, false, false, Myturn}}),
%%    %%
%%    self() ! {search, DS},
%%    explorator(MasterPID, EvalPID, DSearch, TupleSpace, ...);
end.


%% ----------------------------------------------------------------------------------------------
%% ----------------------------------------------------------------------------------------------


%% evaluation is the process in charge of judging the quality of a position
%% ----------------------------------------------------------------------------------------------
%%evaluation(MasterPID, ExploratorPIDs, Dsearch, TupleSpace, ...)
%%receive
%%{evaluate, position} ->
%%%% take from the tuplespace a tuple not evaluated
%%{Father_fen, Rank, FEN, PGN, Value, Ext, Eval, Myturn} = tuple_space:in(TupleSpace, {string, integer, string, string, double, boolean, false, Myturn}),
%%%% evaluate
%%tuple_space:out(TupleSpace, {{Father_fen, Move, Rank, FEN, PGN, NewValue, Ext, true, Myturn}}),
%%self() ! {evaluate, position}
%%evaluation(MasterPID, ExploatorPIDs, Dsearch, TupleSpace, ...);
%%
%%{evaluate, Branch} ->


%% ----------------------------------------------------------------------------------------------
%% ----------------------------------------------------------------------------------------------


%% cleaning is a process that handle the search tree. it gives back to the master the best move to
%% play and empty the tuplespace
%%%% ----------------------------------------------------------------------------------------------
%%cleaning(MasterPID, EvalPID, TupleSpace, ...)
%%receive
%%{choose_my_move} ->
%%EvalPID ! {stop},
%%%% get all the tuple from the first generation
%%%% keep the one with the best evaluation = My_move
%%MasterPID ! {move_to_play, My_move},
%%%% remove the move calculated from the linda tuplespace
%%%% selfcall;


%% ----------------------------------------------------------------------------------------------
%% ----------------------------------------------------------------------------------------------







