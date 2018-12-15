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
        {Father_fen, Move, Rank, Fen, Eval} = tuple_space:in(TupleSpace, {string, string, integer, string, boolean}),
        % send java a request about move generation {pid_sender, task, {args}}
        {JavaProcess, Host} ! {self(), extend, {Father_fen, Move, Rank, FEN, Eval}},
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
        linda_kernel:out(TupleSpace, {GFather_fen, PreviousMove, Rank, FEN, Eval, true}),
        % Now we can and do the same for other position
        self() ! {Pid, search, DS},
        explorator(JavaProcess, Host, MasterPID, DSearch, TupleSpace);
end.

%% ----------------------------------------------------------------------------------------------
%% evaluation is the process in charge of judging the quality of a position
%% ----------------------------------------------------------------------------------------------
evaluation(MasterPID, TupleSpace)
    receive
    {Pid, evaluate} ->
        % take a tuple to evaluate
        {Father_fen, Move, Rank, Fen} = tuple_space:in(TupleSpace, {string, string, integer, string}),
        % associate a value to it
        Eval = evaluate(FEN).
        % put back with evaluation in the tuplespace
        linda_kernel:out(TupleSpace, {Father_fen, Move, Rank, Fen, Eval}),
        evaluation(MasterPID, TupleSpace);

    %% TODO
    %% evaluation should also in certain situation trace back the evaluation of positions to its father
    %% So the quality of a position depends from what is going to happen from it.

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

