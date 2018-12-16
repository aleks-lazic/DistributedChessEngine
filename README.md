# DistributedChessEngine
## Description
Distributed chess engine, exploring and evaluating future possible moves to determine the best move to play.
## Requirements
* Erlang https://www.erlang.org/downloads
* Java https://www.java.com/fr/download
## Engine instructions (ia\ia.erl)
Open an Enrlang terminal in `DistributedChessEngine/ia/`, compile the file `c(ia).`.
### `ia:master(depth, [fen]).`
Start or resume a given game.
*`depth` is the number of moves ahead the program will look for
*`fen`is a given chess board disposition, in Forsyth–Edwards Notation (FEN). If not provided, the program will strat a new game.
## Java API orders
Fen format: `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`  
Move format: `a2a3`
### Get Legal Moves
Request: `{java, 'master@HOSTNAME'} ! {self(), getLegalMoves, {"CURRENTFEN"}}`  
Response: `{sender, getLegalMoves, {[RESULTSFENLIST]}`  
Error: `{sender, error, {"Error"}}`
### White Move
Request: `{java, 'master@HOSTNAME'} ! {self(), whiteMove, {"CURRENTFEN", "MOVE"}}`  
Response: `{sender, whiteMove, {[RESULTFEN]}`  
Error: `{sender, error, {"Error"}}`
