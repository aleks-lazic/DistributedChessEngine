# Distributed Chess Engine
## Description
Distributed chess engine, exploring and evaluating future possible moves to determine the best move to play.
## Requirements
* Erlang https://www.erlang.org/downloads
* Java https://www.java.com/fr/download
## Engine instructions (ia\ia.erl)
In a terminal, run the java chess library node: `java -jar game/game.jar <HOSTNAME> <COOKIE>`, where <HOSTNAME> is this machine hostname and <COOKIE> is a secret word used for Erlang communication between nodes.
In `DistributedChessEngine/ia/`Open an Enrlang terminal: `erl -name <HOSTNAME> -setcookie <COOKIE>`, where <HOSTNAME> is this machine hostname and with the same <COOKIE> as above.
Compile the file ia.erl: `c(ia).`.
### Function `ia:master(javahost, depth, [fen]).`
Start or resume a given game.
* `javahost` is the hostname of the running java chess library node
* `depth` is the number of moves ahead the program will look for
* `fen`is a given chess board disposition, in Forsyth–Edwards Notation (FEN). If not provided, the program will strat a new game.
## Java API orders
Fen format: `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`  
Move format: `a2a3`
### Get Legal Fens
Request: `{java, 'master@HOSTNAME'} ! {self(), getLegalMoves, {"CURRENTFEN"}}`  
Response: `{sender, getLegalMoves, {[RESULTSFENLIST]}`  
Error: `{sender, error, {"Error"}}`
### White Move
Request: `{java, 'master@HOSTNAME'} ! {self(), whiteMove, {"CURRENTFEN", "MOVE"}}`  
Response: `{sender, whiteMove, {[RESULTFEN]}`  
Error: `{sender, error, {"Error"}}`
