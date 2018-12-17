# Distributed Chess Engine
## Description
Distributed chess engine, exploring and evaluating future possible moves to determine the best move to play.
## Requirements
* Erlang: https://www.erlang.org/downloads
* Java: https://www.java.com/fr/download
## Engine instructions
* In a terminal, run the java chess library node: `java -jar game/game.jar <NODE>@<HOSTNAME> <COOKIE>`, where `<NODE>` will be the name of the node,`<HOSTNAME>` is this machine hostname and `<COOKIE>` is a secret word used for Erlang communication between nodes.
* In `DistributedChessEngine/ia/`Open an Erlang terminal: `erl -name <HOSTNAME> -setcookie <COOKIE>`, where `<HOSTNAME>` is this machine hostname and with the same `<COOKIE>` as above.
* Compile the file ia.erl: `c(ia).`.
### Function `ia:master(javahost, depth, [fen]).`
Start or resume a given game.
* `javahost` is the hostname of the running java chess library node, format: `<NODE>@<HOSTNAME>`
* `depth` is the number of moves ahead the program will look for
* `fen` is a given chess board disposition, in Forsyth–Edwards Notation (FEN). If not provided, the program will start a new game.
## Linda tuple space test
* Start erlang in terminal with: `erl -pa Linda-master/src`
* And make sure that in the `Linda-master/src` folder for every `.erl` files you also have the corresponding `.beam`.
* Try to call with start position: `"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"`
## Java API orders
Board format: `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`  
Move format: `a2a3`
### Get Legal Fens
Request: `{java, <HOSTNAME>} ! {self(), getLegalFens, {<BASEFEN>}}`  
Response: `{senderPid, getLegalFens, {[<RESULTFENSLIST>]}`  
Error: `{senderPid, error, {<ERRORMESSAGE>}}`
### Get Legal Moves
Request: `{java, <HOSTNAME>} ! {self(), getLegalMoves, {<BASEFEN>}}`  
Response: `{senderPid, getLegalMoves, {[<RESULTMOVESLIST>]}`  
Error: `{senderPid, error, {<ERRORMESSAGE>}}`
### Extend
Request: `{java, <HOSTNAME>} ! {self(), extend, {_,_,_,<BASEFEN>,_}}`  
Response: `{senderPid, extended, {[_,_,_,<BASEFEN>,_,[{<RESULTMOVES>,<RESULTFENS>}]]}`  
Error: `{senderPid, error, {<ERRORMESSAGE>}}`
### White Move
Request: `{java, <HOSTNAME>} ! {self(), whiteMove, {<BASEFEN>, <MOVE>}}`  
Response: `{senderPid, whiteMove, {[<RESULTFEN>]}`  
Error: `{senderPid, error, {<ERRORMESSAGE>}}`
### Black Move
Request: `{java, <HOSTNAME>} ! {self(), blackMove, {<BASEFEN>, <MOVE>}}`  
Response: `{senderPid, blackMove, {[<RESULTFEN>]}`  
Error: `{senderPid, error, {<ERRORMESSAGE>}}`
