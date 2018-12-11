# DistributedChessEngine
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
