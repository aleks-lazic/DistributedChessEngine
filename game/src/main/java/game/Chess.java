package game;

import java.io.*;
import java.util.*;

import com.ericsson.otp.erlang.*;
import com.github.bhlangonijr.chesslib.*;
import com.github.bhlangonijr.chesslib.move.*;

public class Chess {
	
	public static OtpErlangTuple getLegalMoves(OtpErlangTuple tuple) throws MoveGeneratorException {
		System.out.println("Tuple: " + tuple);
		String fen = tuple.elementAt(0).toString().replace("\"", "");
		Board board = new Board();
		board.loadFromFen(fen);
		MoveList moves = MoveGenerator.generateLegalMoves(board);
		OtpErlangString[] results = new OtpErlangString[moves.size()];
		for (int i = 0; i < results.length; i++) {
			board.loadFromFen(fen);
			board.doMove(moves.get(i));
			results[i] = new OtpErlangString(board.getFen());
		}
		return new OtpErlangTuple(new OtpErlangList(results));
	}
	
	public static OtpErlangTuple extend(OtpErlangTuple tuple) throws MoveGeneratorException {
		System.out.println("Tuple: " + tuple);
		String fen = tuple.elementAt(3).toString().replace("\"", "").replace("\'", "");
		Board board = new Board();
		board.loadFromFen(fen);
		MoveList moves = MoveGenerator.generateLegalMoves(board);
		OtpErlangTuple[] results = new OtpErlangTuple[moves.size()];
		for (int i = 0; i < results.length; i++) {
			board.loadFromFen(fen);
			board.doMove(moves.get(i));
			OtpErlangString[] element = {new OtpErlangString(moves.get(i).toString()), new OtpErlangString(board.getFen())};
			results[i] = new OtpErlangTuple(element);
		}
		ArrayList<OtpErlangObject> response = new ArrayList<OtpErlangObject>();
		Collections.addAll(response, tuple.elements());
		response.add(new OtpErlangList(results));
		return new OtpErlangTuple(response.toArray(new OtpErlangObject[response.size()]));
	}
	
	public static OtpErlangTuple move(OtpErlangTuple tuple) throws IOException, RuntimeException {
		System.out.println("Tuple: " + tuple);
		String fen = tuple.elementAt(0).toString().replace("\"", "");
		Move move = new Move(tuple.elementAt(1).toString().replace("\"", ""), null);
		Board board = new Board();
		board.loadFromFen(fen);
		if (board.doMove(move, true)) {
			return new OtpErlangTuple(new OtpErlangString(board.getFen()));
		} else {
			throw new IOException("Illegal move.");
		}
	}

}
