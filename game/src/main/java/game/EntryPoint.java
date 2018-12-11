package game;

import java.io.*;
import com.ericsson.otp.erlang.*;
import com.github.bhlangonijr.chesslib.move.MoveGeneratorException;


public class EntryPoint {
	
	public static OtpNode self;
	public static OtpMbox mbox;

	public static void main(String[] args) {
		try {
			self = new OtpNode("master", "cookie");
			mbox = self.createMbox("java");
			OtpErlangObject o;
			OtpErlangTuple msg;
			OtpErlangPid from;
			OtpErlangAtom order;
			OtpErlangTuple tuple;
			System.out.println("Started");

			while (true) {
				try {
					o = mbox.receive();
					if (o instanceof OtpErlangTuple) {
						msg = (OtpErlangTuple) o;
						from = (OtpErlangPid) msg.elementAt(0);
						order = (OtpErlangAtom) msg.elementAt(1);
						tuple = (OtpErlangTuple) msg.elementAt(2);
						sortMsg(from, order, tuple);
					}
					
				} catch (Exception e) {
					System.out.println("" + e);
					e.printStackTrace();
				}
			}
			
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void sortMsg(OtpErlangPid from, OtpErlangAtom order, OtpErlangTuple tuple) {
		OtpErlangTuple response = null;
		switch (order.toString()) {
			case "getLegalMoves": try {
				response = Chess.getLegalMoves(tuple);
			} catch (MoveGeneratorException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			} break;
			case "whiteMove": try {
				response = Chess.move(tuple);
			} catch (IOException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			} break;
			case "blackMove": try {
				response = Chess.move(tuple);
			} catch (IOException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			} break;
		default:
			break;
		}
		OtpErlangObject[] msg = {mbox.self(), order, response};
		mbox.send(from, new OtpErlangTuple(msg));
	}

}
