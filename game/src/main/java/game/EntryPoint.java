package game;

import com.ericsson.otp.erlang.*;
import com.github.bhlangonijr.chesslib.move.MoveGeneratorException;

import java.io.IOException;

public class EntryPoint {

	public static OtpNode self;
	public static OtpMbox mbox;

	public static void main(String[] args) {
		try {

			self = new OtpNode("master@aleks", "cookie");
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
		case "getLegalFens":
			try {
				response = Chess.getLegalFens(tuple);
			} catch (MoveGeneratorException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			}
			break;
		case "getLegalMoves":
			try {
				response = Chess.getLegalMoves(tuple);
			} catch (MoveGeneratorException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			}
			break;
		case "extend":
			try {
				order = new OtpErlangAtom("extended");
				response = Chess.extend(tuple);
			} catch (MoveGeneratorException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			}
			break;
		case "whiteMove":
			try {
				response = Chess.move(tuple);
			} catch (IOException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			} catch (RuntimeException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			}
			break;
		case "blackMove":
			try {
				response = Chess.move(tuple);
			} catch (IOException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			} catch (RuntimeException e) {
				order = new OtpErlangAtom("error");
				response = new OtpErlangTuple(new OtpErlangString(e.getMessage()));
			}
			break;
		default:
			break;
		}
		OtpErlangObject[] msg = { mbox.self(), order, response };
		mbox.send(from, new OtpErlangTuple(msg));
	}

}
