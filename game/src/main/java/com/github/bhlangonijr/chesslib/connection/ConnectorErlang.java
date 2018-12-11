package com.github.bhlangonijr.chesslib.connection;

import java.io.IOException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class ConnectorErlang {
	private static final String SERVER = "server";

	public static OtpNode connectToServer(String nodeName, String cookie) {
		OtpNode self = null;
		try {
			self = new OtpNode(nodeName, cookie);

			if (self.ping(SERVER, 2000)) {
				System.out.println("remote is up");
				return self;
			} else {
				System.out.println("remote is not up");
				return null;
			}
		} catch (IOException e1) {
			e1.printStackTrace();
			return null;
		}
	}

	public static void main(String[] _args) throws Exception {

		OtpNode node = connectToServer("myNode", "cookie");
		OtpMbox mbox = node.createMbox("mailBox");
		
		System.out.println("BEFORE PONG");
		mbox.send("pong", new OtpErlangAtom("stop"));
		System.out.println("AFTER PONG");

//		OtpErlangObject[] msg = new OtpErlangObject[1];
//		msg[0] = mbox.self();
//		System.out.println(mbox.self().toString());
//		msg[1] = new OtpErlangAtom("ping");
//		OtpErlangTuple tuple = new OtpErlangTuple(msg);
//		mbox.send("pong", tuple);

//		while (true)
//			try {
//				System.out.println("Hello");
//				OtpErlangObject robj = mbox.receive();
//				OtpErlangTuple rtuple = (OtpErlangTuple) robj;
//				OtpErlangPid fromPid = (OtpErlangPid) (rtuple.elementAt(0));
//				OtpErlangObject rmsg = rtuple.elementAt(1);
//
//				System.out.println("Message: " + rmsg + " received from:  " + fromPid.toString());
//
//				OtpErlangAtom ok = new OtpErlangAtom("stop");
//				mbox.send(fromPid, ok);
//				break;
//
//			} catch (OtpErlangExit e) {
//				e.printStackTrace();
//				break;
//			} catch (OtpErlangDecodeException e) {
//				e.printStackTrace();
//			}
	}
}

