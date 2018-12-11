package game;

import java.io.IOException;
import com.ericsson.otp.erlang.*;

public class EntryPoint {

	public static void main(String[] args) {
		try {
			
			OtpNode self = new OtpNode("java", "cookie");
			OtpMbox mbox = self.createMbox("echo");
			OtpErlangObject o;
			OtpErlangTuple msg;
			OtpErlangPid from;
			System.out.println("C'est bon");

			while (true) {
				try {
					
					o = mbox.receive();
					if (o instanceof OtpErlangTuple) {
						System.out.println("message reçu");
						msg = (OtpErlangTuple) o;
						from = (OtpErlangPid) (msg.elementAt(0));
						mbox.send(from, msg.elementAt(1));
					}
					
				} catch (Exception e) {
					System.out.println("" + e);
				}
			}
			
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
