package javaErlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangRangeException;

// The representation of a Java reference as an Erlang term --
// such terms are communicated to/from Erlang
public class ErlangObjectRef {
    OtpErlangObject ref;

    public ErlangObjectRef(long ref, long counter, int nodeId) {
        this.ref =
            JavaErlang.makeErlangTuple
            (new OtpErlangAtom("object"),
             new OtpErlangLong(ref),
             new OtpErlangLong(counter),
             new OtpErlangInt(nodeId));
    }

    public ErlangObjectRef(OtpErlangObject obj) {
        final OtpErlangTuple t = (OtpErlangTuple) obj;
        final OtpErlangString tag = (OtpErlangString) t.elementAt(0);
        if (t.arity() != 4 || !tag.stringValue().equals("object"))
            throw new Error();
        this.ref = obj;
    }

    public long ref() {
        final OtpErlangTuple t = (OtpErlangTuple) ref;
        return ((OtpErlangLong) t.elementAt(1)).longValue();
    }

    public long counter() {
        final OtpErlangTuple t = (OtpErlangTuple) ref;
        return ((OtpErlangLong) t.elementAt(2)).longValue();
    }

    public int nodeId() {
        final OtpErlangTuple t = (OtpErlangTuple) ref;
        try { return ((OtpErlangInt) t.elementAt(3)).intValue(); }
        catch (OtpErlangRangeException e) { throw new Error(); }
    }

    public boolean isObjectRef(OtpErlangObject obj) {
        if (obj instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) obj;
            if (t.arity() == 4 && t.elementAt(0) instanceof OtpErlangString) {
                final OtpErlangString tag = (OtpErlangString) t.elementAt(0);
                return tag.stringValue().equals("object");
            }
        }
        return false;
    }

    public String tag(OtpErlangObject obj) {
        final OtpErlangTuple t = (OtpErlangTuple) obj;
        return ((OtpErlangString) t.elementAt(0)).toString();
    }

    public OtpErlangObject value() { return ref; }
}
