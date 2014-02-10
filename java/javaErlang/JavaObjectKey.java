package javaErlang;

import com.ericsson.otp.erlang.OtpErlangObject;

public class JavaObjectKey {
    long key;
    OtpErlangObject nodeId;

    public JavaObjectKey(long key, OtpErlangObject nodeId) {
	this.key = key;
	this.nodeId = nodeId;
    }

    public long key() {
	return key;
    }

    public OtpErlangObject nodeId() {
	return nodeId;
    }

    @Override
    public boolean equals(final Object object) {
	if (object instanceof JavaObjectKey) {
	    JavaObjectKey key = (JavaObjectKey) object;
	    return key.key==this.key && key.nodeId==this.nodeId;
	}
	return false;
    }

    public int hashCode() {
	return (int) key;
    }
}
