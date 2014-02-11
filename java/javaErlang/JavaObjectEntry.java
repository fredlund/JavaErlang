package javaErlang;

import com.ericsson.otp.erlang.OtpErlangObject;

public class JavaObjectEntry {
    long counter;
    long freeKeys;
    long key;
    Object object;
    OtpErlangObject nodeId;

    public JavaObjectEntry(Object object, long key, OtpErlangObject nodeId) {
	this.object = object;
	this.counter = 0;
	this.freeKeys = 0;
	this.key = key;
	this.nodeId = nodeId;
    }

    long alias() {
	long oldCounter = counter++;
	freeKeys++;
	return oldCounter;
    }

    long key() {
	return key;
    }

    long references() {
	return freeKeys;
    }

    OtpErlangObject nodeId() {
	return nodeId;
    }

    Object object() {
	return object;
    }

    long free() {
	return --freeKeys;
    }
}
