package javaErlang;

public class JavaObjectKey {
    long key;
    long counter;
    int nodeId;

    public JavaObjectEntryKey(long key, long counter, int nodeId) {
	this.counter = counter;
	this.key = key;
	this.nodeId = nodeId;
    }

    @Override
    public boolean equals(final Object object) {
	if (object instanceof JavaObjectKey) {
	    JavaObjectKey key = (JavaObjectKey) object;
	    return key.key==key && key.counter==counter && key.nodeId==nodeId;
	}
	return false;
    }
}
