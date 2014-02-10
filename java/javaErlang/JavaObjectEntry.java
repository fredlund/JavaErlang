package javaErlang;

public class JavaObjectEntry {
    long counter;
    long freeKeys;
    long key;

    public JavaObjectEntry(long key, int nodeId) {
	this.counter = 0;
	this.freeKeys = 0;
	this.key = key;
    }

    long alias() {
	long oldCounter = counter++;
	freeKeys++;
	return oldCounter;
    }

    long free() {
	return --freeKeys;
    }
}
