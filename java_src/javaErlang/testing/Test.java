package javaErlang.testing;


public class Test {
    public int v = 0;

    public Test() {
    }

    public void print() {
        System.out.println("attribute v in object " + this + " has value " + v);
    }

    int value() {
	return 0;
    }
}
