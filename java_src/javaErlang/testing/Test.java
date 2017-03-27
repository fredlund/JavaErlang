package javaErlang.testing;


public class Test {
    public int v = 2;
    private int x = 1;

    public Test() { }

    public void print() {
        System.out.println("attribute v in object " + this + " has value " + v);
    }

    int value() {
	return 0;
    }
}
