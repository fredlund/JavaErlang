package javaErlang;

import java.lang.reflect.Method;

import javassist.util.proxy.MethodHandler;
import javassist.util.proxy.Proxy;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;

public class ProxyHandler implements MethodHandler {
    int objectId;
    OtpErlangPid pid;
    Object answer;
    JavaErlang root;
    Method methods[];

    public ProxyHandler(JavaErlang root, int objectId, OtpErlangPid pid, Method[] methods) {
	this.root = root;
	this.objectId = objectId;
	this.pid = pid;
	this.methods = methods;
    }

    public Object invoke(Object self,
			 Method m,
			 Method proceed,
			 Object[] args) throws Throwable {
	System.out.println("method "+m+"invoked, pid="+pid);
	System.out.println("methods_length="+methods.length);
	int index = -1;
	for (int i=0; i<methods.length; i++) {
	    System.out.println("checking "+methods[i]+" against "+m);
	    if (methods[i].equals(m)) {
		index=i;
		break;
	    }
	}
	System.out.println("index is "+index);
	OtpErlangObject msg = 
	     root.makeErlangTuple
	     (new OtpErlangAtom("proxy_invoke"),
	      new OtpErlangLong(objectId),
	      root.map_to_erlang(self),
	      root.map_to_erlang(m),
	      new OtpErlangLong(index+1),
	      root.map_to_erlang(args));
        root.msgs.send(pid,msg);
	return null;
    }

    public synchronized Object waitForAnswer() {
	try {
	    this.wait();
	} catch (final InterruptedException exc) {
	    return waitForAnswer();
	}
	return answer;
    }
    
    public synchronized void setAnswer(final Object answer) {
	this.answer = answer;
	notifyAll();
    }
}