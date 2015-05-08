package javaErlang;

import java.lang.reflect.Method;
import java.util.logging.Level;

import javassist.util.proxy.MethodHandler;
import javassist.util.proxy.Proxy;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangList;

public class ProxyHandler implements MethodHandler {
    int objectId;
    OtpErlangPid pid;
    OtpErlangObject answer;
    JavaErlang root;
    Method methods[];
    Method method = null;

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

        if (root.logger.isLoggable(Level.FINE))
            root.logger.log
                (Level.FINE,
                 "proxy method "+m+"invoked, pid="+pid+"methods_length="+methods.length);

        int index = -1;
        for (int i=0; i<methods.length; i++) {
            if (root.logger.isLoggable(Level.FINE))
                root.logger.log(Level.FINE,"checking "+methods[i]+" against "+m);
            if (methods[i].equals(m)) {
                index=i;
                break;
            }
        }
        if (root.logger.isLoggable(Level.FINE))
            root.logger.log(Level.FINE,"index is "+index);

        OtpErlangObject elements[] =
            new OtpErlangObject[args.length];
        for (int i=0; i<args.length; i++)
            elements[i] = root.map_to_erlang(args[i]);

        OtpErlangObject msg =
            root.makeErlangTuple
            (new OtpErlangAtom("proxy_invoke"),
             root.makeErlangTuple
             (new OtpErlangLong(objectId),
              root.map_to_erlang(self),
              root.map_to_erlang(m),
              new OtpErlangLong(index+1),
              new OtpErlangList(elements)));
        root.msgs.send(pid,msg);
        method = m;
        return waitForAnswer();
    }

    public synchronized Object waitForAnswer() throws Exception {
        try {
            this.wait();
        } catch (final InterruptedException exc) {
            return waitForAnswer();
        }
        return root.java_value_from_erlang(answer,method.getReturnType());
    }

    public synchronized void setAnswer(final OtpErlangObject answer) {
        this.answer = answer;
        notifyAll();
    }
}
