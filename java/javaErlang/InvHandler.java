// Copyright (c) 2011, Lars-Ake Fredlund
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     // Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     // Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     // Neither the name of the copyright holders nor the
//       names of its contributors may be used to endorse or promote products
//       derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
// BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
// BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// @author Lars-Ake Fredlund (lfredlund@fi.upm.es)
// @copyright 2011 Lars-Ake Fredlund
// 

package javaErlang;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

class InvHandler implements InvocationHandler {
    OtpErlangPid pid;
    Method hashCode;
    IntKey key;
    Object backingObject;
    volatile Object answer;
    private final JavaErlang root;

    public InvHandler(final JavaErlang root, final OtpErlangPid pid,
            final Object backingObject) {
        this.root = root;
        this.pid = pid;
        this.backingObject = backingObject;
        key = new IntKey(root.objCounter++);
        try {
            hashCode = Object.class.getMethod("hashCode");
        } catch (final Exception exc) {
            System.err.println("could not find hashCode");
        }
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

    public Object invoke(final Object proxy, final Method method,
            final Object args[]) throws Throwable {
        OtpErlangObject otpArg;

        if (method.equals(hashCode)) {
            return key.hashCode();
        } else if (JavaErlang.verbose) {
            System.err.println("\nGot invocation with method=" + method + "\n");
        }

        if (args == null) {
            otpArg = root.map_to_erlang_null();
        } else {
            final OtpErlangObject[] otpArgs = new OtpErlangObject[args.length];
            for (int i = 0; i < args.length; i++) {
                otpArgs[i] = root.map_to_erlang(args[i]);
            }
            otpArg = new OtpErlangTuple(otpArgs);
        }

        final OtpErlangObject proxy_msg = root.makeErlangTuple(
                new OtpErlangAtom("proxy_msg"), root.map_to_erlang(proxy),
                root.acc_map_to_erlang(method), otpArg);
        if (JavaErlang.verbose) {
            System.err.println("Sending proxy reply " + proxy_msg + " to "
                    + pid);
        }
        root.msgs.send(pid, proxy_msg);

        final OtpErlangObject myAnswer = (OtpErlangObject) waitForAnswer();
        if (myAnswer instanceof OtpErlangAtom) {
            final String reply = ((OtpErlangAtom) myAnswer).atomValue();
            if (reply.equals("passbuck")) {
                if (backingObject != null) {
                    return method.invoke(backingObject, args);
                } else {
                    System.err
                            .println("Erlang passes the buck without backing object");
                    System.err.println("Method is " + method);
                    return null;
                }
            }
        }
        return root.java_value_from_erlang(myAnswer);
    }
}