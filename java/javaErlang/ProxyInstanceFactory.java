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

import java.lang.reflect.Method;
import java.util.logging.Level;

import javassist.util.proxy.ProxyFactory;
import javassist.util.proxy.MethodFilter;
import javassist.util.proxy.MethodHandler;
import javassist.util.proxy.Proxy;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ProxyInstanceFactory {
    Class cl;
    JavaErlang root;
    Method mths[];

    public ProxyInstanceFactory(final JavaErlang root, final Class cl, OtpErlangObject methods) {
	this.root = root;
	this.cl = cl;

	OtpErlangList methodList = (OtpErlangList) methods;
	this.mths = new Method[methodList.arity()];
	for (int i=0; i<methodList.arity(); i++) {
	    OtpErlangTuple t = (OtpErlangTuple) methodList.elementAt(i);
	    final String methodName =
		((OtpErlangAtom) t.elementAt(0)).atomValue();
	    final OtpErlangList typeList =
		(OtpErlangList) t.elementAt(1);
	    try {
		final Method method =
		    root.getMethod
		    (cl.getSuperclass(), 
		     methodName,
		     typeList.elements());
		mths[i] = method;
	    } catch (Exception exc) {
		if (root.logger.isLoggable(Level.WARNING)) 
		    root.logger.log
			(Level.WARNING,"Method "+methodList.elementAt(i)+" not found");
		mths[i] = null;
	    }
	}
    }

    public OtpErlangObject newInstance(final int objectId, final OtpErlangPid pid) throws InstantiationException, IllegalAccessException {

	MethodHandler mh = new ProxyHandler(root, objectId, pid, mths);
	Object obj = cl.newInstance();
	((Proxy) obj).setHandler(mh);
	return
	    root.makeErlangTuple
	    (root.map_to_erlang(obj),
	     root.map_to_erlang(mh));
    }
}