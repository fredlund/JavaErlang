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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

class ProxyFactoryClass {
    public static ProxyInstanceFactory newClass(JavaErlang root, final Class superClass, final OtpErlangObject methods) {
	ProxyFactory f = new ProxyFactory();
	f.setSuperclass(superClass);

	OtpErlangList methodsList = (OtpErlangList) methods;
	final Method m[] = new Method[methodsList.arity()];
	for (int i=0; i<methodsList.arity(); i++) {
	    OtpErlangTuple t = (OtpErlangTuple) methodsList.elementAt(i);
	    final String methodName = ((OtpErlangAtom) t.elementAt(0)).atomValue();
	    final OtpErlangList typeList = (OtpErlangList) t.elementAt(1);
	    try {
		final Method method =
		    JavaErlang.getMethod
		    (superClass, 
		     methodName,
		     typeList.elements());
		m[i] = method;
	    } catch (Exception exc) { m[i] = null; }
	}
	f.setFilter
	    (new MethodFilter() {
		    public boolean isHandled(Method mth) {
			for (int i=0; i<m.length; i++) 
			    if (mth.equals(m[i])) return true;
			return false;
		    }
		});
	Class cl = f.createClass();
	ProxyInstanceFactory pif = new ProxyInstanceFactory(root, cl, methods);
	return pif;
    }
}