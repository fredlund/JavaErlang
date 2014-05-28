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

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Logger;
import java.util.logging.Level;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

class ThreadMsgHandler implements Runnable {
    BlockingQueue<OtpErlangObject> queue;
    private final JavaErlang root;

    ThreadMsgHandler(final JavaErlang root) {
        this.root = root;
        queue = new LinkedBlockingQueue<OtpErlangObject>();
    }

    public static ThreadMsgHandler createThreadMsgHandler(final JavaErlang root) {
        final ThreadMsgHandler th = new ThreadMsgHandler(root);
        new Thread(th).start();
        return th;
    }

    public void run() {
	if (JavaErlang.logger.isLoggable(Level.FINE)) {
	    JavaErlang.logger.log(Level.FINE,"\r\n*** starting thread");
	}

        try {
            do_receive();
        } catch (final Exception exc) {
            exc.printStackTrace();
        }

	if (JavaErlang.logger.isLoggable(Level.FINE)) {
	    JavaErlang.logger.log(Level.FINE,"\r\n*** stopping thread");
	}
    }

    void do_receive() throws Exception {
        Short tag;
        OtpErlangPid replyPid;
        OtpErlangObject argument;
	boolean finishing = false;

        do {
            final OtpErlangTuple t = (OtpErlangTuple) queue.take();
            if (JavaErlang.logger.isLoggable(Level.FINER)) {
                JavaErlang.logger.log(Level.FINER,this + " got " + t);
            }
            try {
                tag = ((OtpErlangLong) t.elementAt(0)).uShortValue();
                argument = t.elementAt(2);
                Object result;
                try {
                    result = handleCall(tag, argument);
                } catch (final InvocationTargetException e) {
                    final Throwable te = e.getCause();
                    if (te != null) {
                        result = te;
                    } else {
                        result = e;
                    }
		    if (JavaErlang.logger.isLoggable(Level.WARNING)) {
			JavaErlang.logger.log(Level.WARNING,"\r\n*** Exception " + result + " thrown");
			JavaErlang.logger.log(Level.WARNING,"\r");
			((Throwable) result).printStackTrace();
		    }
                } catch (final Throwable e) {
		    if (JavaErlang.logger.isLoggable(Level.WARNING)) {
			JavaErlang.logger.log(Level.WARNING,"\r\n*** Exception " + e + " thrown");
			JavaErlang.logger.log(Level.WARNING,"\r");
			e.printStackTrace();
		    }
                    result = e;
                }
                if (result != null) {
		    replyPid = (OtpErlangPid) t.elementAt(3);
                    root.reply(result, replyPid);
                } else {
                    finishing = true;
                }
            } catch (final Exception e) {
                System.err.println("Malformed message " + t);
            }
        } while (!finishing);
    }

    Object handleCall(final short tag, final OtpErlangObject argument)
            throws Exception {
	switch (tag) {
	case Tags.call_constructorTag:
            return call_constructor(argument);
	case Tags.call_methodTag:
            return call_method(argument);
	case Tags.getFieldValueTag:
            return getFieldValue(argument);
	case Tags.setFieldValueTag:
            return setFieldValue(argument);
	case Tags.getClassNameTag:
            return getClassName(argument);
	case Tags.array_to_listTag:
            return array_to_list(argument);
	case Tags.list_to_arrayTag:
            return list_to_array(argument);
	case Tags.instofTag:
            return instof(argument);
	case Tags.convertTag:
            return convert(argument);
	case Tags.is_subtypeTag:
            return is_subtype(argument);
	case Tags.getSimpleClassNameTag:
            return getSimpleClassName(argument);
	case Tags.stopThreadTag:
            return null;
        default:
            System.err.println("\rBad tag " + tag + " in received message");
            throw new Exception();
        }
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject array_to_list(final OtpErlangObject value) throws Exception {
        final Object objs = root.java_value_from_erlang(value);
        final Class cl = objs.getClass();
        final Class arrElement = root.getArrayElementClass(cl);
        final int len = Array.getLength(objs);
        final OtpErlangObject objects[] = new OtpErlangObject[len];
        for (int i = 0; i < len; i++) {
            objects[i] = root.map_to_erlang(objs, i, arrElement);
        }
        return new OtpErlangList(objects);
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject list_to_array(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final OtpErlangObject type = t.elementAt(0);
        final OtpErlangObject values = t.elementAt(1);
        final OtpErlangObject[] objs = ((OtpErlangTuple) values).elements();
        final Type element_type = root.fromErlType(type);
        if (element_type instanceof Class) {
            final Class cl = (Class) element_type;
            final Object arr = Array.newInstance(cl, objs.length);
            for (int i = 0; i < objs.length; i++) {
                Array.set(arr, i,
                        root.java_value_from_erlang(objs[i], element_type));
            }
            return root.map_to_erlang(arr);
        } else {
            System.err.println("Cannot convert type description "
                    + element_type + " to a type class");
            throw new Exception();
        }
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject instof(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object obj = root.java_value_from_erlang(t.elementAt(0));
        final String className = ((OtpErlangAtom) t.elementAt(1)).atomValue();
        final Class cl = root.findClass(className);
        return new OtpErlangBoolean(cl.isInstance(obj));
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject convert(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String type = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final OtpErlangObject arg = t.elementAt(1);
        Object result;
        Class resultClass;

        if (arg instanceof OtpErlangLong) {
            final long l = ((OtpErlangLong) arg).longValue();
            if (type.equals("int")) {
                result = (int) l;
                resultClass = Integer.TYPE;
            } else if (type.equals("long")) {
                result = (long) l;
                resultClass = Long.TYPE;
            } else if (type.equals("short")) {
                result = (short) l;
                resultClass = Short.TYPE;
            } else if (type.equals("char")) {
                result = (char) l;
                resultClass = Character.TYPE;
            } else if (type.equals("byte")) {
                result = (byte) l;
                resultClass = Byte.TYPE;
            } else if (type.equals("float")) {
                result = (float) l;
                resultClass = Float.TYPE;
            } else if (type.equals("double")) {
                result = (double) l;
                resultClass = Double.TYPE;
            } else {
                result = l;
                resultClass = Long.TYPE;
            }
        } else {
            final double d = ((OtpErlangDouble) arg).doubleValue();
            if (type.equals("int")) {
                result = (int) d;
                resultClass = Integer.TYPE;
            } else if (type.equals("long")) {
                result = (long) d;
                resultClass = Long.TYPE;
            } else if (type.equals("short")) {
                result = (short) d;
                resultClass = Short.TYPE;
            } else if (type.equals("char")) {
                result = (char) d;
                resultClass = Character.TYPE;
            } else if (type.equals("byte")) {
                result = (byte) d;
                resultClass = Byte.TYPE;
            } else if (type.equals("float")) {
                result = (float) d;
                resultClass = Float.TYPE;
            } else if (type.equals("double")) {
                result = (double) d;
                resultClass = Double.TYPE;
            } else {
                result = d;
                resultClass = Long.TYPE;
            }
        }
        return root.map_to_erlang(result, resultClass);
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    OtpErlangObject is_subtype(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className1 = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final String className2 = ((OtpErlangAtom) t.elementAt(1)).atomValue();
        final Class cl1 = root.findClass(className1);
        final Class cl2 = root.findClass(className2);
        return new OtpErlangBoolean(cl2.isAssignableFrom(cl1));
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject getClassName(final OtpErlangObject cmd) throws Exception {
        final Object obj = root.java_value_from_erlang(cmd);
        final Class cl = obj.getClass();
        return new OtpErlangAtom(cl.getName());
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject getSimpleClassName(final OtpErlangObject cmd)
            throws Exception {
        final Object obj = root.java_value_from_erlang(cmd);
        final Class cl = obj.getClass();
        return new OtpErlangAtom(cl.getSimpleName());
    }

    OtpErlangObject getFieldValue(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object obj = root.java_value_from_erlang(t.elementAt(0));
        final Field field = root.get_field(t.elementAt(1));
        final Object result = field.get(obj);
        return root.map_to_erlang(result, field.getType());
    }

    OtpErlangObject setFieldValue(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object obj = root.java_value_from_erlang(t.elementAt(0));
        final Field field = root.get_field(t.elementAt(1));
        final OtpErlangObject value = t.elementAt(2);
	Object setValue = root.java_value_from_erlang(value, field.getType());
        field.set(obj, setValue);
        return root.map_to_erlang_void();
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject call_constructor(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object fun = root.get_fun(t.elementAt(0));
        final OtpErlangObject[] args =
	    ((OtpErlangTuple) t.elementAt(1)).elements();
        Object result;

        final Constructor cnstr = (Constructor) fun;
	cnstr.setAccessible(true);
        result =
	    cnstr.newInstance
	    (root.java_values_from_erlang
	     (args,cnstr.getParameterTypes()));
        return root.map_to_erlang(result);
    }

    OtpErlangObject call_method(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object fun = root.get_fun(t.elementAt(1));
        final OtpErlangObject[] args = ((OtpErlangTuple) t.elementAt(2))
                .elements();
        Object result;

        final Method method = (Method) fun;
	method.setAccessible(true);
        final OtpErlangObject otpObj = t.elementAt(0);
        final Object obj = root.java_value_from_erlang(otpObj);
        final Object[] translated_args = root.java_values_from_erlang(args,
                method.getParameterTypes());
        result = method.invoke(obj, translated_args);
        return root.map_to_erlang(result, method.getReturnType());
    }
}