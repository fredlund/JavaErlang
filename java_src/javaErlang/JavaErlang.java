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

import java.io.File;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.net.URL;
import java.security.CodeSource;
import java.security.ProtectionDomain;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.logging.ConsoleHandler;

import javassist.util.proxy.ProxyFactory;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangByte;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangShort;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

@SuppressWarnings("rawtypes")
public class JavaErlang {
    volatile private Map<RefEqualsObject, JavaObjectEntry> toErlangMap;
    volatile private Map<JavaObjectKey, JavaObjectEntry> fromErlangMap;
    volatile private Map<Object, OtpErlangObject> accToErlangMap;
    volatile private Map<OtpErlangObject, Object> accFromErlangMap;
    volatile private Map<OtpErlangObject, ThreadMsgHandler> threadMap;
    volatile private Map<Class,Integer> classMap;
    volatile int objCounter = 0;
    volatile int classCounter = 0;
    volatile private int accCounter = 0;
    volatile private int threadCounter = 0;

    volatile OtpMbox msgs;
    volatile OtpErlangObject nodeIdentifier = null;
    static volatile Logger logger = Logger.getLogger("JavaErlangLogger");

    boolean isConnected = false;

    public static void main(final String args[]) {
	Level logLevel = Level.WARNING;
	int currentArg = 0;
        final String name = args[currentArg++];
	String cookie = null;

	while (currentArg < args.length) {
	    final String arg = args[currentArg++];

	    if (arg.equals("-loglevel"))
		if (currentArg < args.length)
		    logLevel = Level.parse(args[currentArg++]);
		else {
		    System.err.println("Missing argument for -loglevel option");
		    System.exit(-1);
		}
	    else if (arg.equals("-setcookie"))
		if (currentArg < args.length)
		    cookie = args[currentArg++];
		else {
		    System.err.println("Missing argument for -setcookie option");
		    System.exit(-1);
		}
	    else {
                System.err.println("\rCannot understand argument " + arg);
                System.exit(-1);
	    }
	}

        logger.setLevel(logLevel);

        ConsoleHandler consoleHandler = new ConsoleHandler();
        consoleHandler.setLevel(logger.getLevel());
        logger.addHandler(consoleHandler);

        try {
            new JavaErlang(name,cookie).do_receive();
        } catch (final Exception e) {
            logger.log
                (Level.SEVERE,
                 "*** Unexpected exception failure in JavaErlang: "
                 + e,e);
        }

    }

    public JavaErlang(final String name, final String cookie) {
        toErlangMap = new ConcurrentHashMap<RefEqualsObject, JavaObjectEntry>();
        fromErlangMap = new ConcurrentHashMap<JavaObjectKey, JavaObjectEntry>();
        accToErlangMap = new ConcurrentHashMap<Object, OtpErlangObject>();
        accFromErlangMap = new ConcurrentHashMap<OtpErlangObject, Object>();
        classMap = new ConcurrentHashMap<Class, Integer>();
        threadMap = new ConcurrentHashMap<OtpErlangObject, ThreadMsgHandler>();
        try {
            final OtpNode node;

	    if (cookie == null) node = new OtpNode(name);
	    else node = new OtpNode(name,cookie);

            node.registerStatusHandler(new OtpStatusHandler(nodeIdentifier));
            if (logger.isLoggable(Level.INFO)) {
                logger.log(Level.INFO,"\rRegistered host " + node.node());
            }
            msgs = node.createMbox("javaNode");
        } catch (final Throwable e) {
            if (logger.isLoggable(Level.SEVERE))
                logger.log
                    (Level.SEVERE,
                     "*** Unexpected exception failure in JavaErlang: "
                     + e,
                     e);
        }
    }

    void do_receive() throws Exception {
        do {
            final OtpErlangObject msg = msgs.receive();
            if (logger.isLoggable(Level.FINER)) {
                logger.log(Level.FINER,"\rGot message " + msg);
            }

            if (msg instanceof OtpErlangTuple) {
                final OtpErlangTuple t = (OtpErlangTuple) msg;
                if (t.arity() == 3 && t.elementAt(0) instanceof OtpErlangLong
                    && t.elementAt(2) instanceof OtpErlangPid) {
                    handle_nonthread_msg(t);
                } else if (t.arity() == 4
                           && t.elementAt(0) instanceof OtpErlangLong
                           && t.elementAt(3) instanceof OtpErlangPid) {
                    handle_thread_msg(t);
                } else if (t.arity() == 2
                           && t.elementAt(0) instanceof OtpErlangLong) {
                    handle_noncall_msg(t);
                } else {
		    if (logger.isLoggable(Level.FINER))
			logger.log(Level.FINER,"\nMalformed message " + msg
				   + " received");
                }
            } else {
		if (logger.isLoggable(Level.FINER))
		    logger.log(Level.FINER,"\nMalformed message " + msg + " received");
            }
        } while (true);
    }

    public void reply(final Object reply, final OtpErlangPid replyPid)
        throws Exception {
        // logger.log(Level.FINER,"returning "+return_value(reply)+" to "+replyPid);
        msgs.send(replyPid, return_value(reply));
    }

    void handle_nonthread_msg(final OtpErlangTuple t) throws Exception {
        final short tag = ((OtpErlangLong) t.elementAt(0)).uShortValue();
        final OtpErlangObject argument = t.elementAt(1);
        final OtpErlangPid replyPid = (OtpErlangPid) t.elementAt(2);
        if (!isConnected) {
            if (tag == Tags.connectTag) {
                final String nameOfRunningVM = ManagementFactory
                    .getRuntimeMXBean().getName();
                final int p = nameOfRunningVM.indexOf('@');
                final String pid = nameOfRunningVM.substring(0, p);
                final Integer intPid = new Integer(pid);
                if (nodeIdentifier == null) {
                    nodeIdentifier = argument;
                }
                reply(makeErlangTuple(new OtpErlangAtom("connected"),
                                      msgs.self(), new OtpErlangLong(intPid)), replyPid);
                isConnected = true;
            } else {
		if (logger.isLoggable(Level.FINER))
		    logger.log(Level.FINER,"\nFirst message should be connect " + t);
            }
        } else {
            Object result;
            try {
                result = handleNonThreadMsg(tag, argument, replyPid);
            } catch (final Throwable e) {
                if (logger.isLoggable(Level.WARNING)) {
                    logger.log(Level.WARNING,"\r\n*** Exception " + e + " thrown\r");
                    e.printStackTrace();
                }
                result = e;
            }
            reply(result, replyPid);
        }
    }

    void handle_noncall_msg(final OtpErlangTuple t) throws Exception {
        final short tag = ((OtpErlangLong) t.elementAt(0)).uShortValue();
        final OtpErlangObject argument = t.elementAt(1);
        try {
            handleNonCallMsg(tag, argument);
        } catch (final Throwable e) {
            if (logger.isLoggable(Level.WARNING)) {
                logger.log(Level.WARNING,"\r\n*** Exception " + e + " thrown\r");
                e.printStackTrace();
            }
        }
    }

    void handle_thread_msg(final OtpErlangTuple t) throws Exception {
        final Object map_result = threadMap.get(t.elementAt(1));
        if (map_result instanceof ThreadMsgHandler) {
            final ThreadMsgHandler th = (ThreadMsgHandler) map_result;
            th.queue.put(t);
        } else {
	    if (logger.isLoggable(Level.FINER))
		logger.log(Level.FINER,"Thread " + t.elementAt(1) + " not found");
        }
    }

    OtpErlangObject handleNonThreadMsg(final short tag,
                                       final OtpErlangObject argument, final OtpErlangPid replyPid)
        throws Exception {
        switch (tag) {
        case Tags.resetTag:
            objCounter = 0;
            toErlangMap = new ConcurrentHashMap<RefEqualsObject, JavaObjectEntry>();
            fromErlangMap = new ConcurrentHashMap<JavaObjectKey, JavaObjectEntry>();
            for (final ThreadMsgHandler th : threadMap.values()) {
                stop_thread(th);
            }
            threadMap = new ConcurrentHashMap<OtpErlangObject, ThreadMsgHandler>();
            System.gc();
            return new OtpErlangAtom("ok");
        case Tags.terminateTag:
            if (logger.isLoggable(Level.FINER)) {
                logger.log(Level.FINER,"\r\nterminating java...");
            }
            reply(new OtpErlangAtom("ok"), replyPid);
            System.exit(0);
            return map_to_erlang_void();
        case Tags.connectTag:
            return new OtpErlangAtom("already_connected");
        case Tags.lookupClassTag:
            return lookupClass(argument);
        case Tags.getConstructorsTag:
            return getConstructors(argument);
        case Tags.getMethodsTag:
            return getMethods(argument);
        case Tags.getClassesTag:
            return getClasses(argument);
        case Tags.getFieldsTag:
            return getFields(argument);
        case Tags.getConstructorTag:
            return getConstructor(argument);
        case Tags.getMethodTag:
            return getMethod(argument);
        case Tags.getFieldTag:
            return getField(argument);
        case Tags.objTypeCompatTag:
            return objTypeCompat(argument);
        case Tags.freeTag:
            return free(argument);
        case Tags.freeInstanceTag:
            return freeInstance(argument);
        case Tags.memoryUsageTag:
            return memoryUsage(argument);
        case Tags.identityTag:
            return identity(argument);
        case Tags.createThreadTag:
            return create_thread();
        case Tags.new_proxy_classTag:
            return new_proxy_class(argument);
        case Tags.new_proxy_objectTag:
            return new_proxy_object(argument);
        case Tags.proxy_replyTag:
            return proxy_reply(argument);
        default:
            logger.log
                (Level.SEVERE,
                 "*** Error: JavaErlang: \nTag " + tag
                 + " not recognized");
            throw new Exception();
        }
    }

    OtpErlangObject handleNonCallMsg(final short tag,
                                     final OtpErlangObject argument)
        throws Exception {
        switch (tag) {
        case Tags.freeInstanceTag:
            return freeInstance(argument);
        case Tags.stopThreadTag:
            final Object map_result = threadMap.get(argument);
            if (map_result instanceof ThreadMsgHandler) {
                final ThreadMsgHandler th = (ThreadMsgHandler) map_result;
                threadMap.remove(argument);
                stop_thread(th);
            } else {
		if (logger.isLoggable(Level.FINER))
		    logger.log
			(Level.FINER,
			 "*** Warning: thread missing in stopThread");
                //throw new Exception();
            }
            return map_to_erlang_void();
        default:
            logger.log
                (Level.SEVERE,
                 "*** Error: JavaErlang: \nTag " + tag
                 + " not recognized");
            throw new Exception();
        }
    }

    OtpErlangObject create_thread() {
        final ThreadMsgHandler th = ThreadMsgHandler
            .createThreadMsgHandler(this);
        return map_new_thread_to_erlang(th);
    }

    static void stop_thread(final ThreadMsgHandler th) throws Exception {
        th.queue.put(mkStopThreadMsg());
    }

    static OtpErlangObject mkStopThreadMsg() {
        OtpErlangObject nullObj = map_to_erlang_null();
        return makeErlangTuple(new OtpErlangLong(Tags.stopThreadTag),
                               nullObj, nullObj, nullObj);
    }

    public static OtpErlangTuple makeErlangTuple(
                                                 final OtpErlangObject... arguments) {
        return new OtpErlangTuple(arguments);
    }

    public static OtpErlangObject makeErlangKey(final String tag,
                                                final IntKey key, final OtpErlangObject nodeId) {
        return makeErlangTuple(new OtpErlangAtom(tag),
                               new OtpErlangInt(key.key()), nodeId);
    }

    Object java_value_from_erlang(final OtpErlangObject value) throws Exception {
        if (value instanceof OtpErlangAtom) {
            final String stringValue = ((OtpErlangAtom) value).atomValue();
            if (stringValue.equals("null")) {
                return null;
            }
            if (stringValue.equals("true")) {
                return new Boolean(true);
            }
            if (stringValue.equals("false")) {
                return new Boolean(false);
            }
	    if (logger.isLoggable(Level.WARNING))
		logger.log(Level.WARNING,"java_value_from_erlang: " + value);
            throw new Exception();
        }

        if (value instanceof OtpErlangTuple) {
            OtpErlangTuple t = (OtpErlangTuple) value;
            final int arity = t.arity();

            if (arity == 2) {
                final OtpErlangObject arg = t.elementAt(1);
                if (t.elementAt(0) instanceof OtpErlangAtom) {
                    final String classSpecifier = ((OtpErlangAtom) t
                                                   .elementAt(0)).atomValue();
                    if (classSpecifier.equals("int")) {
                        return convert_to_integer(arg);
                    } else if (classSpecifier.equals("long")) {
                        return convert_to_long(arg);
                    } else if (classSpecifier.equals("short")) {
                        return convert_to_short(arg);
                    } else if (classSpecifier.equals("char")) {
                        return convert_to_character(arg);
                    } else if (classSpecifier.equals("byte")) {
                        return convert_to_byte(arg);
                    } else if (classSpecifier.equals("float")) {
                        return convert_to_float(arg);
                    } else if (classSpecifier.equals("double")) {
                        return convert_to_double(arg);
                    }
                } else {
                    t = (OtpErlangTuple) t.elementAt(0);
                    final String classSpecifier = ((OtpErlangAtom) t
                                                   .elementAt(0)).atomValue();
                    if (classSpecifier.equals("array")) {
                        final Class comp = (Class) fromErlType(t.elementAt(1));
                        final int dimensions = ((OtpErlangLong) t.elementAt(2))
                            .intValue();
                        final int[] arr_dimensions = checkDimensions(
                                                                     dimensions, arg);
                        final Object array = Array.newInstance(comp,
                                                               arr_dimensions);
                        initializeArray(array, arg, comp);
                        return array;
                    }
                }
            } else if (arity == 5) {
                final String tag = ((OtpErlangAtom) t.elementAt(0)).atomValue();
                if (tag.equals("object")) {
                    final JavaObjectKey key = objectKeyFromErlang(t);
                    final JavaObjectEntry entry = fromErlangMap.get(key);
                    final Object result = entry.object();
                    if (result == null) {
                        if (logger.isLoggable(Level.FINE)) {
                            logger.log(Level.FINE,"\rTranslating " + value);
                        }
                        throw new Exception();
                    }
                    return result;
                } else if (tag.equals("executable")) {
                    final Object result = accFromErlangMap.get(t);
                    if (result == null) {
                        if (logger.isLoggable(Level.FINE)) {
                            logger.log(Level.FINE,"\rTranslating " + value);
                        }
                        throw new Exception();
                    }
                    return result;
                }
            }
        }
	if (logger.isLoggable(Level.WARNING))
	    logger.log(Level.WARNING,"java_value_from_erlang: " + value);
        throw new Exception();
    }

    Object[] java_values_from_erlang(final OtpErlangObject[] values,
                                     final Type[] types) throws Exception {
        final Object[] objects = new Object[values.length];
        for (int i = 0; i < values.length; i++) {
            final Object value = java_value_from_erlang(values[i], types[i]);
            objects[i] = value;
        }
        return objects;
    }

    Object java_value_from_erlang(final OtpErlangObject value, final Type type)
        throws Exception {

        if (logger.isLoggable(Level.FINE)) {
            logger.log(Level.FINE,"\rvalue_from_erlang " + value +
                       " type "+type);
        }

        if (value instanceof OtpErlangTuple) {
            return java_value_from_erlang(value);
        }

        if (value instanceof OtpErlangAtom) {
            return java_value_from_erlang(value);
        }

        // We have to use type information to interpret the value
        if (type == java.lang.Integer.TYPE || type == java.lang.Integer.class) {
            return convert_to_integer(value);
        } else if (type == java.lang.Long.TYPE || type == java.lang.Long.class) {
            return convert_to_long(value);
        } else if (type == java.lang.Short.TYPE
                   || type == java.lang.Short.class) {
            return convert_to_short(value);
        } else if (type == java.lang.Character.TYPE
                   || type == java.lang.Character.class) {
            return convert_to_character(value);
        } else if (type == java.lang.Byte.TYPE || type == java.lang.Byte.class) {
            return convert_to_byte(value);
        } else if (type == java.lang.Float.TYPE
                   || type == java.lang.Float.class) {
            return convert_to_float(value);
        } else if (type == java.lang.Double.TYPE
                   || type == java.lang.Double.class) {
            return convert_to_double(value);
        } else if (type == java.lang.Void.TYPE || type == java.lang.Void.class) {
            return convert_to_void(value);
        } else if (value instanceof OtpErlangLong) {
            return convert_to_integer(value);
        } else if (value instanceof OtpErlangDouble) {
            return convert_to_double(value);
        } else if ((value instanceof OtpErlangString) &&
                   (type == java.lang.String.class)) {
            return ((OtpErlangString) value).stringValue();
        } else if ((value instanceof OtpErlangString) &&
                   (type == java.lang.CharSequence.class)) {
            return ((OtpErlangString) value).stringValue();
        } else if ((value instanceof OtpErlangList) &&
                   (type == java.lang.String.class)) {
            return ((OtpErlangList) value).stringValue();
        }
        else {
            if (type instanceof Class) {
                final Class typeClass = (Class) type;
                final int dimensions = dimensions(typeClass);
                if (typeClass.isArray()) {
                    final Class arrElement = getArrayElementClass(typeClass);
                    final int[] lengths = checkDimensions(dimensions, value);
                    final Object arr = Array.newInstance(arrElement, lengths);
                    initializeArray(arr, value, arrElement);
                    return arr;
                }
		if (logger.isLoggable(Level.FINE)) {
		    logger.log
			(Level.FINE,"Cannot convert " + value + " to type "
			 + typeClass + " dimensions = "+dimensions);
		}
		throw new Exception();
            }
            if (logger.isLoggable(Level.FINE)) {
                logger.log
                    (Level.FINE,"Cannot convert " + value + " to type "
                     + type);
            }
            throw new Exception();
        }
    }

    static Object convert_to_character(final OtpErlangObject value)
        throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).charValue();
        }
	if (logger.isLoggable(Level.FINE)) {
	    logger.log(Level.FINE,"\rerror: convert_to_character " + value);
	    logger.log(Level.FINE,"\rtype is " + value.getClass());
	}
        throw new Exception();
    }

    static Object convert_to_byte(final OtpErlangObject value) throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).byteValue();
        }
	if (logger.isLoggable(Level.FINE)) {
	    logger.log(Level.FINE,"\rerror: convert_to_byte " + value);
	    logger.log(Level.FINE,"\rtype is " + value.getClass());
	}
        throw new Exception();
    }

    static Object convert_to_float(final OtpErlangObject value)
        throws Exception {
        if (value instanceof OtpErlangDouble) {
            return ((OtpErlangDouble) value).floatValue();
        }
	if (logger.isLoggable(Level.FINE)) {
	    logger.log(Level.FINE,"\rerror: convert_to_float " + value);
	    logger.log(Level.FINE,"\rtype is " + value.getClass());
	}
        throw new Exception();
    }

    static Object convert_to_double(final OtpErlangObject value)
        throws Exception {
        if (value instanceof OtpErlangDouble) {
            return ((OtpErlangDouble) value).doubleValue();
        }
	if (logger.isLoggable(Level.FINE)) {
	    logger.log(Level.FINE,"\rerror: convert_to_double " + value);
	    logger.log(Level.FINE,"\rtype is " + value.getClass());
	}
        throw new Exception();
    }

    static Object convert_to_void(final OtpErlangObject value) throws Exception {
	if (logger.isLoggable(Level.FINE)) {
	    logger.log(Level.FINE,"\rerror: convert_to_void " + value);
	    logger.log(Level.FINE,"\rtype is " + value.getClass());
	}
        throw new Exception();
    }

    static Object convert_to_short(final OtpErlangObject value)
        throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).shortValue();
        }
	if (logger.isLoggable(Level.FINE)) {
	    logger.log(Level.FINE,"\rerror: convert_to_short " + value);
	    logger.log(Level.FINE,"\rtype is " + value.getClass());
	}
        throw new Exception();
    }

    static Object convert_to_integer(final OtpErlangObject value)
        throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).intValue();
        }
	if (logger.isLoggable(Level.FINE)) {
	    logger.log(Level.FINE,"\rerror: convert_to_integer " + value);
	    logger.log(Level.FINE,"\rtype is " + value.getClass());
	}
        throw new Exception();
    }

    static Object convert_to_long(final OtpErlangObject value) throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).longValue();
        }
	if (logger.isLoggable(Level.FINE)) {
	    logger.log(Level.FINE,"\rerror: convert_to_long " + value);
	    logger.log(Level.FINE,"\rtype is " + value.getClass());
	}
        throw new Exception();
    }

    static OtpErlangObject[] elements(final OtpErlangObject t) {
        if (t instanceof OtpErlangList) {
            return ((OtpErlangList) t).elements();
        }

        // Jinterface braindamage follows...
        final String value = ((OtpErlangString) t).stringValue();
        final byte[] bytes = value.getBytes();
        final OtpErlangObject[] otpBytes = new OtpErlangObject[bytes.length];
        for (int i = 0; i < bytes.length; i++) {
            otpBytes[i] = new OtpErlangLong(bytes[i]);
        }
        return otpBytes;
    }

    void initializeArray(final Object arr, final OtpErlangObject value, Class objClass)
        throws Exception {
        final int len = Array.getLength(arr);
        final OtpErlangObject[] elements = elements(value);
        for (int i = 0; i < len; i++) {
            final OtpErlangObject element = elements[i];
            final Object obj_at_i = Array.get(arr, i);
            if (objClass.isArray()) {
                initializeArray(obj_at_i, element, objClass);
            } else {
                final Class arrElement = getArrayElementClass(objClass);
                final Object setValue = java_value_from_erlang(element,
                                                               arrElement);
                Array.set(arr, i, setValue);
            }
        }
    }

    static Class getArrayElementClass(final Class arrClass) {
        if (arrClass.isArray()) {
            return getArrayElementClass(arrClass.getComponentType());
        } else {
            return arrClass;
        }
    }

    static int[] checkDimensions(int dimensions, final OtpErlangObject value) {
        final ArrayList<Integer> result = new ArrayList<Integer>();
        while (dimensions > 0) {
            final OtpErlangObject[] elements = elements(value);
            result.add(elements.length);
            dimensions--;
        }
        final int[] return_value = new int[result.size()];
        int i = 0;
        for (final Integer ri : result) {
            return_value[i++] = ri;
        }
        return return_value;
    }

    static int dimensions(final Class arrClass) {
        if (arrClass.isArray()) {
            return 1 + dimensions(arrClass.getComponentType());
        } else {
            return 0;
        }
    }

    static Type[] fromErlTypes(final OtpErlangObject[] erlTypes)
        throws Exception {
        final int len = erlTypes.length;
        // logger.log(Level.FINER,"\rfromErlTypes(len="+len+")");
        final Type types[] = new Type[len];
        for (int i = 0; i < len; i++) {
            types[i] = fromErlType(erlTypes[i]);
        }
        return types;
    }

    static Type fromErlType(final OtpErlangObject erlType) throws Exception {
        if (erlType instanceof OtpErlangAtom) {
            final String name = ((OtpErlangAtom) erlType).atomValue();
            if (name.equals("int")) {
                return java.lang.Integer.TYPE;
            } else if (name.equals("short")) {
                return java.lang.Short.TYPE;
            } else if (name.equals("long")) {
                return java.lang.Long.TYPE;
            } else if (name.equals("char")) {
                return java.lang.Character.TYPE;
            } else if (name.equals("boolean")) {
                return java.lang.Boolean.TYPE;
            } else if (name.equals("byte")) {
                return java.lang.Byte.TYPE;
            } else if (name.equals("float")) {
                return java.lang.Float.TYPE;
            } else if (name.equals("double")) {
                return java.lang.Double.TYPE;
            } else if (name.equals("void")) {
                return java.lang.Void.TYPE;
            } else {
                return findClass(name);
            }
        } else if (erlType instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) erlType;
            final OtpErlangAtom a = (OtpErlangAtom) t.elementAt(0);
            if (a.atomValue().equals("array")) {
                final Class comp = (Class) fromErlType(t.elementAt(1));
                int ndimensions = 1;
                if (t.arity() == 3) {
                    ndimensions = ((OtpErlangLong) t.elementAt(2)).intValue();
                }
                final int[] dimensions = new int[ndimensions];
                for (int i = 0; i < ndimensions; i++) {
                    dimensions[i] = 0;
                }
                final Object array = Array.newInstance(comp, dimensions);
                final Class arrayClass = array.getClass();
                return arrayClass;
            }
        }
	if (logger.isLoggable(Level.WARNING)) 
	    logger.log(Level.WARNING,"\rtype " + erlType + " is not understood?");
        throw new Exception();
    }

    Class findClass(final OtpErlangObject classRef) throws Exception {
	if (classRef instanceof OtpErlangAtom) {
	    final String className = ((OtpErlangAtom) classRef).atomValue();
	    return findClass(className);
	} else {
	    Class cl = (Class) java_value_from_erlang(classRef);
	    return cl;
	}
    }

    static Class findClass(final String className) throws Exception {
        try {
            final Class c = Class.forName(className);
            return c;
        } catch (final Exception e) {  }

        final StringBuilder str = new StringBuilder(className);
        do {
            final int lastIndex = str.lastIndexOf(".");
            if (lastIndex == -1) {
		if (logger.isLoggable(Level.WARNING))
		    logger.log
			(Level.WARNING,
			 "findClass: cannot locate class " + str +
			 " using classpath\n"+System.getProperty("java.class.path")+
                     "\nworking directory is "+System.getProperty("user.dir"));
                if (logger.isLoggable(Level.WARNING)) {
                    logger.log(Level.WARNING,"findClass: cannot locate class " + str);
                }
                throw new Exception();
            }
            str.replace(lastIndex, lastIndex + 1, "$");
            try {
                final Class c = Class.forName(str.toString());
                return c;
            } catch (final Exception exc) { }

        } while (true);
    }

    public OtpErlangObject makeErlangObjectKey(final long key, final long counter, final long classNumber, final OtpErlangObject nodeId) {
        return makeErlangTuple
            (new OtpErlangAtom("object"),
             new OtpErlangLong(key),
             new OtpErlangLong(counter),
             new OtpErlangLong(classNumber),
             nodeId);
    }

    public synchronized OtpErlangObject map_to_erlang(final Object obj) {
        if (obj == null) {
            return map_to_erlang_null();
        }

        final RefEqualsObject obj_key =
            new RefEqualsObject(obj);
        final JavaObjectEntry oldValue =
            toErlangMap.get(obj_key);

        if (oldValue != null) {
            long refCount = oldValue.alias();
            if (logger.isLoggable(Level.INFO))
                logger.log
                    (Level.INFO,
                     "increasing count for "+
                     System.identityHashCode(oldValue.object()));
            return makeErlangObjectKey
                (oldValue.key(),refCount,oldValue.classNumber(), oldValue.nodeId());
        }

        final int newCounter = objCounter++;
        Class cl = obj.getClass();
        Integer classNumber = classMap.get(cl);
        if (classNumber == null) {
            classNumber = classCounter;
            classMap.put(cl,classCounter++);
        }
        final JavaObjectEntry entry =
            new JavaObjectEntry(obj, newCounter, classNumber, nodeIdentifier);
        if (logger.isLoggable(Level.INFO))
            logger.log
                (Level.INFO,
                 "returning new object "+
                 System.identityHashCode(entry.object()));
        toErlangMap.put(obj_key, entry);
        final JavaObjectKey key =
            new JavaObjectKey(newCounter,nodeIdentifier);
        fromErlangMap.put(key, entry);
        long refCount = entry.alias();
        return makeErlangObjectKey(key.key(), refCount, entry.classNumber(), key.nodeId());
    }

    public synchronized OtpErlangObject acc_map_to_erlang(final Object obj) {
        final Object obj_key = obj;
        final OtpErlangObject oldValue = accToErlangMap.get(obj_key);
        if (oldValue != null) {
            return oldValue;
        }

        final IntKey key = new IntKey(accCounter++);
        final OtpErlangObject erlangKey = makeErlangKey("executable", key,
                                                        nodeIdentifier);
        accToErlangMap.put(obj_key, erlangKey);
        accFromErlangMap.put(erlangKey, obj);
        return erlangKey;
    }

    synchronized OtpErlangObject map_new_thread_to_erlang(final ThreadMsgHandler th) {
        final IntKey key = new IntKey(threadCounter++);
        final OtpErlangObject erlangKey = makeErlangKey("thread", key,
                                                        nodeIdentifier);
        threadMap.put(erlangKey, th);
        return erlangKey;
    }

    public OtpErlangObject map_to_erlang(final Object obj, final Class classType)
        throws Exception {
        if (classType == java.lang.Integer.TYPE) {
            return map_to_erlang_int(((Integer) obj).intValue());
        } else if (classType == java.lang.Short.TYPE) {
            return map_to_erlang_short(((Short) obj).shortValue());
        } else if (classType == java.lang.Long.TYPE) {
            return map_to_erlang_long(((Long) obj).longValue());
        } else if (classType == java.lang.Byte.TYPE) {
            return map_to_erlang_byte(((Byte) obj).byteValue());
        } else if (classType == java.lang.Boolean.TYPE) {
            return map_to_erlang_boolean(((Boolean) obj).booleanValue());
        } else if (classType == java.lang.Character.TYPE) {
            return map_to_erlang_character(((Character) obj).charValue());
        } else if (classType == java.lang.Float.TYPE) {
            return map_to_erlang_float(((Float) obj).floatValue());
        } else if (classType == java.lang.Double.TYPE) {
            return map_to_erlang_double(((Double) obj).doubleValue());
        } else if (classType == java.lang.Void.TYPE) {
            return map_to_erlang_void();
        } else {
            return map_to_erlang(obj);
        }
    }

    public OtpErlangObject map_to_erlang(final Object obj, final int pos,
                                         final Class classType) throws Exception {
        if (classType == java.lang.Integer.TYPE) {
            return map_to_erlang_long(Array.getInt(obj, pos));
        } else if (classType == java.lang.Short.TYPE) {
            return map_to_erlang_long(Array.getShort(obj, pos));
        } else if (classType == java.lang.Long.TYPE) {
            return map_to_erlang_long(Array.getLong(obj, pos));
        } else if (classType == java.lang.Byte.TYPE) {
            return map_to_erlang_long(Array.getByte(obj, pos));
        } else if (classType == java.lang.Boolean.TYPE) {
            return map_to_erlang_boolean(Array.getBoolean(obj, pos));
        } else if (classType == java.lang.Character.TYPE) {
            return map_to_erlang_character(Array.getChar(obj, pos));
        } else if (classType == java.lang.Float.TYPE) {
            return map_to_erlang_double(Array.getFloat(obj, pos));
        } else if (classType == java.lang.Double.TYPE) {
            return map_to_erlang_double(Array.getDouble(obj, pos));
        } else if (classType == java.lang.Void.TYPE) {
            return map_to_erlang_void();
        } else {
            return map_to_erlang(Array.get(obj, pos));
        }
    }

    public static OtpErlangObject map_to_erlang_long(final long value) {
        return new OtpErlangLong(value);
    }

    public static OtpErlangObject map_to_erlang_int(final int value) {
        return new OtpErlangInt(value);
    }

    public static OtpErlangObject map_to_erlang_short(final short value) {
        return new OtpErlangShort(value);
    }

    public static OtpErlangObject map_to_erlang_byte(final byte value) {
        return new OtpErlangByte(value);
    }

    public static OtpErlangObject map_to_erlang_character(final char value) {
        return new OtpErlangChar(value);
    }

    public static OtpErlangObject map_to_erlang_double(final double value) {
        if (Double.isNaN(value) || Double.isInfinite(value))
          throw new ArithmeticException("Can't represent infinite or NaN doubles");

        return new OtpErlangDouble(value);
    }

    public static OtpErlangObject map_to_erlang_float(final float value) {
        if (Float.isNaN(value) || Float.isInfinite(value))
            throw new ArithmeticException("Can't represent infinite or NaN floats");

        return new OtpErlangDouble(value);
    }

    public static OtpErlangObject map_to_erlang_boolean(final boolean value) {
        return new OtpErlangBoolean(value);
    }

    public static OtpErlangObject map_to_erlang_void() {
        return new OtpErlangAtom("void");
    }

    public static OtpErlangObject map_to_erlang_null() {
        return new OtpErlangAtom("null");
    }

    public JavaObjectKey objectKeyFromErlang(OtpErlangObject obj) {
        final OtpErlangTuple tuple = (OtpErlangTuple) obj;
        final OtpErlangLong key = (OtpErlangLong) tuple.elementAt(1);
        return new JavaObjectKey(key.longValue(),nodeIdentifier);
    }

    synchronized OtpErlangObject free(final OtpErlangObject arg) {
        final JavaObjectKey key = objectKeyFromErlang(arg);
        final JavaObjectEntry entry = fromErlangMap.get(key);
        final RefEqualsObject objKey = new RefEqualsObject(entry.object());
        toErlangMap.remove(objKey);
        fromErlangMap.remove(key);
        return new OtpErlangBoolean(true);
    }

    synchronized OtpErlangObject freeInstance(final OtpErlangObject arg) {
        final JavaObjectKey key = objectKeyFromErlang(arg);
        final JavaObjectEntry entry = fromErlangMap.get(key);
        if (entry.free() <= 0) {
            if (logger.isLoggable(Level.INFO))
                logger.log
                    (Level.INFO,
                     "freeing "+System.identityHashCode(entry.object()));
            final RefEqualsObject objKey = new RefEqualsObject(entry.object());
            toErlangMap.remove(objKey);
            fromErlangMap.remove(key);
            return new OtpErlangBoolean(true);
        } else {
            if (logger.isLoggable(Level.INFO))
                logger.log
                    (Level.INFO,
                     System.identityHashCode(entry.object())+
                     " has "+entry.references()+" references");
            return new OtpErlangBoolean(false);
        }
    }

    synchronized OtpErlangObject memoryUsage(final OtpErlangObject arg) {
        int N = toErlangMap.size();
        int M = fromErlangMap.size();
        if (N != M && logger.isLoggable(Level.WARNING))
            logger.log
                (Level.WARNING,
                 "Warning: from table has size "+M+"=/="+N+" to table");
        return new OtpErlangInt(N);
    }

    OtpErlangObject identity(final OtpErlangObject arg) {
        try {
            final Object obj = java_value_from_erlang(arg);
            return map_to_erlang(obj,obj.getClass());
        } catch (Exception e) { return map_to_erlang_null(); }
    }

    OtpErlangObject objTypeCompat(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple tuple = (OtpErlangTuple) cmd;
        final OtpErlangObject[] alternatives = ((OtpErlangTuple) tuple
                                                .elementAt(0)).elements();
        final OtpErlangObject[] objs = ((OtpErlangTuple) tuple.elementAt(1))
            .elements();

	if (logger.isLoggable(Level.FINER))
	    logger.log(Level.FINER,"objTypeCompat: "+cmd);

        for (int i = 0; i < objs.length; i++) {
            final Type t = fromErlType(alternatives[i]);
            final Class tc = (Class) t;
            if (!is_acceptable_as_argument(objs[i], tc)) {
                return new OtpErlangBoolean(false);
            }
        }
        return new OtpErlangBoolean(true);
    }

    synchronized OtpErlangObject lookupClass(final OtpErlangObject cmd) throws Exception {
	final Class cl = findClass(cmd);
	final Integer classNumber = classMap.get(cl);
	
	if (classNumber != null) {
	    return new OtpErlangLong(classNumber);
	} else {
	    classMap.put(cl,classCounter);
	    return new OtpErlangLong(classCounter++);
	}
    }

    OtpErlangObject getConstructor(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final OtpErlangTuple typeList = (OtpErlangTuple) t.elementAt(2);
        final Constructor cnstr = getConstructor(t.elementAt(0), typeList.elements());
        if (logger.isLoggable(Level.FINER)) {
            logger.log(Level.FINER,"\rcmd " + cmd + " has typelist "
                       + typeList.elements());
        }
        return acc_map_to_erlang(cnstr);
    }

    OtpErlangObject getField(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Field field = getField(findClass(t.elementAt(0)),
                                     ((OtpErlangAtom) t.elementAt(1)).atomValue());
        return acc_map_to_erlang(field);
    }

    public static String add_className(String locationStr,
                                       final String className) {
        final String separator = System.getProperty("file.separator");
        if (locationStr.endsWith(separator)) {
            locationStr = locationStr.substring(0, locationStr.length() - 1);
        }
        final String[] classParts = className.split("\\.");
        String retvalue = locationStr;
        for (final String part : classParts) {
            retvalue = retvalue + separator + part;
        }
        return retvalue + ".class";
    }

    public static String getExtension(final String s) {

        final String separator = System.getProperty("file.separator");
        String filename;

        // Remove the path upto the filename.
        final int lastSeparatorIndex = s.lastIndexOf(separator);
        if (lastSeparatorIndex == -1) {
            filename = s;
        } else {
            filename = s.substring(lastSeparatorIndex + 1);
        }

        // Remove the extension.
        final int extensionIndex = filename.lastIndexOf(".");
        if (extensionIndex == -1) {
            return "";
        }

        return filename.substring(extensionIndex);
    }

    OtpErlangObject getConstructors(final OtpErlangObject cmd)
        throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final boolean observerInPackage = ((OtpErlangAtom) t.elementAt(1))
            .booleanValue();
        final Class cl = findClass(t.elementAt(0));
        final Constructor[] constructors = cl.getConstructors();
        final ArrayList<OtpErlangTuple> erlConstructors = new ArrayList<OtpErlangTuple>();
        for (final Constructor constructor : constructors) {
            final int modifiers = constructor.getModifiers();
            if (is_visibleToUs(modifiers, observerInPackage)) {
                final OtpErlangAtom name = new OtpErlangAtom(
                                                             constructor.getName());
                final Type[] parameterTypes = constructor.getParameterTypes();
                final OtpErlangObject[] erlTypes = new OtpErlangObject[parameterTypes.length];
                for (int i = 0; i < parameterTypes.length; i++) {
                    erlTypes[i] = toErlType(parameterTypes[i]);
                }
                erlConstructors.add(makeErlangTuple(name, new OtpErlangList(erlTypes)));
            }
        }
        final OtpErlangTuple[] tmp_arr = new OtpErlangTuple[erlConstructors
                                                            .size()];
        for (int i = 0; i < erlConstructors.size(); i++) {
            tmp_arr[i] = erlConstructors.get(i);
        }
        return new OtpErlangList(tmp_arr);
    }

    OtpErlangObject getMethods(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
	final boolean selectStatics = ((OtpErlangAtom) t.elementAt(1))
                .booleanValue(); 
        final boolean observerInPackage = ((OtpErlangAtom) t.elementAt(2))
                .booleanValue();
        final Class cl = findClass(t.elementAt(0));
        final Method[] methods = cl.getMethods();
        final ArrayList<OtpErlangTuple> erlMethods = new ArrayList<OtpErlangTuple>();
        for (final Method method : methods) {
            if (method.isBridge() || method.isSynthetic()) {
                if (logger.isLoggable(Level.FINER)) {
                    logger.log(Level.FINER,"Skipping synthetic or bridge method "
			       + method + " in class " + cl);
                }
                continue;
            }
            final int modifiers = method.getModifiers();
            if (is_static(modifiers) != selectStatics) continue;
            if (is_executable(modifiers)
                && is_visibleToUs(modifiers, observerInPackage)) {
                final OtpErlangAtom name = new OtpErlangAtom(method.getName());
                final Type[] parameterTypes = method.getParameterTypes();
                final OtpErlangObject[] erlTypes = new OtpErlangObject[parameterTypes.length];
                for (int i = 0; i < parameterTypes.length; i++) {
                    erlTypes[i] = toErlType(parameterTypes[i]);
                }
                erlMethods.add(makeErlangTuple(name, new OtpErlangList(erlTypes)));
            } else if (logger.isLoggable(Level.FINER)) {
                logger.log(Level.FINER,"\rMethod is not visible to us");
            }
        }
        final OtpErlangTuple[] tmp_arr = new OtpErlangTuple[erlMethods.size()];
        for (int i = 0; i < erlMethods.size(); i++) {
            tmp_arr[i] = erlMethods.get(i);
        }
        return new OtpErlangList(tmp_arr);
    }

    OtpErlangObject getClasses(final OtpErlangObject cmd)
        throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Class cl = findClass(t.elementAt(0));
        final Class[] classes = cl.getClasses();
        final ArrayList<OtpErlangAtom> erlClasses = new ArrayList<OtpErlangAtom>();
        for (final Class cl_cand : classes) {
            final int modifiers = cl_cand.getModifiers();

            if (!is_interface(modifiers)) {
                final OtpErlangAtom name = new OtpErlangAtom(cl_cand.getName());
                erlClasses.add(name);
            }
        }
        final OtpErlangAtom[] tmp_arr = new OtpErlangAtom[erlClasses.size()];
        for (int i = 0; i < erlClasses.size(); i++) {
            tmp_arr[i] = erlClasses.get(i);
        }
        return new OtpErlangList(tmp_arr);
    }

    OtpErlangObject getFields(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
	final boolean selectStatics = ((OtpErlangAtom) t.elementAt(1))
                .booleanValue(); 
        final Class cl = findClass(t.elementAt(0));
        final Field[] fields = cl.getFields();
        final ArrayList<OtpErlangTuple> erlFields = new ArrayList<OtpErlangTuple>();
        for (final Field field : fields) {
            final int modifiers = field.getModifiers();
            if (is_static(modifiers) != selectStatics) continue;

            final OtpErlangAtom name = new OtpErlangAtom(field.getName());
            final OtpErlangObject fieldType = toErlType(field.getType());
            if (field.isSynthetic()) {
                if (logger.isLoggable(Level.FINER)) {
                    logger.log(Level.FINER,"Skipping synthetic or bridge field "
                            + field + " in class " + cl);
                }
                continue;
            }
            erlFields.add(makeErlangTuple(name, new OtpErlangList(fieldType)));
        }
        final OtpErlangTuple[] tmp_arr = new OtpErlangTuple[erlFields.size()];
        for (int i = 0; i < erlFields.size(); i++) {
            tmp_arr[i] = erlFields.get(i);
        }
        return new OtpErlangList(tmp_arr);
    }

    static boolean is_executable(final int modifier) {
        return (modifier & Modifier.ABSTRACT) == 0;
    }

    static boolean is_interface(final int modifier) {
        return (modifier & Modifier.INTERFACE) != 0;
    }

    static boolean is_static(final int modifier) {
        return (modifier & Modifier.STATIC) != 0;
    }

    static boolean is_visibleToUs(final int modifier, final boolean inPackage) {
        if (!inPackage) {
            return (modifier & Modifier.PUBLIC) != 0;
        } else {
            return (modifier & (Modifier.PUBLIC | Modifier.PROTECTED)) != 0
                || (modifier & Modifier.PRIVATE) == 0;
        }
    }

    Field get_field(final OtpErlangObject obj) throws Exception {
        final Object result = accFromErlangMap.get(obj);
        if (result instanceof Field) {
            return (Field) result;
        }
        throw new Exception();
    }

    OtpErlangObject getMethod(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String methodName = ((OtpErlangAtom) t.elementAt(1)).atomValue();
        final OtpErlangTuple typeList = (OtpErlangTuple) t.elementAt(2);
        final Method method = getMethod(findClass(t.elementAt(0)), methodName,
                typeList.elements());
	return acc_map_to_erlang(method);
    }

    Constructor getConstructor(final OtpErlangObject classRef,
            final OtpErlangObject[] erlTypes) throws Exception {
        final Class cl = findClass(classRef);
        final Type[] types = fromErlTypes(erlTypes);
        for (final Constructor cnstr : cl.getConstructors()) {
            if (checkTypes(cnstr.getParameterTypes(), types)) {
                // Fix for java bug 4071957
                if (cl.isMemberClass()) {
                    cnstr.setAccessible(true);
                }
                return cnstr;
            }
        }
	if (logger.isLoggable(Level.FINE))
	    logger.log(Level.FINE,"No constructor found for " + cl.getName() + ":");
        printObjectArray(types);
	if (logger.isLoggable(Level.FINER)) {
	    logger.log(Level.FINER,"");
	    logger.log(Level.FINER,"Available constructors: ");
	    for (final Constructor cnstr : cl.getConstructors()) {
		logger.log(Level.FINER,"constructor: ");
		printObjectArray(cnstr.getParameterTypes());
	    }
	    logger.log(Level.FINER,"\r------------------------");
	}
        throw new Exception();
    }

    static Field getField(final Class cl, final String fieldName)
        throws Exception {
        for (final Field field : cl.getFields()) {
            if (field.getName().equals(fieldName)) {
                // Fix for java bug 4071957
                if (cl.isMemberClass()) {
                    field.setAccessible(true);
                }
                return field;
            }
        }
	if (logger.isLoggable(Level.FINE))
	    logger.log(Level.FINE,"\rNo field found");
	if (logger.isLoggable(Level.FINER)) {
	    for (final Field field : cl.getFields()) {
		logger.log(Level.FINER,"\rfield: " + field.getName());
	    }
        }
        throw new Exception();
    }

    static Method getMethod(final Class cl, final String methodName,
                            final OtpErlangObject[] erlTypes) throws Exception {
        final Type[] types = fromErlTypes(erlTypes);
        for (final Method method : cl.getMethods()) {
            if (!method.getName().equals(methodName)) {
                continue;
            }
            if (checkTypes(method.getParameterTypes(), types)) {
                // Fix for java bug 4071957
                if (cl.isMemberClass() || cl.isAnonymousClass()) {
                    method.setAccessible(true);
                }
                return method;
            }
        }
	if (logger.isLoggable(Level.FINE))
	    logger.log(Level.FINE,"No method found for " + cl.getName() + "."
		       + methodName + ":");
	if (logger.isLoggable(Level.FINER)) {
	    printObjectArray(types);
	    logger.log(Level.FINER,"");
	    logger.log(Level.FINER,"Available methods: ");
	    for (final Method method : cl.getMethods()) {
		if (method.getName().equals(methodName)) {
		    logger.log(Level.FINER,"method: ");
		    printObjectArray(method.getParameterTypes());
		}
	    }
	    logger.log(Level.FINER,"\r------------------------");
	}
        throw new Exception();
    }

    Object get_fun(final OtpErlangObject cmd) throws Exception {
        final Object result = accFromErlangMap.get(cmd);
        if (result instanceof Method || result instanceof Constructor) {
            return result;
        }
	if (logger.isLoggable(Level.FINE))
	    logger.log(Level.FINE,cmd + " is not a method/constructor");
	if (logger.isLoggable(Level.FINER)) {
	    final Set<OtpErlangObject> keys = accFromErlangMap.keySet();
	    logger.log(Level.FINER,"\rMap contains:");
	    for (final OtpErlangObject key : keys) {
		logger.log(Level.FINER,key + ",");
	    }
	}
        throw new Exception();
    }

    static boolean is_integer_like(final OtpErlangObject value) {
        return value instanceof OtpErlangLong || value instanceof OtpErlangInt
            || value instanceof OtpErlangShort
            || value instanceof OtpErlangChar
            || value instanceof OtpErlangByte;
    }

    static boolean is_float_like(final OtpErlangObject value) {
        return value instanceof OtpErlangFloat
            || value instanceof OtpErlangDouble;
    }

    @SuppressWarnings("unchecked")
    boolean is_acceptable_as_argument(final OtpErlangObject value,
                                      final Class type) throws Exception {
        Object obj;
        boolean result;

        try {
            obj = java_value_from_erlang(value, type);
        } catch (final Exception e) {
	    if (logger.isLoggable(Level.FINER))
		logger.log
		    (Level.FINER,
		     "cannot convert "+value+" into type "+type);
            return false;
        }

	if (obj == null)
	    return !is_basic_type(type);
	
        final Class normalizedType = conv_basic_type(type);
        result = obj != null && normalizedType.isAssignableFrom(obj.getClass());
	if (!result && logger.isLoggable(Level.FINER)) {
	    logger.log
		(Level.FINER,
		 value+" is not acceptable as an argument for "+type);
	}
        return result;
    }

    static boolean is_basic_type(final Class type) {
	return
	    (type == java.lang.Integer.TYPE)
	    || (type == java.lang.Long.TYPE)
	    || (type == java.lang.Short.TYPE)
	    || (type == java.lang.Character.TYPE)
	    || (type == java.lang.Byte.TYPE)
	    || (type == java.lang.Float.TYPE)
	    || (type == java.lang.Double.TYPE)
	    || (type == java.lang.Boolean.TYPE)
	    || (type == java.lang.Void.TYPE);
    }

    static Class conv_basic_type(final Class type) {
        if (type == java.lang.Integer.TYPE) {
            return java.lang.Integer.class;
        }
        if (type == java.lang.Long.TYPE) {
            return java.lang.Long.class;
        }
        if (type == java.lang.Short.TYPE) {
            return java.lang.Short.class;
        }
        if (type == java.lang.Character.TYPE) {
            return java.lang.Character.class;
        }
        if (type == java.lang.Byte.TYPE) {
            return java.lang.Byte.class;
        }
        if (type == java.lang.Float.TYPE) {
            return java.lang.Float.class;
        }
        if (type == java.lang.Double.TYPE) {
            return java.lang.Double.class;
        }
        if (type == java.lang.Boolean.TYPE) {
            return java.lang.Boolean.class;
        }
        if (type == java.lang.Void.TYPE) {
            return java.lang.Void.class;
        }
        return type;
    }

    static void printObjectArray(final Object[] arr) {
	if (logger.isLoggable(Level.FINER)) {
	    for (final Object t : arr) {
		logger.log(Level.FINER,t + ", ");
	    }
        }
    }

    static OtpErlangObject toErlType(final Type t) throws Exception {
        if (t instanceof Class) {
            final Class c = (Class) t;
            if (c.isArray()) {
                return makeErlangTuple(new OtpErlangAtom("array"),
                                       new OtpErlangAtom(getArrayElementClass(c)
                                                         .getCanonicalName()), new OtpErlangLong(
                                                                                                 dimensions(c)));
            } else {
                return new OtpErlangAtom(c.getCanonicalName());
            }
        } else {
	if (logger.isLoggable(Level.WARNING))
            logger.log(Level.WARNING,"\rCannot handle " + t + " yet");
	throw new Exception();
        }
    }

    static boolean checkTypes(final Type a1[], final Type a2[]) {
        if (a1.length != a2.length) {
            return false;
        }
        for (int i = 0; i < a1.length; i++) {
            if (!a1[i].equals(a2[i])) {
                return false;
            }
        }
        return true;
    }

    public OtpErlangObject return_value(final Object obj) throws Exception {
        if (obj instanceof OtpErlangObject) {
            return makeErlangTuple(new OtpErlangAtom("value"),
                                   (OtpErlangObject) obj);
        } else if (obj instanceof Throwable) {
            final Throwable t = (Throwable) obj;
            return makeErlangTuple(new OtpErlangAtom("exception"),
                                   map_to_erlang(t));
        }
        logger.log(Level.SEVERE,"Cannot return non-Erlang/non-Exception " + obj);
        throw new Exception();
    }

    //////////////////////////////////////////////////////////////////////
    OtpErlangObject new_proxy_class(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final OtpErlangAtom className = (OtpErlangAtom) t.elementAt(0);
        final OtpErlangObject methods = t.elementAt(1);
        final Class cl = Class.forName(className.atomValue());
        final ProxyInstanceFactory pcl = ProxyFactoryClass.newClass(this, cl, methods);
        return map_to_erlang(pcl);
    }

    OtpErlangObject new_proxy_object(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final ProxyInstanceFactory pif = (ProxyInstanceFactory) java_value_from_erlang(t.elementAt(0));
        final int objectId = ((OtpErlangLong) t.elementAt(1)).intValue();
        final OtpErlangPid pid = (OtpErlangPid) t.elementAt(2);
        final OtpErlangObject obj = pif.newInstance(objectId,pid);
        return obj;
    }

    OtpErlangObject proxy_reply(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final ProxyHandler handler = (ProxyHandler) java_value_from_erlang(t.elementAt(0));
        handler.setAnswer(t.elementAt(1));
        return map_to_erlang_void();
    }
}
