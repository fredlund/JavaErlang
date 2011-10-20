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
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.net.URL;
import java.security.CodeSource;
import java.security.ProtectionDomain;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

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
import com.ericsson.otp.erlang.OtpNodeStatus;

@SuppressWarnings("rawtypes")
public class JavaErlang {
    static volatile Map<RefEqualsObject, OtpErlangObject> toErlangMap;
    static volatile Map<OtpErlangObject, Object> fromErlangMap;
    static volatile Map<Object, OtpErlangObject> accToErlangMap;
    static volatile Map<OtpErlangObject, Object> accFromErlangMap;
    static volatile Map<OtpErlangObject, ThreadMsgHandler> threadMap;
    static volatile int objCounter = 0;
    static volatile int accCounter = 0;
    static volatile int threadCounter = 0;

    static volatile OtpMbox msgs;
    static volatile OtpErlangObject nodeIdentifier = null;
    static volatile boolean verbose = false;

    static boolean isConnected = false;

    public static void main(final String args[]) {
        try {
            final String number = args[0];
            if (args.length > 1) {
                if (args[1].equals("-verbose")) {
                    verbose = true;
                } else {
                    System.err.println("\rCannot understand argument "
                            + args[1]);
                    System.exit(-1);
                }
            }
            toErlangMap = new HashMap<RefEqualsObject, OtpErlangObject>();
            fromErlangMap = new HashMap<OtpErlangObject, Object>();
            accToErlangMap = new HashMap<Object, OtpErlangObject>();
            accFromErlangMap = new HashMap<OtpErlangObject, Object>();
            threadMap = new HashMap<OtpErlangObject, ThreadMsgHandler>();
            final OtpNode node = new OtpNode("javaNode_" + number);
            node.registerStatusHandler(new OtpStatusHandler());
            if (verbose) {
                System.err.println("\rRegistered host " + node.node());
            }
            msgs = node.createMbox("javaNode");
            do_receive();
        } catch (final Throwable e) {
            System.err
                    .println("*** Unexpected exception failure in JavaErlang: "
                            + e);
            e.printStackTrace();
        }
    }

    static void do_receive() throws Exception {
        do {
            final OtpErlangObject msg = msgs.receive();
            if (verbose) {
                System.err.println("\rGot message " + msg);
            }
            if (msg instanceof OtpErlangTuple) {
                final OtpErlangTuple t = (OtpErlangTuple) msg;
                if (t.arity() == 3 && t.elementAt(0) instanceof OtpErlangAtom
                        && t.elementAt(2) instanceof OtpErlangPid) {
                    handle_nonthread_msg(t);
                } else if (t.arity() == 4
                        && t.elementAt(0) instanceof OtpErlangAtom
                        && t.elementAt(3) instanceof OtpErlangPid) {
                    handle_thread_msg(t);
                } else {
                    System.err.println("\nMalformed message " + msg
                            + " received");
                }
            } else {
                System.err.println("\nMalformed message " + msg + " received");
            }
        } while (true);
    }

    public static void reply(final Object reply, final OtpErlangPid replyPid)
            throws Exception {
        // System.err.println("returning "+return_value(reply)+" to "+replyPid);
        JavaErlang.msgs.send(replyPid, return_value(reply));
    }

    static void handle_nonthread_msg(final OtpErlangTuple t) throws Exception {
        final String tag = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final OtpErlangObject argument = t.elementAt(1);
        final OtpErlangPid replyPid = (OtpErlangPid) t.elementAt(2);
        final OtpErlangObject nodeId = t.elementAt(3);
        if (!isConnected) {
            if (tag.equals("connect")) {
                final String nameOfRunningVM = ManagementFactory
                        .getRuntimeMXBean().getName();
                final int p = nameOfRunningVM.indexOf('@');
                final String pid = nameOfRunningVM.substring(0, p);
                final Integer intPid = new Integer(pid);
                if (nodeIdentifier == null) {
                    nodeIdentifier = argument;
                }
                reply(makeErlangTuple(new OtpErlangAtom("connected"),
                        JavaErlang.msgs.self(), new OtpErlangLong(intPid)),
                        replyPid);
                isConnected = true;
            } else {
                System.err.println("\nFirst message should be connect " + t);
            }
        } else {
            Object result;
            try {
                result = handleNonThreadMsg(tag, argument, replyPid);
            } catch (final Throwable e) {
                if (verbose) {
                    System.err.println("\r\n*** Exception " + e + " thrown");
                    System.err.print("\r");
                    e.printStackTrace();
                }
                result = e;
            }
            reply(result, replyPid);
        }
    }

    static void handle_thread_msg(final OtpErlangTuple t) throws Exception {
        final Object map_result = threadMap.get(t.elementAt(1));
        if (map_result instanceof ThreadMsgHandler) {
            final ThreadMsgHandler th = (ThreadMsgHandler) map_result;
            th.queue.put(t);
        } else {
            System.err.println("Thread " + t.elementAt(1) + " not found");
        }
    }

    static OtpErlangObject handleNonThreadMsg(final String tag,
            final OtpErlangObject argument, final OtpErlangPid replyPid)
            throws Exception {
        if (tag.equals("reset")) {
            JavaErlang.objCounter = 0;
            JavaErlang.toErlangMap = new HashMap<RefEqualsObject, OtpErlangObject>();
            JavaErlang.fromErlangMap = new HashMap<OtpErlangObject, Object>();
            for (final ThreadMsgHandler th : threadMap.values()) {
                stop_thread(th, replyPid);
            }
            JavaErlang.threadMap = new HashMap<OtpErlangObject, ThreadMsgHandler>();
            System.gc();
            return new OtpErlangAtom("ok");
        } else if (tag.equals("terminate")) {
            if (JavaErlang.verbose) {
                System.err.println("\r\nterminating java...");
            }
            reply(new OtpErlangAtom("ok"), replyPid);
            System.exit(0);
            return JavaErlang.map_to_erlang_void();
        } else if (tag.equals("connect")) {
            return new OtpErlangAtom("already_connected");
        } else if (tag.equals("define_invocation_handler")) {
            return invhandler(argument);
        } else if (tag.equals("proxy_reply")) {
            return proxy_reply(argument);
        } else if (tag.equals("getConstructors")) {
            return getConstructors(argument);
        } else if (tag.equals("getMethods")) {
            return getMethods(argument);
        } else if (tag.equals("getClasses")) {
            return getClasses(argument);
        } else if (tag.equals("getFields")) {
            return getFields(argument);
        } else if (tag.equals("getClassLocation")) {
            return getClassLocation(argument);
        } else if (tag.equals("getConstructor")) {
            return getConstructor(argument);
        } else if (tag.equals("getMethod")) {
            return getMethod(argument);
        } else if (tag.equals("getField")) {
            return getField(argument);
        } else if (tag.equals("objTypeCompat")) {
            return objTypeCompat(argument);
        } else if (tag.equals("free")) {
            return free(argument);
        } else if (tag.equals("createThread")) {
            return create_thread();
        } else if (tag.equals("stopThread")) {
            final Object map_result = threadMap.get(argument);
            if (map_result instanceof ThreadMsgHandler) {
                final ThreadMsgHandler th = (ThreadMsgHandler) map_result;
                threadMap.remove(argument);
                stop_thread(th, replyPid);
            } else {
                throw new Exception();
            }
            return map_to_erlang_void();
        } else {
            System.err.println("*** Error: JavaErlang: \nTag " + tag
                    + " not recognized");
            throw new Exception();
        }
    }

    static OtpErlangObject create_thread() {
        final ThreadMsgHandler th = ThreadMsgHandler.createThreadMsgHandler();
        return map_new_thread_to_erlang(th);
    }

    static void stop_thread(final ThreadMsgHandler th,
            final OtpErlangPid replyPid) throws Exception {
        th.queue.put(mkStopThreadMsg(replyPid));
    }

    static OtpErlangObject mkStopThreadMsg(final OtpErlangPid replyPid) {
        return makeErlangTuple(new OtpErlangAtom("stopThread"),
                map_to_erlang_null(), map_to_erlang_null(), replyPid);
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

    static Object java_value_from_erlang(final OtpErlangObject value)
            throws Exception {
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
            System.err.println("java_value_from_erlang: atom " + value);
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
                        initializeArray(array, arg);
                        return array;
                    }
                }
            } else if (arity == 3) {
                final String tag = ((OtpErlangAtom) t.elementAt(0)).atomValue();
                if (tag.equals("object")) {
                    final Object result = JavaErlang.fromErlangMap.get(t);
                    if (result == null) {
                        if (JavaErlang.verbose) {
                            System.err.println("\rTranslating " + value);
                        }
                        throw new Exception();
                    }
                    return result;
                } else if (tag.equals("executable")) {
                    final Object result = JavaErlang.accFromErlangMap.get(t);
                    if (result == null) {
                        if (JavaErlang.verbose) {
                            System.err.println("\rTranslating " + value);
                        }
                        throw new Exception();
                    }
                    return result;
                }
            }
        }
        System.err.println("java_value_from_erlang: " + value);
        throw new Exception();
    }

    static Object[] java_values_from_erlang(final OtpErlangObject[] values,
            final Type[] types) throws Exception {
        final Object[] objects = new Object[values.length];
        for (int i = 0; i < values.length; i++) {
            final Object value = java_value_from_erlang(values[i], types[i]);
            objects[i] = value;
        }
        return objects;
    }

    static Object java_value_from_erlang(final OtpErlangObject value,
            final Type type) throws Exception {
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
        } else {
            if (type instanceof Class) {
                final Class typeClass = (Class) type;
                final int dimensions = dimensions(typeClass);
                if (typeClass.isArray()) {
                    final Class arrElement = getArrayElementClass(typeClass);
                    final int[] lengths = checkDimensions(dimensions, value);
                    final Object arr = Array.newInstance(arrElement, lengths);
                    initializeArray(arr, value);
                    return arr;
                }
            }
            if (verbose) {
                System.err.println("Cannot convert " + value + " to type "
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
        System.err.println("\rerror: convert_to_character " + value);
        System.err.println("\rtype is " + value.getClass());
        throw new Exception();
    }

    static Object convert_to_byte(final OtpErlangObject value) throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).byteValue();
        }
        System.err.println("\rerror: convert_to_byte " + value);
        System.err.println("\rtype is " + value.getClass());
        throw new Exception();
    }

    static Object convert_to_float(final OtpErlangObject value)
            throws Exception {
        if (value instanceof OtpErlangDouble) {
            return ((OtpErlangDouble) value).floatValue();
        }
        System.err.println("\rerror: convert_to_float " + value);
        System.err.println("\rtype is " + value.getClass());
        throw new Exception();
    }

    static Object convert_to_double(final OtpErlangObject value)
            throws Exception {
        if (value instanceof OtpErlangDouble) {
            return ((OtpErlangDouble) value).doubleValue();
        }
        System.err.println("\rerror: convert_to_double " + value);
        System.err.println("\rtype is " + value.getClass());
        throw new Exception();
    }

    static Object convert_to_void(final OtpErlangObject value) throws Exception {
        System.err.println("\rerror: convert_to_void " + value);
        System.err.println("\rtype is " + value.getClass());
        throw new Exception();
    }

    static Object convert_to_short(final OtpErlangObject value)
            throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).shortValue();
        }
        System.err.println("\rerror: convert_to_short " + value);
        System.err.println("\rtype is " + value.getClass());
        throw new Exception();
    }

    static Object convert_to_integer(final OtpErlangObject value)
            throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).intValue();
        }
        System.err.println("\rerror: convert_to_integer " + value);
        System.err.println("\rtype is " + value.getClass());
        throw new Exception();
    }

    static Object convert_to_long(final OtpErlangObject value) throws Exception {
        if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong) value).longValue();
        }
        System.err.println("\rerror: convert_to_long " + value);
        System.err.println("\rtype is " + value.getClass());
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

    static void initializeArray(final Object arr, final OtpErlangObject value)
            throws Exception {
        final int len = Array.getLength(arr);
        final OtpErlangObject[] elements = elements(value);
        final Class objClass = Array.get(arr, 0).getClass();
        for (int i = 0; i < len; i++) {
            final OtpErlangObject element = elements[i];
            final Object obj_at_i = Array.get(arr, i);
            if (objClass.isArray()) {
                initializeArray(obj_at_i, element);
            } else {
                final Object setValue = JavaErlang.java_value_from_erlang(
                        element, objClass);
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
        // System.err.println("\rfromErlTypes(len="+len+")");
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
        System.err.println("\rtype " + erlType + " is not understood?");
        throw new Exception();
    }

    static Class findClass(final String className) throws Exception {
        try {
            final Class c = Class.forName(className);
            return c;
        } catch (final Exception e) {
        }

        final StringBuilder str = new StringBuilder(className);
        do {
            final int lastIndex = str.lastIndexOf(".");
            if (lastIndex == -1) {
                System.err.println("findClass: cannot locate class " + str);
                if (JavaErlang.verbose) {
                    System.err.println("findClass: cannot locate class " + str);
                }
                throw new Exception();
            }
            str.replace(lastIndex, lastIndex + 1, "$");
            try {
                final Class c = Class.forName(str.toString());
                return c;
            } catch (final Exception exc) {
            }

        } while (true);
    }

    public static OtpErlangObject map_to_erlang(final Object obj) {
        if (obj == null) {
            return map_to_erlang_null();
        }

        final RefEqualsObject obj_key = new RefEqualsObject(obj);
        final OtpErlangObject oldValue = JavaErlang.toErlangMap.get(obj_key);
        if (oldValue != null) {
            return oldValue;
        }

        final IntKey key = new IntKey(JavaErlang.objCounter++);
        final OtpErlangObject erlangKey = makeErlangKey("object", key,
                JavaErlang.nodeIdentifier);
        JavaErlang.toErlangMap.put(obj_key, erlangKey);
        JavaErlang.fromErlangMap.put(erlangKey, obj);
        return erlangKey;
    }

    public static OtpErlangObject acc_map_to_erlang(final Object obj) {
        final Object obj_key = obj;
        final OtpErlangObject oldValue = JavaErlang.accToErlangMap.get(obj_key);
        if (oldValue != null) {
            return oldValue;
        }

        final IntKey key = new IntKey(JavaErlang.accCounter++);
        final OtpErlangObject erlangKey = makeErlangKey("executable", key,
                JavaErlang.nodeIdentifier);
        JavaErlang.accToErlangMap.put(obj_key, erlangKey);
        JavaErlang.accFromErlangMap.put(erlangKey, obj);
        return erlangKey;
    }

    static OtpErlangObject map_new_thread_to_erlang(final ThreadMsgHandler th) {
        final IntKey key = new IntKey(JavaErlang.threadCounter++);
        final OtpErlangObject erlangKey = makeErlangKey("thread", key,
                JavaErlang.nodeIdentifier);
        JavaErlang.threadMap.put(erlangKey, th);
        return erlangKey;
    }

    public static OtpErlangObject map_to_erlang(final Object obj,
            final Class classType) throws Exception {
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
            return JavaErlang.map_to_erlang(obj);
        }
    }

    public static OtpErlangObject map_to_erlang(final Object obj,
            final int pos, final Class classType) throws Exception {
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
            return JavaErlang.map_to_erlang(Array.get(obj, pos));
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
        return new OtpErlangDouble(value);
    }

    public static OtpErlangObject map_to_erlang_float(final float value) {
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

    static OtpErlangObject free(final OtpErlangObject arg) {
        final Object object = JavaErlang.fromErlangMap.remove(arg);
        final RefEqualsObject obj_key = new RefEqualsObject(object);
        final OtpErlangObject oldValue = JavaErlang.toErlangMap.remove(obj_key);
        return map_to_erlang_void();
    }

    static OtpErlangObject objTypeCompat(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple tuple = (OtpErlangTuple) cmd;
        final OtpErlangObject[] alternatives = ((OtpErlangTuple) tuple
                .elementAt(0)).elements();
        final OtpErlangObject[] objs = ((OtpErlangTuple) tuple.elementAt(1))
                .elements();

        for (int i = 0; i < objs.length; i++) {
            final Type t = JavaErlang.fromErlType(alternatives[i]);
            final Class tc = (Class) t;
            if (!is_acceptable_as_argument(objs[i], tc)) {
                return new OtpErlangBoolean(false);
            }
        }
        return new OtpErlangBoolean(true);
    }

    static OtpErlangObject invhandler(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final OtpErlangPid pid = (OtpErlangPid) t.elementAt(0);
        final Object obj = JavaErlang.java_value_from_erlang(t.elementAt(1));
        final InvocationHandler handler = new InvHandler(pid, obj);
        return JavaErlang.map_to_erlang(handler);
    }

    static OtpErlangObject proxy_reply(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final InvHandler handler = (InvHandler) JavaErlang.fromErlangMap.get(t
                .elementAt(0));
        handler.setAnswer(t.elementAt(1));
        return new OtpErlangAtom("ok");
    }

    static OtpErlangObject getConstructor(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final OtpErlangTuple typeList = (OtpErlangTuple) t.elementAt(1);
        final Constructor cnstr = getConstructor(className, typeList.elements());
        if (JavaErlang.verbose) {
            System.err.println("\rcmd " + cmd + " has typelist "
                    + typeList.elements());
        }
        return JavaErlang.makeErlangTuple(JavaErlang.acc_map_to_erlang(cnstr),
                new OtpErlangInt(cnstr.getParameterTypes().length));
    }

    static OtpErlangObject getField(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final Field field = getField(className,
                ((OtpErlangAtom) t.elementAt(1)).atomValue());
        return JavaErlang.makeErlangTuple(JavaErlang.acc_map_to_erlang(field),
                new OtpErlangBoolean(Modifier.isStatic(field.getModifiers())));
    }

    static OtpErlangObject getClassLocation(final OtpErlangObject n) {
        String locationStr = "";
        final String className = ((OtpErlangAtom) n).atomValue();

        try {
            final Class cl = findClass(className);
            final ProtectionDomain d = cl.getProtectionDomain();
            final CodeSource cs = d.getCodeSource();
            final URL url = cs.getLocation();
            locationStr = url.toString();
        } catch (final Throwable t) {
        }

        if (locationStr.startsWith("file:")) {
            locationStr = locationStr.substring(5);
        }
        if (!locationStr.equals("")) {
            File fd = new File(locationStr);
            final String extension = getExtension(locationStr);
            if (extension.equals(".jar") && fd.isFile() && fd.canRead()) {
                return new OtpErlangString(locationStr);
            }

            if (fd.isDirectory()) {
                locationStr = add_className(locationStr, className);

                fd = new File(locationStr);
                if (fd.isFile() && fd.canRead()) {
                    return new OtpErlangString(locationStr);
                }
            }
        }
        return new OtpErlangString("");
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

    static OtpErlangObject getConstructors(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final boolean observerInPackage = ((OtpErlangAtom) t.elementAt(1))
                .booleanValue();
        final Class cl = JavaErlang.findClass(className);
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
                erlConstructors.add(JavaErlang.makeErlangTuple(name,
                        new OtpErlangBoolean(false),
                        new OtpErlangList(erlTypes)));
            }
        }
        final OtpErlangTuple[] tmp_arr = new OtpErlangTuple[erlConstructors
                .size()];
        for (int i = 0; i < erlConstructors.size(); i++) {
            tmp_arr[i] = erlConstructors.get(i);
        }
        return new OtpErlangList(tmp_arr);
    }

    static OtpErlangObject getMethods(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final boolean observerInPackage = ((OtpErlangAtom) t.elementAt(1))
                .booleanValue();
        final Class cl = JavaErlang.findClass(className);
        final Method[] methods = cl.getMethods();
        final ArrayList<OtpErlangTuple> erlMethods = new ArrayList<OtpErlangTuple>();
        for (final Method method : methods) {
            if (method.isBridge() || method.isSynthetic()) {
                if (JavaErlang.verbose) {
                    System.err.println("Skipping synthetic or bridge method "
                            + method + " in class " + className);
                }
                continue;
            }
            final int modifiers = method.getModifiers();
            if (is_executable(modifiers)
                    && is_visibleToUs(modifiers, observerInPackage)) {
                final OtpErlangAtom name = new OtpErlangAtom(method.getName());
                final Type[] parameterTypes = method.getParameterTypes();
                final OtpErlangObject[] erlTypes = new OtpErlangObject[parameterTypes.length];
                for (int i = 0; i < parameterTypes.length; i++) {
                    erlTypes[i] = toErlType(parameterTypes[i]);
                }
                erlMethods.add(JavaErlang.makeErlangTuple(name,
                        new OtpErlangBoolean(is_static(modifiers)),
                        new OtpErlangList(erlTypes)));
            } else if (JavaErlang.verbose) {
                System.err.println("\rMethod is not visible to us");
            }
        }
        final OtpErlangTuple[] tmp_arr = new OtpErlangTuple[erlMethods.size()];
        for (int i = 0; i < erlMethods.size(); i++) {
            tmp_arr[i] = erlMethods.get(i);
        }
        return new OtpErlangList(tmp_arr);
    }

    static OtpErlangObject getClasses(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final Class cl = JavaErlang.findClass(className);
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

    static OtpErlangObject getFields(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final Class cl = JavaErlang.findClass(className);
        final Field[] fields = cl.getFields();
        final ArrayList<OtpErlangTuple> erlFields = new ArrayList<OtpErlangTuple>();
        for (final Field field : fields) {
            final int modifiers = field.getModifiers();

            final OtpErlangAtom name = new OtpErlangAtom(field.getName());
            final OtpErlangObject fieldType = toErlType(field.getType());
            if (field.isSynthetic()) {
                if (JavaErlang.verbose) {
                    System.err.println("Skipping synthetic or bridge field "
                            + field + " in class " + className);
                }
                continue;
            }
            erlFields.add(JavaErlang.makeErlangTuple(name,
                    new OtpErlangBoolean(is_static(modifiers)), fieldType));
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

    static Field get_field(final OtpErlangObject obj) throws Exception {
        final Object result = JavaErlang.accFromErlangMap.get(obj);
        if (result instanceof Field) {
            return (Field) result;
        }
        throw new Exception();
    }

    static OtpErlangObject getMethod(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final String methodName = ((OtpErlangAtom) t.elementAt(1)).atomValue();
        final OtpErlangTuple typeList = (OtpErlangTuple) t.elementAt(2);
        final Method method = getMethod(JavaErlang.findClass(className),
                methodName, typeList.elements());
        final OtpErlangObject key = JavaErlang.acc_map_to_erlang(method);
        return JavaErlang.makeErlangTuple(key,
                new OtpErlangInt(method.getParameterTypes().length),
                new OtpErlangBoolean(Modifier.isStatic(method.getModifiers())));
    }

    static Constructor getConstructor(final String className,
            final OtpErlangObject[] erlTypes) throws Exception {
        final Class cl = JavaErlang.findClass(className);
        final Type[] types = JavaErlang.fromErlTypes(erlTypes);
        for (final Constructor cnstr : cl.getConstructors()) {
            if (checkTypes(cnstr.getParameterTypes(), types)) {
                // Fix for java bug 4071957
                if (cl.isMemberClass()) {
                    cnstr.setAccessible(true);
                }
                return cnstr;
            }
        }
        System.err.print("No constructor found for " + cl.getName() + ":");
        printObjectArray(types);
        System.err.println();
        System.err.print("Available constructors: ");
        for (final Constructor cnstr : cl.getConstructors()) {
            System.err.print("constructor: ");
            printObjectArray(cnstr.getParameterTypes());
        }
        System.err.println("\r------------------------");
        throw new Exception();
    }

    static Field getField(final String className, final String fieldName)
            throws Exception {
        final Class cl = JavaErlang.findClass(className);
        for (final Field field : cl.getFields()) {
            if (field.getName().equals(fieldName)) {
                // Fix for java bug 4071957
                if (cl.isMemberClass()) {
                    field.setAccessible(true);
                }
                return field;
            }
        }
        System.err.println("\rNo field found");
        for (final Field field : cl.getFields()) {
            System.err.println("\rfield: " + field.getName());
        }
        throw new Exception();
    }

    static Method getMethod(final Class cl, final String methodName,
            final OtpErlangObject[] erlTypes) throws Exception {
        final Type[] types = JavaErlang.fromErlTypes(erlTypes);
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
        System.err.print("No method found for " + cl.getName() + "."
                + methodName + ":");
        printObjectArray(types);
        System.err.println();
        System.err.print("Available methods: ");
        for (final Method method : cl.getMethods()) {
            if (method.getName().equals(methodName)) {
                System.err.print("method: ");
                printObjectArray(method.getParameterTypes());
            }
        }
        System.err.println("\r------------------------");
        throw new Exception();
    }

    static Object get_fun(final OtpErlangObject cmd) throws Exception {
        final Object result = JavaErlang.accFromErlangMap.get(cmd);
        if (result instanceof Method || result instanceof Constructor) {
            return result;
        }
        System.err.println(cmd + " is not a method/constructor");
        final Set<OtpErlangObject> keys = JavaErlang.accFromErlangMap.keySet();
        System.err.println("\rMap contains:");
        for (final OtpErlangObject key : keys) {
            System.err.print(key + ",");
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
    static boolean is_acceptable_as_argument(final OtpErlangObject value,
            final Class type) throws Exception {
        Object obj;
        boolean result;

        try {
            obj = JavaErlang.java_value_from_erlang(value, type);
        } catch (final Exception e) {
            return false;
        }

        final Class normalizedType = conv_basic_type(type);
        result = obj != null && normalizedType.isAssignableFrom(obj.getClass());
        return result;
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
        for (final Object t : arr) {
            System.err.print(t + ", ");
        }
    }

    static OtpErlangObject toErlType(final Type t) throws Exception {
        if (t instanceof Class) {
            final Class c = (Class) t;
            if (c.isArray()) {
                return JavaErlang.makeErlangTuple(new OtpErlangAtom("array"),
                        new OtpErlangAtom(JavaErlang.getArrayElementClass(c)
                                .getCanonicalName()), new OtpErlangLong(
                                JavaErlang.dimensions(c)));
            } else {
                return new OtpErlangAtom(c.getCanonicalName());
            }
        } else {
            System.err.println("\rCannot handle " + t + " yet");
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

    public static OtpErlangObject return_value(final Object obj)
            throws Exception {
        if (obj instanceof OtpErlangObject) {
            return JavaErlang.makeErlangTuple(new OtpErlangAtom("value"),
                    (OtpErlangObject) obj);
        } else if (obj instanceof Throwable) {
            final Throwable t = (Throwable) obj;
            return JavaErlang.makeErlangTuple(new OtpErlangAtom("exception"),
                    JavaErlang.map_to_erlang(t));
        }
        System.err.println("Cannot return non-Erlang/non-Exception " + obj);
        throw new Exception();
    }
}

class ThreadMsgHandler implements Runnable {
    BlockingQueue<OtpErlangObject> queue;

    ThreadMsgHandler() {
        queue = new LinkedBlockingQueue<OtpErlangObject>();
    }

    public static ThreadMsgHandler createThreadMsgHandler() {
        final ThreadMsgHandler th = new ThreadMsgHandler();
        new Thread(th).start();
        return th;
    }

    public void run() {
        try {
            do_receive();
        } catch (final Exception exc) {
            exc.printStackTrace();
        }
    }

    void do_receive() throws Exception {
        String tag;
        OtpErlangPid replyPid;
        OtpErlangObject argument;

        do {
            final OtpErlangTuple t = (OtpErlangTuple) queue.take();
            if (JavaErlang.verbose) {
                System.err.println(this + " got " + t);
            }
            try {
                tag = ((OtpErlangAtom) t.elementAt(0)).atomValue();
                argument = t.elementAt(2);
                replyPid = (OtpErlangPid) t.elementAt(3);
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
                } catch (final Throwable e) {
                    result = e;
                }
                if (result != null) {
                    JavaErlang.reply(result, replyPid);
                } else {
                    break;
                }
            } catch (final Exception e) {
                System.err.println("Malformed message " + t);
            }
        } while (true);
    }

    Object handleCall(final String tag, final OtpErlangObject argument)
            throws Exception {
        if (tag.equals("call_constructor")) {
            return call_constructor(argument);
        } else if (tag.equals("call_method")) {
            return call_method(argument);
        } else if (tag.equals("getFieldValue")) {
            return getFieldValue(argument);
        } else if (tag.equals("setFieldValue")) {
            return setFieldValue(argument);
        } else if (tag.equals("getClassName")) {
            return getClassName(argument);
        } else if (tag.equals("array_to_list")) {
            return array_to_list(argument);
        } else if (tag.equals("list_to_array")) {
            return list_to_array(argument);
        } else if (tag.equals("instof")) {
            return instof(argument);
        } else if (tag.equals("convert")) {
            return convert(argument);
        } else if (tag.equals("is_subtype")) {
            return is_subtype(argument);
        } else if (tag.equals("getClassName")) {
            return getClassName(argument);
        } else if (tag.equals("getSimpleClassName")) {
            return getSimpleClassName(argument);
        } else if (tag.equals("stopThread")) {
            return null;
        } else {
            System.err.println("\rBad tag " + tag + " in received message");
            throw new Exception();
        }
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject array_to_list(final OtpErlangObject value) throws Exception {
        final Object objs = JavaErlang.java_value_from_erlang(value);
        final Class cl = objs.getClass();
        final Class arrElement = JavaErlang.getArrayElementClass(cl);
        final int len = Array.getLength(objs);
        final OtpErlangObject objects[] = new OtpErlangObject[len];
        for (int i = 0; i < len; i++) {
            objects[i] = JavaErlang.map_to_erlang(objs, i, arrElement);
        }
        return new OtpErlangList(objects);
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject list_to_array(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final OtpErlangObject type = t.elementAt(0);
        final OtpErlangObject values = t.elementAt(1);
        final OtpErlangObject[] objs = ((OtpErlangTuple) values).elements();
        final Type element_type = JavaErlang.fromErlType(type);
        if (element_type instanceof Class) {
            final Class cl = (Class) element_type;
            final Object arr = Array.newInstance(cl, objs.length);
            for (int i = 0; i < objs.length; i++) {
                Array.set(arr, i, JavaErlang.java_value_from_erlang(objs[i],
                        element_type));
            }
            return JavaErlang.map_to_erlang(arr);
        } else {
            System.err.println("Cannot convert type description "
                    + element_type + " to a type class");
            throw new Exception();
        }
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject instof(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object obj = JavaErlang.java_value_from_erlang(t.elementAt(0));
        final String className = ((OtpErlangAtom) t.elementAt(1)).atomValue();
        final Class cl = JavaErlang.findClass(className);
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
        return JavaErlang.map_to_erlang(result, resultClass);
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    OtpErlangObject is_subtype(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final String className1 = ((OtpErlangAtom) t.elementAt(0)).atomValue();
        final String className2 = ((OtpErlangAtom) t.elementAt(1)).atomValue();
        final Class cl1 = JavaErlang.findClass(className1);
        final Class cl2 = JavaErlang.findClass(className2);
        return new OtpErlangBoolean(cl2.isAssignableFrom(cl1));
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject getClassName(final OtpErlangObject cmd) throws Exception {
        final Object obj = JavaErlang.java_value_from_erlang(cmd);
        final Class cl = obj.getClass();
        return new OtpErlangAtom(cl.getName());
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject getSimpleClassName(final OtpErlangObject cmd)
            throws Exception {
        final Object obj = JavaErlang.java_value_from_erlang(cmd);
        final Class cl = obj.getClass();
        return new OtpErlangAtom(cl.getSimpleName());
    }

    OtpErlangObject getFieldValue(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object obj = JavaErlang.fromErlangMap.get(t.elementAt(0));
        final Field field = JavaErlang.get_field(t.elementAt(1));
        final Object result = field.get(obj);
        return JavaErlang.map_to_erlang(result, field.getType());
    }

    OtpErlangObject setFieldValue(final OtpErlangObject cmd) throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object obj = JavaErlang.fromErlangMap.get(t.elementAt(0));
        final Field field = JavaErlang.get_field(t.elementAt(1));
        final OtpErlangObject value = t.elementAt(2);
        field.set(obj,
                JavaErlang.java_value_from_erlang(value, field.getType()));
        return JavaErlang.map_to_erlang_void();
    }

    @SuppressWarnings("rawtypes")
    OtpErlangObject call_constructor(final OtpErlangObject cmd)
            throws Exception {
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object fun = JavaErlang.get_fun(t.elementAt(0));
        final OtpErlangObject[] args = ((OtpErlangTuple) t.elementAt(1))
                .elements();
        Object result;

        final Constructor cnstr = (Constructor) fun;
        result = cnstr.newInstance(JavaErlang.java_values_from_erlang(args,
                cnstr.getParameterTypes()));
        return JavaErlang.map_to_erlang(result);
    }

    OtpErlangObject call_method(final OtpErlangObject cmd) throws Exception {
        // System.err.println("cmd="+cmd+"\n");
        final OtpErlangTuple t = (OtpErlangTuple) cmd;
        final Object fun = JavaErlang.get_fun(t.elementAt(1));
        final OtpErlangObject[] args = ((OtpErlangTuple) t.elementAt(2))
                .elements();
        Object result;

        final Method method = (Method) fun;
        final OtpErlangObject otpObj = t.elementAt(0);
        final Object obj = JavaErlang.java_value_from_erlang(otpObj);
        final Object[] translated_args = JavaErlang.java_values_from_erlang(
                args, method.getParameterTypes());
        // System.err.println("method="+method+" obj="+obj);
        result = method.invoke(obj, translated_args);
        return JavaErlang.map_to_erlang(result, method.getReturnType());
    }
}

class OtpStatusHandler extends OtpNodeStatus {
    String connectedNode = null;

    @Override
    public void remoteStatus(final String node, final boolean up,
            final Object info) {
        if (JavaErlang.verbose) {
            System.err.println("Event at node " + node + " with " + up
                    + "; info=" + info);
        }

        if (connectedNode == null && up) {
            connectedNode = node;
        } else if (connectedNode != null && !up && node.equals(connectedNode)) {
            if (JavaErlang.verbose) {
                System.err.println("Erlang peer node "
                        + JavaErlang.nodeIdentifier + " died; terminating...");
            }
            System.exit(0);
        }
    }
}

class RefEqualsObject {
    Object object;

    public RefEqualsObject(final Object object) {
        this.object = object;
    }

    @Override
    public boolean equals(final Object object2) {
        if (object2 instanceof RefEqualsObject) {
            return object == ((RefEqualsObject) object2).object();
        } else {
            return this == object2;
        }
    }

    @Override
    public int hashCode() {
        if (object != null) {
            return object.hashCode();
        } else {
            return 0;
        }
    }

    public Object object() {
        return object;
    }
}

class IntKey {
    int key;

    public IntKey(final int previousCounter) {
        key = previousCounter;
    }

    @Override
    public String toString() {
        return new Integer(key).toString();
    }

    public int key() {
        return key;
    }

    @Override
    public boolean equals(final Object k) {
        if (k instanceof IntKey) {
            final IntKey otherKey = (IntKey) k;
            return key == otherKey.key();
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return key;
    }
}

class InvHandler implements InvocationHandler {
    OtpErlangPid pid;
    Method hashCode;
    IntKey key;
    Object backingObject;
    volatile Object answer;

    public InvHandler(final OtpErlangPid pid, final Object backingObject) {
        this.pid = pid;
        this.backingObject = backingObject;
        key = new IntKey(JavaErlang.objCounter++);
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
            otpArg = JavaErlang.map_to_erlang_null();
        } else {
            final OtpErlangObject[] otpArgs = new OtpErlangObject[args.length];
            for (int i = 0; i < args.length; i++) {
                otpArgs[i] = JavaErlang.map_to_erlang(args[i]);
            }
            otpArg = new OtpErlangTuple(otpArgs);
        }

        final OtpErlangObject proxy_msg = JavaErlang.makeErlangTuple(
                new OtpErlangAtom("proxy_msg"),
                JavaErlang.map_to_erlang(proxy),
                JavaErlang.acc_map_to_erlang(method), otpArg);
        if (JavaErlang.verbose) {
            System.err.println("Sending proxy reply " + proxy_msg + " to "
                    + pid);
        }
        JavaErlang.msgs.send(pid, proxy_msg);

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
        return JavaErlang.java_value_from_erlang(myAnswer);
    }
}
