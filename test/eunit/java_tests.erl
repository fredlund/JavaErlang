-module(java_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

start() ->
    case node() of
        nonode@nohost ->
            _ = os:cmd("epmd -daemon"),
            {ok, _} = net_kernel:start([?MODULE, shortnames]),
            ok;
        _ ->
            ok
    end.

stop(_) ->
    todo.

node_test_() ->
    {setup, fun start/0, fun stop/1, fun all_tests/1}.

node_gc_test_() ->
    {setup, fun start/0, fun stop/1, fun all_gc_tests/1}.

all_tests(_) ->
    [?_test(test0()),
     ?_test(test1()),
     ?_test(test2()),
     ?_test(test3()),
     ?_test(test35()),
     ?_test(test4()),
     ?_test(test5()),
     ?_test(test6()),
     ?_test(test7()),
     ?_test(test8()),
     ?_test(test9()),
     ?_test(test10()),
     ?_test(test11()),
     ?_test(test12()),
     ?_test(test13()),
     ?_test(test14()),
     ?_test(test15()),
     ?_test(test16()),
     ?_test(test17()),
     ?_test(test18()),
     ?_test(test19()),
     ?_test(test20()),
     ?_test(test21()),
     ?_test(test22())
    ].

all_gc_tests(_) ->
    [?_test(test0gc()),
     ?_test(test1gc()),
     ?_test(test2gc()),
     ?_test(test3gc()),
     ?_test(test35gc()),
     ?_test(test4gc()),
     ?_test(test5gc()),
     ?_test(test6gc()),
     ?_test(test7gc()),
     ?_test(test8gc()),
     ?_test(test9gc()),
     ?_test(test10gc()),
     ?_test(test11gc()),
     ?_test(test12gc()),
     ?_test(test13gc()),
     ?_test(test14gc()),
     ?_test(test15gc()),
     ?_test(test16gc()),
     ?_test(test17gc()),
     ?_test(test18gc()),
     ?_test(test19gc()),
     ?_test(test20gc()),
     ?_test(test21gc()),
     ?_test(test22gc())
    ].

%% Without gc

test0() ->
    ?assertEqual("java.lang.Integer",print_exception(fun () -> tc() end)).
test1() ->
    ?assertEqual(999,print_exception(fun () -> tc1() end)).
test2() ->
    ?assertEqual(ok,print_exception(fun () -> tc2() end)).
test3() ->
    ?assertEqual(353,print_exception(fun () -> tc3() end)).
test35() ->
    ?assertEqual(ok,print_exception(fun () -> tc35() end)).
test4() ->
    ?assertEqual(3,print_exception(fun () -> tc4() end)).
test5() ->
    ?assertEqual(ok,print_exception(fun () -> tc5() end)).
test6() ->
    ?assertEqual(true,print_exception(fun () -> tc6() end)).
test7() ->
    ?assertEqual(ok,print_exception(fun () -> tc7() end)).
test8() ->
    ?assertEqual(4,print_exception(fun () -> tc8() end)).
test9() ->
    ?assertEqual(0,print_exception(fun () -> tc9() end)).
test10() ->
    ?assertEqual(0,print_exception(fun () -> tc10() end)).
test11() ->
    ?assertEqual(true,print_exception(fun () -> tc11() end)).
test12() ->
    ?assertEqual(true,print_exception(fun () -> tc12() end)).
test13() ->
    ?assertEqual(true,print_exception(fun () -> tc13() end)).
test14() ->
    ?assertEqual(true,print_exception(fun () -> tc14() end)).
test15() ->
    ?assertEqual(true,print_exception(fun () -> tc15() end)).
test16() ->
    ?assertEqual(true,print_exception(fun () -> tc16() end)).
test17() ->
    ?assertEqual(true,print_exception(fun () -> tc17() end)).
test18() ->
    ?assertEqual(true,print_exception(fun () -> tc18() end)).
test19() ->
    ?assertEqual(true,print_exception(fun () -> tc19() end)).
test20() ->
    ?assertEqual(1,print_exception(fun () -> tc20() end)).
test21() ->
    ?assertEqual(ok,print_exception(fun () -> tc21() end)).
test22() ->
    ?assertEqual(ok,print_exception(fun () -> tc22() end)).

%% With gc

test0gc() ->
    ?assertEqual("java.lang.Integer",print_exception(fun () -> tc_gc() end)).
test1gc() ->
    ?assertEqual(999,print_exception(fun () -> tc1_gc() end)).
test2gc() ->
    ?assertEqual(ok,print_exception(fun () -> tc2_gc() end)).
test3gc() ->
    ?assertEqual(353,print_exception(fun () -> tc3_gc() end)).
test35gc() ->
    ?assertEqual(ok,print_exception(fun () -> tc35_gc() end)).
test4gc() ->
    ?assertEqual(3,print_exception(fun () -> tc4_gc() end)).
test5gc() ->
    ?assertEqual(ok,print_exception(fun () -> tc5_gc() end)).
test6gc() ->
    ?assertEqual(true,print_exception(fun () -> tc6_gc() end)).
test7gc() ->
    ?assertEqual(ok,print_exception(fun () -> tc7_gc() end)).
test8gc() ->
    ?assertEqual(4,print_exception(fun () -> tc8_gc() end)).
test9gc() ->
    ?assertEqual(0,print_exception(fun () -> tc9_gc() end)).
test10gc() ->
    ?assertEqual(0,print_exception(fun () -> tc10_gc() end)).
test11gc() ->
    ?assertEqual(true,print_exception(fun () -> tc11_gc() end)).
test12gc() ->
    ?assertEqual(true,print_exception(fun () -> tc12_gc() end)).
test13gc() ->
    ?assertEqual(true,print_exception(fun () -> tc13_gc() end)).
test14gc() ->
    ?assertEqual(true,print_exception(fun () -> tc14_gc() end)).
test15gc() ->
    ?assertEqual(true,print_exception(fun () -> tc15_gc() end)).
test16gc() ->
    ?assertEqual(true,print_exception(fun () -> tc16_gc() end)).
test17gc() ->
    ?assertEqual(true,print_exception(fun () -> tc17_gc() end)).
test18gc() ->
    ?assertEqual(true,print_exception(fun () -> tc18_gc() end)).
test19gc() ->
    ?assertEqual(true,print_exception(fun () -> tc19_gc() end)).
test20gc() ->
    ?assertEqual(1,print_exception(fun () -> tc20_gc() end)).
test21gc() ->
    ?assertEqual(ok,print_exception(fun () -> tc21_gc() end)).
test22gc() ->
    ?assertEqual(ok,print_exception(fun () -> tc22_gc() end)).

tc() ->
    io:format("Starting tc~n",[]),
    {ok,NodeId} = java:start_node(),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("Obtained an integer ~p~n",[I]),
    Cl = java:call(I,getClass,[]),
    io:format("Which is of class ~p~n",[Cl]),
    CanonicalType = java:string_to_list(java:call(Cl,getCanonicalName,[])),
    io:format("The canonical type is ~p~n", [CanonicalType]),
    CanonicalType.

tc_gc() ->
    io:format("Starting tc~n",[]),
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("Obtained an integer ~p~n",[I]),
    Cl = java:call(I,getClass,[]),
    io:format("Which is of class ~p~n",[Cl]),
    CanonicalType = java:string_to_list(java:call(Cl,getCanonicalName,[])),
    io:format("The canonical type is ~p~n", [CanonicalType]),
    CanonicalType.

tc1() ->
    {ok,NodeId} = java:start_node(),
    H = java:new(NodeId,'java.util.HashSet',[]),
    io:format("got a hashset ~p~n",[H]),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("got an integer ~p~n",[I]),
    Boolean = java:call(H,add,[I]),
    io:format("the result is ~p~n",[Boolean]),
    I2 = java:new(NodeId,'java.lang.Integer',[3]),
    io:format("we have an integer 3 = ~p~n",[I2]),
    Boolean2 = java:call(H,add,[I2]),
    io:format("the result is ~p~n",[Boolean2]),
    I3 = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("we have an integer 2 = ~p~n",[I3]),
    Boolean3 = java:call(H,add,[I3]),
    io:format("the result is ~p~n",[Boolean3]),
    do_loop(1,1000,H,NodeId),
    Size = java:call(H,size,[]),
    io:format
      ("after loop~nnumber of elements is ~p~n",
       [Size]),
    Size.

tc1_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    H = java:new(NodeId,'java.util.HashSet',[]),
    io:format("got a hashset ~p~n",[H]),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("got an integer ~p~n",[I]),
    Boolean = java:call(H,add,[I]),
    io:format("the result is ~p~n",[Boolean]),
    I2 = java:new(NodeId,'java.lang.Integer',[3]),
    io:format("we have an integer 3 = ~p~n",[I2]),
    Boolean2 = java:call(H,add,[I2]),
    io:format("the result is ~p~n",[Boolean2]),
    I3 = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("we have an integer 2 = ~p~n",[I3]),
    Boolean3 = java:call(H,add,[I3]),
    io:format("the result is ~p~n",[Boolean3]),
    do_loop(1,1000,H,NodeId),
    Size = java:call(H,size,[]),
    io:format
      ("after loop~nnumber of elements is ~p~n",
       [Size]),
    Size.

tc2() ->
    {ok,NodeId} = java:start_node(),
    java:new(NodeId,'java.lang.Integer',[2]),
    ok.

tc2_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    java:new(NodeId,'java.lang.Integer',[2]),
    ok.

tc3() ->
    {ok,NodeId} = java:start_node(),
    Object = java:new(NodeId,'java.lang.Integer',[int],[353]),
    Value = java:call(Object,intValue,[]),
    io:format
      ("Java returned value ~p; whoohoop~n",
       [Value]),
    Value2 =
        java:call_static(NodeId,'java.lang.Integer',reverseBytes,[Value]),
    io:format
      ("Reversing bytes yields ~p~n",
       [Value2]),
    java:call_static(NodeId,'java.lang.Integer',reverseBytes,[Value2]).

tc3_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    Object = java:new(NodeId,'java.lang.Integer',[int],[353]),
    Value = java:call(Object,intValue,[]),
    io:format
      ("Java returned value ~p; whoohoop~n",
       [Value]),
    Value2 =
        java:call_static(NodeId,'java.lang.Integer',reverseBytes,[Value]),
    io:format
      ("Reversing bytes yields ~p~n",
       [Value2]),
    java:call_static(NodeId,'java.lang.Integer',reverseBytes,[Value2]).

tc35() ->
    {ok,NodeId} = java:start_node(),
    java:acquire_class(NodeId,'java.io.PrintStream'),
    ok.

tc35_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    java:acquire_class(NodeId,'java.io.PrintStream'),
    ok.

tc4() ->
    {ok,NodeId} = java:start_node(),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    IString = java:call(I,toString,[]),
    io:format("result is ~p~n",[java:string_to_list(IString)]),
    AL = java:new(NodeId,'java.util.ArrayList',[]),
    io:format("new arraylist~n",[]),
    java:call(AL,add,[I]),
    java:call(AL,add,[I]),
    java:call(AL,add,[I]),
    io:format("added elements~n",[]),
    Array = java:call(AL,toArray,[]),
    io:format("array is ~p~n",[java:array_to_list(Array)]),
    Obj = java:call_static(java:node_id(Array),'java.util.Arrays',asList,[Array]),
    io:format("Obj is ~p~n",[Obj]),
    ClassNameString = java:getClassName(Obj),
    io:format("Class name is ~p~n",[ClassNameString]),
    Size = java:call(Obj,size,[]),
    io:format("Size is ~p~n",[Size]),
    Size.

tc4_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    IString = java:call(I,toString,[]),
    io:format("result is ~p~n",[java:string_to_list(IString)]),
    AL = java:new(NodeId,'java.util.ArrayList',[]),
    io:format("new arraylist~n",[]),
    java:call(AL,add,[I]),
    java:call(AL,add,[I]),
    java:call(AL,add,[I]),
    io:format("added elements~n",[]),
    Array = java:call(AL,toArray,[]),
    io:format("array is ~p~n",[java:array_to_list(Array)]),
    Obj = java:call_static(java:node_id(Array),'java.util.Arrays',asList,[Array]),
    io:format("Obj is ~p~n",[Obj]),
    ClassNameString = java:getClassName(Obj),
    io:format("Class name is ~p~n",[ClassNameString]),
    Size = java:call(Obj,size,[]),
    io:format("Size is ~p~n",[Size]),
    java:terminate(NodeId),
    Size.

tc5() ->
    {ok,NodeId} = java:start_node(),
    _I = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("I...~n",[]),
    ok.

tc5_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    _I = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("I...~n",[]),
    java:terminate(NodeId),
    ok.

tc6() ->
    {ok,NodeId} = java:start_node(),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    IS = java:call(I,toString,[]),
    J = java:new(NodeId,'java.lang.Integer',[IS]),
    JS = java:call(J,toString,[]),
    io:format
      ("I=~p J=~p and they are equal==~p~n",
       [java:string_to_list(IS),
        java:string_to_list(JS),
        java:call(I,equals,[J])]),
    java:call(I,equals,[J]).

tc6_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    IS = java:call(I,toString,[]),
    J = java:new(NodeId,'java.lang.Integer',[IS]),
    JS = java:call(J,toString,[]),
    io:format
      ("I=~p J=~p and they are equal==~p~n",
       [java:string_to_list(IS),
        java:string_to_list(JS),
        java:call(I,equals,[J])]),
    java:call(I,equals,[J]).

tc7() ->
    {ok,NodeId} = java:start_node(),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    IS = java:call(I,toString,[]),
    io:format("after I and IS~n",[]),
    Err = java:get_static(NodeId,'java.lang.System',err),
    io:format("we have java.lang.System.err~n",[]),
    java:call(Err,println,[IS]),
    ok.

tc7_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    IS = java:call(I,toString,[]),
    io:format("after I and IS~n",[]),
    Err = java:get_static(NodeId,'java.lang.System',err),
    io:format("we have java.lang.System.err~n",[]),
    java:call(Err,println,[IS]),
    ok.

tc8() ->
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["classes"]}]),
    Obj = java:new(NodeId,'javaErlang.testing.Test',[]),
    java:call(Obj,print,[]),
    io:format("value is ~p~n",[java:get(Obj,v)]),
    java:set(Obj,v,1),
    io:format("value is ~p~n",[java:get(Obj,v)]),
    I = java:new(NodeId,'java.lang.Integer',[4]),
    java:call(Obj,print,[]),
    io:format
      ("The value of the integer is ~p~n",
       [java:call(I,intValue,[])]),
    java:call(I,intValue,[]).

tc8_gc() ->
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["classes"]},{enable_gc,true}]),
    Obj = java:new(NodeId,'javaErlang.testing.Test',[]),
    java:call(Obj,print,[]),
    io:format("value is ~p~n",[java:get(Obj,v)]),
    java:set(Obj,v,1),
    io:format("value is ~p~n",[java:get(Obj,v)]),
    I = java:new(NodeId,'java.lang.Integer',[4]),
    java:call(Obj,print,[]),
    io:format
      ("The value of the integer is ~p~n",
       [java:call(I,intValue,[])]),
    java:call(I,intValue,[]).

tc9() ->
    {ok,NodeId1} = java:start_node([{add_to_java_classpath,["classes"]}]),
    {ok,NodeId2} = java:start_node([{add_to_java_classpath,["classes"]}]),
    Obj1 = java:new(NodeId1,'javaErlang.testing.Test',[]),
    Obj2 = java:new(NodeId2,'javaErlang.testing.Test',[]),
    java:call(Obj1,print,[]),
    java:call(Obj2,print,[]),
    io:format("2: value is ~p~n",[java:get(Obj2,v)]),
    java:set(Obj1,v,1),
    io:format("1: value is ~p~n",[java:get(Obj1,v)]),
    java:set(Obj2,v,1),
    io:format("2: value is ~p~n",[java:get(Obj2,v)]),
    io:format("1: value is ~p~n",[java:get(Obj1,v)]),
    java:call(Obj2,print,[]),
    java:call(Obj1,print,[]),
    java:get(Obj2,v)-java:get(Obj1,v).

tc9_gc() ->
    {ok,NodeId1} = java:start_node([{add_to_java_classpath,["classes"]},{enable_gc,true}]),
    {ok,NodeId2} = java:start_node([{add_to_java_classpath,["classes"]}]),
    Obj1 = java:new(NodeId1,'javaErlang.testing.Test',[]),
    Obj2 = java:new(NodeId2,'javaErlang.testing.Test',[]),
    java:call(Obj1,print,[]),
    java:call(Obj2,print,[]),
    io:format("2: value is ~p~n",[java:get(Obj2,v)]),
    java:set(Obj1,v,1),
    io:format("1: value is ~p~n",[java:get(Obj1,v)]),
    java:set(Obj2,v,1),
    io:format("2: value is ~p~n",[java:get(Obj2,v)]),
    io:format("1: value is ~p~n",[java:get(Obj1,v)]),
    java:call(Obj2,print,[]),
    java:call(Obj1,print,[]),
    java:get(Obj2,v)-java:get(Obj1,v).

tc10() ->
    {ok,NodeId1} = java:start_node([{add_to_java_classpath,["classes"]}]),
    {ok,NodeId2} = java:start_node([{add_to_java_classpath,["classes"]}]),
    Obj1 = java:new(NodeId1,'javaErlang.testing.Test',[]),
    Obj2 = java:new(NodeId2,'javaErlang.testing.Test',[]),
    java:call(Obj1,print,[]),
    java:call(Obj2,print,[]),
    io:format("2: value is ~p~n",[java:get(Obj2,v)]),
    java:set(Obj1,v,1),
    io:format("1: value is ~p~n",[java:get(Obj1,v)]),
    java:set(Obj2,v,1),
    io:format("2: value is ~p~n",[java:get(Obj2,v)]),
    io:format("1: value is ~p~n",[java:get(Obj1,v)]),
    java:call(Obj2,print,[]),
    java:call(Obj1,print,[]),
    java:get(Obj2,v)-java:get(Obj1,v).

tc10_gc() ->
    {ok,NodeId1} = java:start_node([{add_to_java_classpath,["classes"]},{enable_gc,true}]),
    {ok,NodeId2} = java:start_node([{add_to_java_classpath,["classes"]}]),
    Obj1 = java:new(NodeId1,'javaErlang.testing.Test',[]),
    Obj2 = java:new(NodeId2,'javaErlang.testing.Test',[]),
    java:call(Obj1,print,[]),
    java:call(Obj2,print,[]),
    io:format("2: value is ~p~n",[java:get(Obj2,v)]),
    java:set(Obj1,v,1),
    io:format("1: value is ~p~n",[java:get(Obj1,v)]),
    java:set(Obj2,v,1),
    io:format("2: value is ~p~n",[java:get(Obj2,v)]),
    io:format("1: value is ~p~n",[java:get(Obj1,v)]),
    java:call(Obj2,print,[]),
    java:call(Obj1,print,[]),
    java:get(Obj2,v)-java:get(Obj1,v).

tc11() ->
    {ok,NodeId} = java:start_node(),
    I = java:new(NodeId,'java.lang.Integer',[int],[3]),
    J = java:new(NodeId,'java.lang.Integer',[int],[3]),
    io:format
      ("3=~p~n",
       [java:string_to_list(java:call(I,toString,[]))]),
    io:format
      ("hashcode(3)=~p~n",
       [java:call(I,hashCode,[])]),
    io:format
      ("3 equals 3==~p~n",
       [java:call(I,equals,[J])]),
    java:call(I,equals,[J]).

tc11_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    I = java:new(NodeId,'java.lang.Integer',[int],[3]),
    J = java:new(NodeId,'java.lang.Integer',[int],[3]),
    io:format
      ("3=~p~n",
       [java:string_to_list(java:call(I,toString,[]))]),
    io:format
      ("hashcode(3)=~p~n",
       [java:call(I,hashCode,[])]),
    io:format
      ("3 equals 3==~p~n",
       [java:call(I,equals,[J])]),
    java:call(I,equals,[J]).

tc12() ->
    {ok,NodeId} = java:start_node(),
    Space = java:new(NodeId,'java.lang.Character',[32]),
    SpaceStr = java:call(Space,toString,[]),
    true = string:equal(java:string_to_list(SpaceStr)," "),
    Hello = "Hello World!",
    CharArray = java:list_to_array(NodeId,Hello,char),
    Cl = java:call(CharArray,getClass,[]),
    true =
        string:equal
          (java:string_to_list(java:call(Cl,getCanonicalName,[])),
           "char[]"),
    CharStr = java:new(NodeId,'java.lang.String',[CharArray]),
    true = string:equal(java:string_to_list(CharStr),Hello),
    true = string:equal(java:array_to_list(CharArray),Hello),
    io:format("finishing...~n",[]),
    true.

tc12_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    Space = java:new(NodeId,'java.lang.Character',[32]),
    SpaceStr = java:call(Space,toString,[]),
    true = string:equal(java:string_to_list(SpaceStr)," "),
    Hello = "Hello World!",
    CharArray = java:list_to_array(NodeId,Hello,char),
    Cl = java:call(CharArray,getClass,[]),
    true =
        string:equal
          (java:string_to_list(java:call(Cl,getCanonicalName,[])),
           "char[]"),
    CharStr = java:new(NodeId,'java.lang.String',[CharArray]),
    true = string:equal(java:string_to_list(CharStr),Hello),
    true = string:equal(java:array_to_list(CharArray),Hello),
    java:terminate(NodeId),
    io:format("finishing...~n",[]),
    true.

tc13() ->
    {ok,NodeId} = java:start_node(),
    try
        java:new(NodeId,'hola',[]),
        false
    catch {java_exception,Exc} ->
            io:format("Exception (~p) is of type ~p~n",[Exc,java:getClassName(Exc)]),
            Err = java:get_static(NodeId,'java.lang.System',err),
            io:format("Err (~p) is of type ~p~n",[Err,java:getClassName(Err)]),
            io:format("Exception stack trace:~n",[]),
            java:call(Exc,printStackTrace,[Err]),
            io:format("~n",[]),
            true
    end.

tc13_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    try
        java:new(NodeId,'hola',[]),
        false
    catch {java_exception,Exc} ->
            io:format("Exception (~p) is of type ~p~n",[Exc,java:getClassName(Exc)]),
            Err = java:get_static(NodeId,'java.lang.System',err),
            io:format("Err (~p) is of type ~p~n",[Err,java:getClassName(Err)]),
            io:format("Exception stack trace:~n",[]),
            java:call(Exc,printStackTrace,[Err]),
            io:format("~n",[]),
            true
    end.

tc14() ->
    {ok,NodeId} = java:start_node([]),
    False = java:new(NodeId,'java.lang.Boolean',[false]),
    false == java:call(False,booleanValue,[]).

tc14_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    False = java:new(NodeId,'java.lang.Boolean',[false]),
    false == java:call(False,booleanValue,[]).

tc15() ->
    {ok,N} = java:start_node(),
    java:call_static(N,'java.lang.String',valueOf,[2]),
    java:call_static(N,'java.lang.String',valueOf,[{char,2}]),
    java:call_static(N,'java.lang.String',valueOf,[{int,2}]),
    true.

tc15_gc() ->
    {ok,N} = java:start_node([{enable_gc,true}]),
    java:call_static(N,'java.lang.String',valueOf,[2]),
    java:call_static(N,'java.lang.String',valueOf,[{char,2}]),
    java:call_static(N,'java.lang.String',valueOf,[{int,2}]),
    true.

tc16() ->
    {ok,N} = java:start_node(),
    S = java:new(N,'java.lang.String',[{{array,char,1},"Hello World"}]),
    true = string:equal("Hello World",java:string_to_list(S)),
    S2 = java:new(N,'java.lang.String',["Hello World!"]),
    true == string:equal("Hello World!",java:string_to_list(S2)).

tc16_gc() ->
    {ok,N} = java:start_node([{enable_gc,true}]),
    S = java:new(N,'java.lang.String',[{{array,char,1},"Hello World"}]),
    true = string:equal("Hello World",java:string_to_list(S)),
    S2 = java:new(N,'java.lang.String',["Hello World!"]),
    true == string:equal("Hello World!",java:string_to_list(S2)).

tc17() ->
    {ok,N} = java:start_node(),
    Zero = java:new(N,'java.lang.Integer',[0]),
    true = java:call(Zero,equals,[0]),
    false = java:call(Zero,equals,[2]),
    false == java:call(Zero,equals,[0.0]).

tc17_gc() ->
    {ok,N} = java:start_node([{enable_gc,true}]),
    Zero = java:new(N,'java.lang.Integer',[0]),
    true = java:call(Zero,equals,[0]),
    false = java:call(Zero,equals,[2]),
    false == java:call(Zero,equals,[0.0]).

set_get_report(Parent,Value) ->
    {ok,N} = java:start_node([{add_to_java_classpath,["classes"]}]),
    Parent!{node,N},
    Obj = java:new(N,'javaErlang.testing.Test',[]),
    timer:sleep(rand:uniform(2)*1000),
    java:set(Obj,v,Value),
    timer:sleep(rand:uniform(2)*1000),
    Parent!{value,(java:get(Obj,v))}.

tc18() ->
    Self = self(),
    spawn(fun () -> set_get_report(Self,1) end),
    receive {node,_} -> ok end,
    spawn(fun () -> set_get_report(Self,3) end),
    {V1,V2} =
        receive
            {value,X} -> {X,receive {value,Y} -> Y end}
        end,
    receive {node,_} -> ok end,
    (V1==1 andalso V2==3) orelse (V1==3 andalso V2==1).

set_get_report_gc(Parent,Value) ->
    {ok,N} = java:start_node([{add_to_java_classpath,["classes"]},{enable_gc,true}]),
    Parent!{node,N},
    Obj = java:new(N,'javaErlang.testing.Test',[]),
    timer:sleep(rand:uniform(2)*1000),
    java:set(Obj,v,Value),
    timer:sleep(rand:uniform(2)*1000),
    Parent!{value,(java:get(Obj,v))}.

tc18_gc() ->
    Self = self(),
    spawn(fun () -> set_get_report_gc(Self,1) end),
    receive {node,_} -> ok end,
    spawn(fun () -> set_get_report_gc(Self,3) end),
    {V1,V2} =
        receive
            {value,X} -> {X,receive {value,Y} -> Y end}
        end,
    receive {node,_} -> ok end,
    (V1==1 andalso V2==3) orelse (V1==3 andalso V2==3).

tc19() ->
    true = tc19a(),
    true = tc19b(),
    true = tc19c(),
    true = tc19d(),
    true = tc19e(),
    true = tc19f(),
    true = tc19g(),
    true = tc19h(),
    true = tc19i(),
    true = tc19j().

tc19a() ->
    {ok,NodeId} = java:start_node(),
    java:acquire_class(NodeId,'java.lang.Integer'),
    Int10 = java:new(NodeId,'java.lang.Integer',[10]),
    _String10 = java:call(Int10,toString,[]),
    true.

tc19b() ->
    {ok,NodeId} = java:start_node(),
    Int10 = java:new(NodeId,'java.lang.Integer',[10]),
    _String10 = java:call(Int10,toString,[]),
    true.

tc19c() ->
    {ok,NodeId} = java:start_node(),
    _False = java:new(NodeId,'java.lang.Boolean',[false]),
    _HelloWorldString = java:new(NodeId,'java.lang.String',[java:list_to_array(NodeId,"Hello World!",char)]),
    Zero = java:new(NodeId,'java.lang.Integer',[0]),
    0 = java:call(Zero,intValue,[]),
    "Hello World" = java:string_to_list(java:new(NodeId,'java.lang.String',[{{array,char,1},"Hello World"}])),
    "Hello World" = java:string_to_list(java:new(NodeId,'java.lang.String',["Hello World"])),
    true.

tc19d() ->
    {ok,NodeId} = java:start_node(),
    Zero = java:new(NodeId,'java.lang.Integer',[0]),
    true = java:call(Zero,equals,[0]),
    false = java:call(Zero,equals,[2]),
    false = java:call(Zero,equals,[0.0]),
    true.

tc19e() ->
    {ok,N} = java:start_node(),
    Zero = java:new(N,'java.lang.Integer',[0]),
    0 = java:call(Zero,intValue,[]),
    true.

tc19f() ->
    {ok,NodeId} = java:start_node(),
    try java:new(NodeId,'hola',[]), false
    catch {java_exception,Exc} ->
            io:format("Exception is of type ~p~n",[java:getClassName(Exc)]),
            java:print_stacktrace(Exc),
            true
    end.

tc19g() ->
    {ok,NodeId} = java:start_node(),
    _I2 = java:new(NodeId,'java.lang.Integer',[2]),
    true.

tc19h() ->
    {ok,NodeId} = java:start_node(),
    _I2 = java:new(NodeId,'java.lang.Integer',[2]),
    true.

tc19i() ->
    {ok,NodeId} = java:start_node(),
    I2 = java:new(NodeId,'java.lang.Integer',[2]),
    I2b = java:new(NodeId,'java.lang.Integer',[2]),
    true = java:call(I2,equals,[I2b]),
    true.

tc19j() ->
    {ok,NodeId} = java:start_node(),
    Err = java:get_static(NodeId,'java.lang.System',err),
    java:call(Err,println,[{int,2}]),
    true.

tc19_gc() ->
    true = tc19a_gc(),
    true = tc19b_gc(),
    true = tc19c_gc(),
    true = tc19d_gc(),
    true = tc19e_gc(),
    true = tc19f_gc(),
    true = tc19g_gc(),
    true = tc19h_gc(),
    true = tc19i_gc(),
    true = tc19j_gc().

tc19a_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    java:acquire_class(NodeId,'java.lang.Integer'),
    Int10 = java:new(NodeId,'java.lang.Integer',[10]),
    _String10 = java:call(Int10,toString,[]),
    true.

tc19b_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    Int10 = java:new(NodeId,'java.lang.Integer',[10]),
    _String10 = java:call(Int10,toString,[]),
    true.

tc19c_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    _False = java:new(NodeId,'java.lang.Boolean',[false]),
    _HelloWorldString = java:new(NodeId,'java.lang.String',[java:list_to_array(NodeId,"Hello World!",char)]),
    Zero = java:new(NodeId,'java.lang.Integer',[0]),
    0 = java:call(Zero,intValue,[]),
    "Hello World" = java:string_to_list(java:new(NodeId,'java.lang.String',[{{array,char,1},"Hello World"}])),
    "Hello World" = java:string_to_list(java:new(NodeId,'java.lang.String',["Hello World"])),
    true.

tc19d_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    Zero = java:new(NodeId,'java.lang.Integer',[0]),
    true = java:call(Zero,equals,[0]),
    false = java:call(Zero,equals,[2]),
    false = java:call(Zero,equals,[0.0]),
    true.

tc19e_gc() ->
    {ok,N} = java:start_node([{enable_gc,true}]),
    Zero = java:new(N,'java.lang.Integer',[0]),
    0 = java:call(Zero,intValue,[]),
    true.

tc19f_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    try java:new(NodeId,'hola',[]), false
    catch {java_exception,Exc} ->
            io:format("Exception is of type ~p~n",[java:getClassName(Exc)]),
            java:print_stacktrace(Exc),
            true
    end.

tc19g_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    _I2 = java:new(NodeId,'java.lang.Integer',[2]),
    true.

tc19h_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    _I2 = java:new(NodeId,'java.lang.Integer',[2]),
    true.

tc19i_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    I2 = java:new(NodeId,'java.lang.Integer',[2]),
    I2b = java:new(NodeId,'java.lang.Integer',[2]),
    true = java:call(I2,equals,[I2b]),
    true.

tc19j_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    Err = java:get_static(NodeId,'java.lang.System',err),
    java:call(Err,println,[{int,2}]),
    true.

tc20() ->
    {ok,N} = java:start_node([]),
    H = java:new(N,'java.util.HashSet',[]),
    I = java:new(N,'java.lang.Integer',[0]),
    tc20l(20000,H,I).

tc20_gc() ->
    {ok,N} = java:start_node([{enable_gc,true}]),
    H = java:new(N,'java.util.HashSet',[]),
    I = java:new(N,'java.lang.Integer',[0]),
    tc20l(20000,H,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_exception(F) ->
    try F()
    catch {java_exception,Exc} ->
            java:print_stacktrace(Exc)
    end.

do_loop(N,N,_,_) -> ok;
do_loop(J,N,H,NodeId) ->
    java:call(H,add,[java:new(NodeId,'java.lang.Integer',[J])]),
    do_loop(J+1,N,H,NodeId).

tc20l(0,H,_I) ->
    java:call(H,size,[]);
tc20l(N,H,I) ->
    java:call(H,add,[I]),
    tc20l(N-1,H,I).

tc21() ->
    {ok,NodeId} = java:start_node(),
    lists:foreach
      (fun (_) ->
               spawn
                 (fun () ->
                          timer:sleep(100+rand:uniform(100)),
                          java:new(NodeId,'java.lang.Integer',[2]),
                          timer:sleep(100+rand:uniform(100))
                  end)
       end,
       lists:duplicate(100,10)),
    ok.

tc21_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    lists:foreach
      (fun (_) ->
               spawn
                 (fun () ->
                          timer:sleep(100+rand:uniform(100)),
                          java:new(NodeId,'java.lang.Integer',[2]),
                          timer:sleep(100+rand:uniform(100))
                  end)
       end,
       lists:duplicate(100,10)),
    ok.

tc22() ->
    {ok,NodeId} = java:start_node(),
    Self = self(),
    lists:foreach
      (fun (_) ->
               spawn
                 (fun () ->
                          timer:sleep(100+rand:uniform(100)),
                          java:new(NodeId,'java.lang.Integer',[2]),
                          Self!ok,
                          timer:sleep(300+rand:uniform(100))
                  end)
       end,
       lists:duplicate(100,10)),
    count(100),
    java:terminate(NodeId),
    ok.

tc22_gc() ->
    {ok,NodeId} = java:start_node([{enable_gc,true}]),
    Self = self(),
    lists:foreach
      (fun (_) ->
               spawn
                 (fun () ->
                          timer:sleep(100+rand:uniform(100)),
                          java:new(NodeId,'java.lang.Integer',[2]),
                          Self!ok,
                          timer:sleep(300+rand:uniform(100))
                  end)
       end,
       lists:duplicate(100,10)),
    count(100),
    java:terminate(NodeId),
    ok.

count(0) ->
    ok;
count(N) ->
    receive
        ok -> count(N-1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

runtests() ->
    tc(),
    tc1(),
    tc2(),
    tc3(),
    tc35(),
    tc4(),
    tc5(),
    tc6(),
    tc7(),
    tc8(),
    tc9(),
    tc10(),
    tc11(),
    tc12(),
    tc13(),
    tc14(),
    tc15(),
    tc16(),
    tc17(),
    tc18(),
    tc19(),
    tc20(),
    tc21(),
    tc22(),

    tc_gc(),
    tc1_gc(),
    tc2_gc(),
    tc3_gc(),
    tc35_gc(),
    tc4_gc(),
    tc5_gc(),
    tc6_gc(),
    tc7_gc(),
    tc8_gc(),
    tc9_gc(),
    tc10_gc(),
    tc11_gc(),
    tc12_gc(),
    tc13_gc(),
    tc14_gc(),
    tc15_gc(),
    tc16_gc(),
    tc17_gc(),
    tc18_gc(),
    tc19_gc(),
    tc20_gc(),
    tc21_gc(),
    tc22_gc().


