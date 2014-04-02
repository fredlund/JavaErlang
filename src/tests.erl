-module(tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
 

%% Without gc 
  
test0_test() ->
  ?assertEqual("java.lang.Integer",print_exception(fun () -> tc() end)).
test1_test() ->
  ?assertEqual(999,print_exception(fun () -> tc1() end)).
test2_test() ->
  ?assertEqual(ok,print_exception(fun () -> tc2() end)).
test3_test() ->
  ?assertEqual(353,print_exception(fun () -> tc3() end)).
test35_test() ->
  ?assertEqual(ok,print_exception(fun () -> tc35() end)).
test4_test() ->
  ?assertEqual(3,print_exception(fun () -> tc4() end)).
test5_test() ->
  ?assertEqual(ok,print_exception(fun () -> tc5() end)).
test6_test() ->
  ?assertEqual(true,print_exception(fun () -> tc6() end)).
test7_test() ->
  ?assertEqual(ok,print_exception(fun () -> tc7() end)).
test8_test() ->
  ?assertEqual(4,print_exception(fun () -> tc8() end)).
test9_test() ->
  ?assertEqual(0,print_exception(fun () -> tc9() end)).
test10_test() ->
  ?assertEqual(0,print_exception(fun () -> tc10() end)).
test11_test() ->
  ?assertEqual(true,print_exception(fun () -> tc11() end)).
test12_test() ->
  ?assertEqual(true,print_exception(fun () -> tc12() end)).
test13_test() ->
  ?assertEqual(true,print_exception(fun () -> tc13() end)).
test14_test() ->
  ?assertEqual(true,print_exception(fun () -> tc14() end)).
test15_test() ->
  ?assertEqual(true,print_exception(fun () -> tc15() end)).
test16_test() ->
  ?assertEqual(true,print_exception(fun () -> tc16() end)).
test17_test() ->
  ?assertEqual(true,print_exception(fun () -> tc17() end)).
test18_test() ->
  ?assertEqual(true,print_exception(fun () -> tc18() end)).
test19_test() ->
  ?assertEqual(true,print_exception(fun () -> tc19() end)).
test20_test() ->
  ?assertEqual(1,print_exception(fun () -> tc20() end)).

%% With gc 

test0gc_test() ->
  ?assertEqual("java.lang.Integer",print_exception(fun () -> tc_gc() end)).
test1gc_test() ->
  ?assertEqual(999,print_exception(fun () -> tc1_gc() end)).
test2gc_test() ->
  ?assertEqual(ok,print_exception(fun () -> tc2_gc() end)).
test3gc_test() ->
  ?assertEqual(353,print_exception(fun () -> tc3_gc() end)).
test35gc_test() ->
  ?assertEqual(ok,print_exception(fun () -> tc35_gc() end)).
test4gc_test() ->
  ?assertEqual(3,print_exception(fun () -> tc4_gc() end)).
test5gc_test() ->
  ?assertEqual(ok,print_exception(fun () -> tc5_gc() end)).
test6gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc6_gc() end)).
test7gc_test() ->
  ?assertEqual(ok,print_exception(fun () -> tc7_gc() end)).
test8gc_test() ->
  ?assertEqual(4,print_exception(fun () -> tc8_gc() end)).
test9gc_test() ->
  ?assertEqual(0,print_exception(fun () -> tc9_gc() end)).
test10gc_test() ->
  ?assertEqual(0,print_exception(fun () -> tc10_gc() end)).
test11gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc11_gc() end)).
test12gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc12_gc() end)).
test13gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc13_gc() end)).
test14gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc14_gc() end)).
test15gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc15_gc() end)).
test16gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc16_gc() end)).
test17gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc17_gc() end)).
test18gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc18_gc() end)).
test19gc_test() ->
  ?assertEqual(true,print_exception(fun () -> tc19_gc() end)).
test20gc_test() ->
  ?assertEqual(1,print_exception(fun () -> tc20_gc() end)).

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
  timer:sleep(random:uniform(2)*1000),
  java:set(Obj,v,Value),
  timer:sleep(random:uniform(2)*1000),
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
  (V1==1 andalso V2==3) orelse (V1==3 andalso V2==3).

set_get_report_gc(Parent,Value) ->
  {ok,N} = java:start_node([{add_to_java_classpath,["classes"]},{enable_gc,true}]),
  Parent!{node,N},
  Obj = java:new(N,'javaErlang.testing.Test',[]),
  timer:sleep(random:uniform(2)*1000),
  java:set(Obj,v,Value),
  timer:sleep(random:uniform(2)*1000),
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
  String10 = java:call(Int10,toString,[]),
  true.

tc19b() ->
  {ok,NodeId} = java:start_node(),
  Int10 = java:new(NodeId,'java.lang.Integer',[10]),
  String10 = java:call(Int10,toString,[]),
  true.

tc19c() ->
  {ok,NodeId} = java:start_node(),
  False = java:new(NodeId,'java.lang.Boolean',[false]),
  HelloWorldString = java:new(NodeId,'java.lang.String',[java:list_to_array(NodeId,"Hello World!",char)]),
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
  I2 = java:new(NodeId,'java.lang.Integer',[2]),
  true.

tc19h() ->
  {ok,NodeId} = java:start_node(),
  I2 = java:new(NodeId,'java.lang.Integer',[2]),
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
  String10 = java:call(Int10,toString,[]),
  true.

tc19b_gc() ->
  {ok,NodeId} = java:start_node([{enable_gc,true}]),
  Int10 = java:new(NodeId,'java.lang.Integer',[10]),
  String10 = java:call(Int10,toString,[]),
  true.

tc19c_gc() ->
  {ok,NodeId} = java:start_node([{enable_gc,true}]),
  False = java:new(NodeId,'java.lang.Boolean',[false]),
  HelloWorldString = java:new(NodeId,'java.lang.String',[java:list_to_array(NodeId,"Hello World!",char)]),
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
  I2 = java:new(NodeId,'java.lang.Integer',[2]),
  true.

tc19h_gc() ->
  {ok,NodeId} = java:start_node([{enable_gc,true}]),
  I2 = java:new(NodeId,'java.lang.Integer',[2]),
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

tc20l(0,H,I) ->
  java:call(H,size,[]);
tc20l(N,H,I) ->
  java:call(H,add,[I]),
  tc20l(N-1,H,I).


