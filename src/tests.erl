-module(tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
 
  
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

print_exception(F) ->
  try F()
  catch {java_exception,Exc} ->
      java:print_stacktrace(Exc)
  end.

tc() ->
  io:format("Starting tc~n",[]),
  {ok,NodeId} = java:start_node([{java_verbose,false}]),
  I = (java:module(NodeId,'java.lang.Integer')):'Integer'(NodeId,2),
  io:format("Obtained an integer ~p~n",[I]),
  Cl = (java:module(I)):getClass(I),
  io:format("Which is of class ~p~n",[Cl]),
  CanonicalType = java:string_to_list((java:module(Cl)):getCanonicalName(Cl)),
  io:format("The canonical type is ~p~n", [CanonicalType]),
  CanonicalType.

tc1() ->
  {ok,NodeId} = java:start_node(),
  H = java:new(NodeId,'java.util.HashSet',[]),
  io:format("got a hashset ~p~n",[H]),
  I = java:new(NodeId,'java.lang.Integer',[2]),
  io:format("got an integer ~p~n",[I]),
  Boolean = java_util_HashSet:add(H,I),
  io:format("the result is ~p~n",[Boolean]),
  I2 = java_lang_Integer:'Integer'(NodeId,3),
  io:format("we have an integer 3 = ~p~n",[I2]),
  Boolean2 = java_util_HashSet:add(H,I2),
  io:format("the result is ~p~n",[Boolean2]),
  I3 = java_lang_Integer:'Integer'(NodeId,2),
  io:format("we have an integer 2 = ~p~n",[I3]),
  Boolean3 = java_util_HashSet:add(H,I3),
  io:format("the result is ~p~n",[Boolean3]),
  do_loop(1,1000,H,NodeId),
  Size = java_util_HashSet:size(H),
  io:format
    ("after loop~nnumber of elements is ~p~n",
     [Size]),
  Size.

do_loop(N,N,_,_) -> ok;
do_loop(J,N,H,NodeId) ->
  java_util_HashSet:add(H,java_lang_Integer:'Integer'(NodeId,J)),
  do_loop(J+1,N,H,NodeId).

tc2() ->
  {ok,NodeId} = java:start_node(),
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

tc35() ->
  {ok,NodeId} = java:start_node(),
  java:acquire_class(NodeId,'java.io.PrintStream'),
  ok.

tc4() ->
  {ok,NodeId} = java:start_node(),
  I = java:new(NodeId,'java.lang.Integer',[2]),
  IString = java_lang_Integer:toString(I),
  io:format("result is ~p~n",[java:string_to_list(IString)]),
  AL = java:new(NodeId,'java.util.ArrayList',[]),
  io:format("new arraylist~n",[]),
  java_util_ArrayList:add(AL,I),
  java_util_ArrayList:add(AL,I),
  java_util_ArrayList:add(AL,I),
  io:format("added elements~n",[]),
  Array = java_util_ArrayList:toArray(AL),
  io:format("array is ~p~n",[java:array_to_list(Array)]),
  Obj = java:call_static(java:node_id(Array),'java.util.Arrays',asList,[Array]),
  io:format("Obj is ~p~n",[Obj]),
  ClassNameString = java:getClassName(Obj),
  io:format("Class name is ~p~n",[ClassNameString]),
  Size = java:call(Obj,size,[]),
  io:format("Size is ~p~n",[Size]),
  Size.

tc5() ->
  {ok,NodeId} = java:start_node(),
  _I = java:new(NodeId,'java.lang.Integer',[2]),
  io:format("I...~n",[]),
  ok.

tc6() ->
  {ok,NodeId} = java:start_node(),
  I = java:new(NodeId,'java.lang.Integer',[2]),
  IS = java_lang_Integer:toString(I),
  J = java_lang_Integer:'Integer'(NodeId,IS),
  JS = java_lang_Integer:toString(J),
  io:format
    ("I=~p J=~p and they are equal==~p~n",
     [java:string_to_list(IS),
      java:string_to_list(JS),
      java_lang_Integer:equals(I,J)]),
  java_lang_Integer:equals(I,J).

tc7() ->
  {ok,NodeId} = java:start_node(),
  I = java:new(NodeId,'java.lang.Integer',[2]),
  IS = java_lang_Integer:toString(I),
  io:format("after I and IS~n",[]),
  Err = java:get_static(NodeId,'java.lang.System',err),
  io:format("we have java.lang.System.err~n",[]),
  java:call(Err,println,[IS]),
  ok.

tc8() ->
  {ok,NodeId} = java:start_node([]),
  Obj = java:new(NodeId,'javaErlang.testing.Test',[]),
  java:call(Obj,print,[]),
  io:format("value is ~p~n",[java:get(Obj,v)]),
  java:set(Obj,v,1),
  io:format("value is ~p~n",[java:get(Obj,v)]),
  I = java:new(NodeId,'java.lang.Integer',[4]),
  java:call(Obj,print,[]),
  io:format
    ("The value of the integer is ~p~n",
     [java_lang_Integer:intValue(I)]),
  java_lang_Integer:intValue(I).

tc9() ->
  {ok,NodeId1} = java:start_node([]),
  {ok,NodeId2} = java:start_node([{mangle_classnames,true}]),
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
  {ok,NodeId1} = java:start_node([{mangle_classnames,true}]),
  {ok,NodeId2} = java:start_node([{mangle_classnames,true}]),
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
  java:acquire_class(NodeId,'java.lang.Integer'),
  Cnst = java_lang_Integer:constructor(NodeId,[int]),
  I = Cnst(3),
  J = Cnst(3),
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
  java:acquire_class(NodeId,'java.lang.Character'),
  java:acquire_class(NodeId,'java.lang.String'),
  Space = java_lang_Character:'Character'(NodeId,32),
  SpaceStr = java:call(Space,toString,[]),
  true = string:equal(java:string_to_list(SpaceStr)," "),
  Hello = "Hello World!",
  CharArray = java:list_to_array(NodeId,Hello,char),
  Cl = java:call(CharArray,getClass,[]),
  true =
    string:equal
      (java:string_to_list(java:call(Cl,getCanonicalName,[])),
       "char[]"),
  CharStr = java_lang_String:'String'(NodeId,CharArray),
  true = string:equal(java:string_to_list(CharStr),Hello),
  true = string:equal(java:array_to_list(CharArray),Hello),
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

tc14() ->
  {ok,NodeId} = java:start_node([{java_verbose,false}]),
  False = java:new(NodeId,'java.lang.Boolean',[false]),
  false == java:call(False,booleanValue,[]).

tc15() ->
  {ok,N} = java:start_node(),
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

 tc17() ->
  {ok,N} = java:start_node(),
  Zero = java:new(N,'java.lang.Integer',[0]),
  true = java:call(Zero,equals,[0]),
  false = java:call(Zero,equals,[2]),
  false == java:call(Zero,equals,[0.0]).

set_get_report(Parent,Value) ->
  {ok,N} = java:start_node(),
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
  Int10 = java_lang_Integer:'Integer'(NodeId,10),
  String10 = java_lang_Integer:toString(Int10),
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
  java:acquire_class(NodeId,'java.lang.Integer'),
  I2 = java_lang_Integer:'Integer'(NodeId,2),
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

tc20() ->
  {ok,N} = java:start_node(),
  H = java:new(N,'java.util.HashSet',[]),
  I = java:new(N,'java.lang.Integer',[0]),
  tc20l(20000,H,I).

tc20l(0,H,I) ->
  java:call(H,size,[]);
tc20l(N,H,I) ->
  java:call(H,add,[I]),
  tc20l(N-1,H,I).

