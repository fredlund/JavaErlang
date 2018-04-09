-module(java_tests).
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
    {setup, fun start/0, fun stop/1, {inorder, all_tests([])}}.
node_gc_test_() ->
    {setup, fun start/0, fun stop/1, {inorder, all_tests([{enable_gc,true}])}}.

all_tests(Options) ->
  [
   ?_assertEqual("java.lang.Integer",print_exception(fun () -> tc(Options) end))
  ,?_assertEqual(999,print_exception(fun () -> tc1(Options) end))
  ,?_assertEqual(ok,print_exception(fun () -> tc2(Options) end))
  ,?_assertEqual(353,print_exception(fun () -> tc3(Options) end))
  ,?_assertEqual(ok,print_exception(fun () -> tc35(Options) end))
  ,?_assertEqual(3,print_exception(fun () -> tc4(Options) end))
  ,?_assertEqual(ok,print_exception(fun () -> tc5(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc6(Options) end))
  ,?_assertEqual(ok,print_exception(fun () -> tc7(Options) end))
  ,?_assertEqual(4,print_exception(fun () -> tc8(Options) end))
  ,?_assertEqual(0,print_exception(fun () -> tc9(Options) end))
  ,?_assertEqual(0,print_exception(fun () -> tc10(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc11(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc12(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc13(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc14(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc15(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc16(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc17(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc18(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc19(Options) end))
  ,?_assertEqual(1,print_exception(fun () -> tc20(Options) end))
  ,?_assertEqual(ok,print_exception(fun () -> tc21(Options) end))
  ,?_assertEqual(ok,print_exception(fun () -> tc22(Options) end))
  ,?_assertEqual(0,print_exception(fun () -> tc23(Options) end))
  ,?_assertEqual(1,print_exception(fun () -> tc24(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc25(Options) end))
  ,?_assertEqual(true,print_exception(fun () -> tc26(Options) end))
  ].

tc(Options) ->
    io:format("Starting tc~n",[]),
    {ok,NodeId} = java:start_node(Options),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("Obtained an integer ~p~n",[I]),
    Cl = java:call(I,getClass,[]),
    io:format("Which is of class ~p~n",[Cl]),
    CanonicalType = java:string_to_list(java:call(Cl,getCanonicalName,[])),
    io:format("The canonical type is ~p~n", [CanonicalType]),
    CanonicalType.

tc1(Options) ->
    {ok,NodeId} = java:start_node(Options),
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

tc2(Options) ->
    {ok,NodeId} = java:start_node(Options),
    java:new(NodeId,'java.lang.Integer',[2]),
    ok.

tc3(Options) ->
    {ok,NodeId} = java:start_node(Options),
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

tc35(Options) ->
    {ok,NodeId} = java:start_node(Options),
    java:acquire_class(NodeId,'java.io.PrintStream'),
    ok.

tc4(Options) ->
    {ok,NodeId} = java:start_node(Options),
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

tc5(Options) ->
    {ok,NodeId} = java:start_node(Options),
    _I = java:new(NodeId,'java.lang.Integer',[2]),
    io:format("I...~n",[]),
    ok.

tc6(Options) ->
    {ok,NodeId} = java:start_node(Options),
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

tc7(Options) ->
    {ok,NodeId} = java:start_node(Options),
    I = java:new(NodeId,'java.lang.Integer',[2]),
    IS = java:call(I,toString,[]),
    io:format("after I and IS~n",[]),
    Err = java:get_static(NodeId,'java.lang.System',err),
    io:format("we have java.lang.System.err~n",[]),
    java:call(Err,println,[IS]),
    ok.

tc8(Options) ->
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["classes"]}|Options]),
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

tc9(Options) ->
    {ok,NodeId1} = java:start_node([{add_to_java_classpath,["classes"]}|Options]),
    {ok,NodeId2} = java:start_node([{add_to_java_classpath,["classes"]}|Options]),
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

tc10(Options) ->
    {ok,NodeId1} = java:start_node([{add_to_java_classpath,["classes"]}|Options]),
    {ok,NodeId2} = java:start_node([{add_to_java_classpath,["classes"]}|Options]),
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

tc11(Options) ->
    {ok,NodeId} = java:start_node(Options),
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

tc12(Options) ->
    {ok,NodeId} = java:start_node(Options),
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


tc13(Options) ->
    {ok,NodeId} = java:start_node(Options),
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

tc14(Options) ->
    {ok,NodeId} = java:start_node(Options),
    False = java:new(NodeId,'java.lang.Boolean',[false]),
    false == java:call(False,booleanValue,[]).

tc15(Options) ->
    {ok,N} = java:start_node(Options),
    java:call_static(N,'java.lang.String',valueOf,[2]),
    java:call_static(N,'java.lang.String',valueOf,[{char,2}]),
    java:call_static(N,'java.lang.String',valueOf,[{int,2}]),
    true.

tc16(Options) ->
    {ok,N} = java:start_node(Options),
    S = java:new(N,'java.lang.String',[{{array,char,1},"Hello World"}]),
    true = string:equal("Hello World",java:string_to_list(S)),
    S2 = java:new(N,'java.lang.String',["Hello World!"]),
    true == string:equal("Hello World!",java:string_to_list(S2)).

tc17(Options) ->
    {ok,N} = java:start_node(Options),
    Zero = java:new(N,'java.lang.Integer',[0]),
    true = java:call(Zero,equals,[0]),
    false = java:call(Zero,equals,[2]),
    false == java:call(Zero,equals,[0.0]).

set_get_report(Parent,Value,Options) ->
    {ok,N} = java:start_node([{add_to_java_classpath,["classes"]}|Options]),
    Parent!{node,N},
    Obj = java:new(N,'javaErlang.testing.Test',[]),
    timer:sleep(rand:uniform(2)*1000),
    java:set(Obj,v,Value),
    timer:sleep(rand:uniform(2)*1000),
    Parent!{value,(java:get(Obj,v))}.

tc18(Options) ->
    Self = self(),
    spawn(fun () -> set_get_report(Self,1,Options) end),
    receive {node,_} -> ok end,
    spawn(fun () -> set_get_report(Self,3,Options) end),
    {V1,V2} =
        receive
            {value,X} -> {X,receive {value,Y} -> Y end}
        end,
    receive {node,_} -> ok end,
    (V1==1 andalso V2==3) orelse (V1==3 andalso V2==1).

tc19(Options) ->
    true = tc19a(Options),
    true = tc19b(Options),
    true = tc19c(Options),
    true = tc19d(Options),
    true = tc19e(Options),
    true = tc19f(Options),
    true = tc19g(Options),
    true = tc19h(Options),
    true = tc19i(Options),
    true = tc19j(Options).

tc19a(Options) ->
    {ok,NodeId} = java:start_node(Options),
    java:acquire_class(NodeId,'java.lang.Integer'),
    Int10 = java:new(NodeId,'java.lang.Integer',[10]),
    _String10 = java:call(Int10,toString,[]),
    true.

tc19b(Options) ->
    {ok,NodeId} = java:start_node(Options),
    Int10 = java:new(NodeId,'java.lang.Integer',[10]),
    _String10 = java:call(Int10,toString,[]),
    true.

tc19c(Options) ->
    {ok,NodeId} = java:start_node(Options),
    _False = java:new(NodeId,'java.lang.Boolean',[false]),
    _HelloWorldString = java:new(NodeId,'java.lang.String',[java:list_to_array(NodeId,"Hello World!",char)]),
    Zero = java:new(NodeId,'java.lang.Integer',[0]),
    0 = java:call(Zero,intValue,[]),
    "Hello World" = java:string_to_list(java:new(NodeId,'java.lang.String',[{{array,char,1},"Hello World"}])),
    "Hello World" = java:string_to_list(java:new(NodeId,'java.lang.String',["Hello World"])),
    true.

tc19d(Options) ->
    {ok,NodeId} = java:start_node(Options),
    Zero = java:new(NodeId,'java.lang.Integer',[0]),
    true = java:call(Zero,equals,[0]),
    false = java:call(Zero,equals,[2]),
    false = java:call(Zero,equals,[0.0]),
    true.

tc19e(Options) ->
    {ok,N} = java:start_node(Options),
    Zero = java:new(N,'java.lang.Integer',[0]),
    0 = java:call(Zero,intValue,[]),
    true.

tc19f(Options) ->
    {ok,NodeId} = java:start_node(Options),
    try java:new(NodeId,'hola',[]), false
    catch {java_exception,Exc} ->
            io:format("Exception is of type ~p~n",[java:getClassName(Exc)]),
            java:print_stacktrace(Exc),
            true
    end.

tc19g(Options) ->
    {ok,NodeId} = java:start_node(Options),
    _I2 = java:new(NodeId,'java.lang.Integer',[2]),
    true.

tc19h(Options) ->
    {ok,NodeId} = java:start_node(Options),
    _I2 = java:new(NodeId,'java.lang.Integer',[2]),
    true.

tc19i(Options) ->
    {ok,NodeId} = java:start_node(Options),
    I2 = java:new(NodeId,'java.lang.Integer',[2]),
    I2b = java:new(NodeId,'java.lang.Integer',[2]),
    true = java:call(I2,equals,[I2b]),
    true.

tc19j(Options) ->
    {ok,NodeId} = java:start_node(Options),
    Err = java:get_static(NodeId,'java.lang.System',err),
    java:call(Err,println,[{int,2}]),
    true.

tc20(Options) ->
    {ok,N} = java:start_node(Options),
    H = java:new(N,'java.util.HashSet',[]),
    I = java:new(N,'java.lang.Integer',[0]),
    tc20l(20000,H,I).

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

tc21(Options) ->
    {ok,NodeId} = java:start_node(Options),
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


tc22(Options) ->
    {ok,NodeId} = java:start_node(Options),
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

tc23(Options) ->
  {ok,NodeId} = java:start_node([{add_to_java_classpath,["classes"]},{enter_classes,['javaErlang.testing.Test']}|Options]),
  Obj = java:new(NodeId,'javaErlang.testing.Test',[]),
  java:call(Obj,value,[]).

tc24(Options) ->
  {ok,NodeId} = java:start_node([{add_to_java_classpath,["classes"]},{enter_classes,['javaErlang.testing.Test']}|Options]),
  Obj = java:new(NodeId,'javaErlang.testing.Test',[]),
  java:get(Obj,x).

tc25(Options) ->
  {ok,NodeId} = java:start_node(Options),
  java:string_to_list
    (java:call_static
       (NodeId,
	'java.util.Arrays',toString,[{{array,'int',2},[[1],[2]]}])),
  true.

tc26(Options) ->
  {ok,NodeId} = java:start_node(Options),
  java:string_to_list
    (java:call_static
       (NodeId,
	'java.util.Arrays',toString,[{{array,'int',1},[]}])),
  true.

count(0) ->
    ok;
count(N) ->
    receive
        ok -> count(N-1)
    end.



