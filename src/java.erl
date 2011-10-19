%% Copyright (c) 2011, Lars-Ake Fredlund
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc This module implements a facility for invoking Java code
%% (methods, constructors, fields) from Erlang, using the JInterface library.
%% @author Lars-Ake Fredlund (lfredlund@fi.upm.es)
%% @copyright 2011 Lars-Ake Fredlund
%%

%% New features?
%%
%% - Supporting synchronized? This possibly means to lock, and synchronize
%% on a variable, a particular thread in JavaErlang until it is unlocked
%% (possible on the level of JNI).
%%
%% - Permit calling constructors and methods on non-public classes, and
%% non-public constructors and methods of public classes.
%%
%% - If a field is final, don't generate a setter function.
%%

-module(java).


-include_lib("kernel/include/file.hrl").

-record(node,
	{node_name=void,node_pid=void,node_id=void,options,symbolic_name,
	 unix_pid=void,ping_retry=5000,connect_timeout=1000,
	 max_java_start_tries=3,call_timeout,num_start_tries=0}).

-include("classinfo.hrl").

-record(class,{node,constructors,methods,get_fields,set_fields,name,module_name,class_location}).


-export([init/1]).
-export([start_node/0,start_node/1,nodes/0,symbolic_name/1]).
-export([default_options/0,version/0]).
-export([free/1,reset/1,terminate/1,terminate_all/0]).
-export([brutally_terminate/1,recreate_node/1]).
-export([module/1,module/2,acquire_class/2,node_id/1]).
-export([new/3,new/4]).
-export([call/3,call/4,call_static/4,call_static/5]).
-export([set_timeout/1]).
-export([get/2,get_static/3,set/3,set_static/4]).
-export([is_object_ref/1]).
-export([array_to_list/1,string_to_list/1,list_to_string/2,list_to_array/3,convert/3]).
-export([getClassName/1,getSimpleClassName/1,instanceof/2,is_subtype/3]).
-export([print_stacktrace/1]).

-export([set_loglevel/1,format/2,format/3]).

-export_type([node_id/0,object_ref/0]).

%% Private
-export([javaCall/3,type_compatible_alternatives/3]).
-export([get_class_info/3,get_option/2]).
-export([classname/2,to_erl_name/1,finalComponent/1]).
-export([class_info/2]).
-export([constructor/3,method/4,method_obj_and_fun/4,field_get/3,field_set/3]).
-export([node_lookup/1]).

-include("debug.hrl").

-type loglevel() ::
  all | none | 
  alert | critical | debug | emergency | error | info | notice | warning.

-type option() ::
  {java_sources,string()} 
    | {symbolic_name,string()}
    | {java_beams,string()}
    | {java_class,string()}
    | {add_to_java_classpath,[string()]}
    | {java_classpath,[string()]}
    | {java_exception_as_value,boolean()}
    | {java_verbose,boolean()}
    | {java_executable,string()}
    | {mangle_classnames,boolean()}
    | {log_level,loglevel()}
    | {call_timeout,integer() | infinity}.
%% <ul>
%% <li>`java_sources' determines the directory
%% where generated Erlang modules
%% corresponding to Java classes are stored
%% (default value "java_sources").</li>
%% <li>`symbolic_name' provides a symbolic name for the node.</li>
%% <li>`java_beams' determines the directory where beam files generated from
%% Erlang modules corresponding to Java classes are stored 
%% (default value "java_sources/ebin").</li>
%% <li>`java_classpath' provides a classpath to the Java executable.
%% The default classpath includes the OtpErlang.jar library, and
%% the Java class files needed by the JavaErl library.</li>
%% <li>`add_to_java_classpath' adds additional entries to an
%% existing classpath established by java_classpath.</li>
%% <li>`java_exception_as_value' determines whether exceptions
%% generated by a Java runtime is delivered as a tuple
%% "{java_exception,Object}" or as an Erlang exception
%% with the above tuple as cause.</li>
%% <li>`java_executable' determines which program will be used
%% to start the Java interpreter (by default "java").</li>
%% <li>`java_verbose' provides diagnostic output from the 
%% Java interface class (default false).</li>
%% <li>`call_timeout' sets a timeout value for all calls 
%% to Java from Erlang (default 10 seconds).</li>
%% <li>`mangle_classname' is an option which permits 
%% talking to multiple Java nodes,
%% that have incompatible but identically named Java classes.
%% This options should not normally be set to true (default false).</li>
%% </ul>
 
-opaque node_id() :: integer().
%%-type node_id() :: integer().
%% Identifies a connected Java node.

%% Likely to change.
-opaque object_type() :: object | executable | thread.

-opaque object_ref() :: {object_type(), integer(), node_id()}.
%%-type object_ref() :: {atom(), integer(), node_id()}.
%% A Java object reference.

-type class_name() :: atom() | string().
%% A Java classname, e.g., the quoted atom 'java.lang.Integer'.

-type method_name() :: atom().
%% A name of a Java method, e.g., the atom 'toString'.

-type attribute_name() :: atom().
%% A name of a Java attribute, represented as an atom.

-type type() :: primitive_type() | class_name() | array_type().
%% The representation of a Java types as an Erlang term.

-type array_type() :: {array,type(),integer()}.

-type value() :: object_ref() | number() |
                 null | true | false | void | array_value() |
                 value_spec().

-type java_number() :: integer() | float().

-type value_spec() :: {int_type(), integer()} |
                      {float_type(), float()} |
                      {class_name, object_ref()} |
                      {array_type(), array_value()}.

-type array_value() :: string() | [value()].

-type primitive_type() :: int_type() | float_type().

-type number_type() :: int_type() | float_type().
-type int_type() :: int | long | short | char | byte .
-type float_type() :: float | double.


%% @doc Starts a Java node and establises the connection
%% to Erlang. Returns a Java library "node identifier" (not a normal
%% Erlang node identifier).
%%
-spec start_node() -> {ok,node_id()} | {error,any()}.
start_node() ->
  start_node([]).

%% @doc Starts a Java node and establishes the connection
%% to Erlang. UserOptions provides options for how
%% Java is started.
%% Returns a "Java library node identifier" (not a normal
%% Erlang node identifier).
%% To make your Java classes (and Jar files) visible to the library
%% the option ``add_to_java_classpath'' should be provided to
%% ``java:start_node/1''. An example:<br/>
%% ```
%% {ok,NodeId} = java:start_node([{add_to_java_classpath,["classes"]}]).
%% '''
%% Adds the directory ``classes''
%% to the classpath of the started Java interpreter.
%%
-spec start_node([option()]) -> {ok,node_id()} | {error,any()}.
start_node(UserOptions) ->
  case whereis(net_kernel) of
    undefined ->
      format
	(error,
	 "*** Error: net_kernel system process is not running.~n"++
	 "Make sure to start erlang using \"erl -sname nodename ...\"~nor "++
	 "call net_kernel:start/1~n~n"),
      throw(net_kernel_undefined);
    _ ->
      ok
  end,
  case ets:info(java_nodes) of
    undefined -> init([]);
    _ -> ok
  end,
  [{_,StandardOptions}] = ets:lookup(java_nodes,options),
  Options = UserOptions++StandardOptions,
  check_options(Options),
  CallTimeout = proplists:get_value(call_timeout,Options),
  PreNode = #node{options=Options,call_timeout=CallTimeout},
  spawn_java(PreNode,get_java_node_id()).
  

spawn_java(PreNode,PreNodeId) ->
  if PreNode#node.num_start_tries>=PreNode#node.max_java_start_tries ->
      format(error,"*** Error: failed to start Java~n"),
      {error,too_many_tries};
     true ->
      NodeId = PreNodeId+99,
      Options = PreNode#node.options,
      JavaVerbose = proplists:get_value(java_verbose,Options),
      ClassPath = compute_classpath(Options),
      spawn
	(fun () ->
	     run_java
	       (NodeId,
		proplists:get_value(java_executable,Options),
		JavaVerbose,ClassPath,
		proplists:get_value(java_class,Options))
	 end),
      NodeName = javaNodeName(NodeId),
      SymbolicName = proplists:get_value(symbolic_name,Options,NodeName),
      PreNode1 =
	PreNode#node{node_id=NodeId,node_name=NodeName,
		     symbolic_name=SymbolicName},
      case connectToNode(PreNode1) of
	{ok,Node} ->
	  java:format
	    (debug,"Connect succeeded with pid ~p~n",[Node#node.node_pid]),
	  node_store(Node),
	  java:format
	    (debug,"Fresh connection to ~p established~n",[NodeId]),
	  {ok,NodeId};
	{error,Reason} ->
	  java:format
	    (debug,
	     "Failed to connect at try ~p with reason ~p~n",
	     [PreNode1#node.num_start_tries,Reason]),
	  spawn_java
	    (PreNode1#node{num_start_tries=PreNode1#node.num_start_tries+1},
	     NodeId)
      end
  end.

compute_classpath(Options) ->
  ClassPath =
    proplists:get_value(java_classpath,Options),
  AllAdditionals =
    proplists:get_all_values(add_to_java_classpath,Options),
  lists:foldl(fun (CPs,CP) -> CPs++CP end, ClassPath, AllAdditionals).

check_options(Options) ->
  lists:foreach
    (fun (Option) ->
	 OptionName = 
	   case Option of
	     {Name,_} when is_atom(Name) -> Name;
	     Name when is_atom(Name) -> Name
	   end,
	 case lists:member
	   (OptionName,
	    [java_sources,symbolic_name,log_level,
	     java_beams,java_class,java_classpath,add_to_java_classpath,
	     java_exception_as_value,java_verbose,
	     java_executable,
	     call_timeout,mangle_classnames]) of
	   true -> ok;
	   false ->
	     format
	       (error,
		"*** error: option ~p to java:start_node/2 not understood~n",
		[OptionName]),
	     throw(badarg)
	 end
     end, Options).

%% @private
get_option(Option,NodeId) ->
  {ok,Node} = node_lookup(NodeId),
  proplists:get_value(Option,Node#node.options).

get_option(Option,NodeId,Default) ->
  {ok,Node} = node_lookup(NodeId),
  proplists:get_value(Option,Node#node.options,Default).


get_java_node_id() ->
  case ets:lookup(java_nodes,java_node_id) of
    [] ->
      {A1,A2,A3} = now(),
      random:seed(A1,A2,A3),
      Value = random:uniform(100000),
      ets:insert(java_nodes,{java_node_id,Value+1}), Value;
    [{_,Value}] ->
      ets:insert(java_nodes,{java_node_id,Value+1}), Value
  end.

run_java(Identity,Executable,Verbose,Paths,Class) ->
  ClassPath = 
    case combine_paths(Paths) of
      "" -> [];
      PathSpec -> ["-cp",PathSpec]
    end,
  VerboseArg = if Verbose -> ["-verbose"]; true -> [] end,
  Args = ClassPath++[Class,integer_to_list(Identity)]++VerboseArg,
  format
    (info,
     "Starting Java node with command~n~s and args ~p~n",
     [Executable,Args]),
  Port = open_port({spawn_executable,Executable},[{line,1000},stderr_to_stdout,{args,Args}]),
  java_reader(Port,Identity).

combine_paths(Paths) ->
  Combinator = 
    case runs_on_windows() of
      true -> ";";
      _ -> ":"
    end,
  combine_paths(Combinator,Paths).

combine_paths(_,[]) ->  "";
combine_paths(_,[P]) ->  P;
combine_paths(Combinator,[P|Rest]) -> P++Combinator++combine_paths(Rest).

java_reader(Port,Identity) ->
  receive
    {_,{data,{eol,Message}}} ->
      io:format("~s~n",[Message]),
      java_reader(Port,Identity);
    {_,{data,{noeol,Message}}} ->
      io:format("~s~n",[Message]),
      java_reader(Port,Identity);
    Other ->
      format
	(warning,
	 "java_reader ~p got strange message~n  ~p~n",[Identity,Other]),
      java_reader(Port,Identity)
  end.

connectToNode(Node) ->
  connectToNode
    (Node,
     addTimeStamps(erlang:now(),milliSecondsToTimeStamp(Node#node.ping_retry))).

connectToNode(PreNode,KeepOnTryingUntil) ->
  NodeName = PreNode#node.node_name,
  case net_adm:ping(NodeName) of
    pong ->
      java:format(debug,"Connected to Java node ~p~n",[NodeName]),
      {javaNode,NodeName}!{connect,PreNode#node.node_id,self()},
      receive
	{value,{connected,Pid,UnixPid}} when is_pid(Pid) ->
	  java:format(debug,"Got Java pid ~p~n",[Pid]),
	  Node = PreNode#node{node_pid=Pid,unix_pid=UnixPid},
	  {ok,Node};
	{value,already_connected} ->
	  %% Oops. We are talking to an old Java node...
	  %% We should try to start another one...
	  {error,already_connected};
	Other ->
	  format
	    (warning,
	     "*** Warning: got reply ~p instead of a pid "++
	     "when trying to connect to node ~p~n",
	     [Other,{javaNode,NodeName}]),
	  connectToNode(PreNode,KeepOnTryingUntil)
      after PreNode#node.connect_timeout -> 
	  %% Failed to connect. We should try to start another node.
	  {error,connect_timeout}
      end;
    pang ->
      case compareTimes_ge(erlang:now(),KeepOnTryingUntil) of
	true -> 
	  format
	    (error,
	     "*** Error: failed trying to connect to Java node ~p~n",
	     [NodeName]),
	  {error,timeout};
	false ->
	  timer:sleep(100),
	  connectToNode(PreNode,KeepOnTryingUntil)
      end
  end.

compareTimes_ge({M1,S1,Mic1}, {M2,S2,Mic2}) ->
  M1 > M2
    orelse (M1 =:= M2 andalso S1 > S2)
    orelse (M1 =:= M2 andalso S1 =:= S2 andalso Mic1 >= Mic2).

milliSecondsToTimeStamp(MilliSeconds) ->
  Seconds = MilliSeconds div 1000,
  MegaSeconds = Seconds div 1000000,
  {MegaSeconds, Seconds rem 1000000, MilliSeconds rem 1000 * 1000}.

addTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  Mic=Mic1+Mic2,
  MicRem = Mic rem 1000000,
  MicDiv = Mic div 1000000,
  S = S1+S2+MicDiv,
  SRem = S rem 1000000,
  SDiv = S div 1000000,
  M = M1+M2+SDiv,
  {M,SRem,MicRem}.

javaNodeName(Identity) ->
  IdentityStr =
    integer_to_list(Identity),
  NodeStr =
    atom_to_list(node()),
  HostPart =
    string:substr(NodeStr,string:str(NodeStr,"@")),
  list_to_atom("javaNode_"++IdentityStr++HostPart).

%% @private
-spec javaCall(node_id(),atom(),any()) -> any().
javaCall(NodeId,Type,Msg) ->
  case node_lookup(NodeId) of
    {ok, Node} ->
      JavaMsg = create_msg(Type,Msg,Node),
      Node#node.node_pid!JavaMsg,
      Reply = wait_for_reply(Node),
      Reply;
    _ ->
      format(error,"javaCall: nodeId ~p not found~n",[NodeId]),
      format(error,"type: ~p message: ~p~n",[Type,Msg]),
      throw(javaCall)
  end.

create_msg(Type,Msg,Node) ->
  case msg_type(Type) of
    thread_msg -> 
      {Type,get_thread(Node),Msg,self()};
    _ ->
      {Type,Msg,self()}
  end.
    
msg_type(reset) -> non_thread_msg;
msg_type(terminate) -> non_thread_msg;
msg_type(connect) -> non_thread_msg;
msg_type(define_invocation_handler) -> non_thread_msg;
msg_type(proxy_reply) -> non_thread_msg;
msg_type(getConstructors) -> non_thread_msg;
msg_type(getClassLocation) -> non_thread_msg;
msg_type(getMethods) -> non_thread_msg;
msg_type(getClasses) -> non_thread_msg;
msg_type(getFields) -> non_thread_msg;
msg_type(getConstructor) -> non_thread_msg;
msg_type(getMethod) -> non_thread_msg;
msg_type(getField) -> non_thread_msg;
msg_type(objTypeCompat) -> non_thread_msg;
msg_type(createThread) -> non_thread_msg;
msg_type(stopThread) -> non_thread_msg;
msg_type(free) -> non_thread_msg;
msg_type(_) -> thread_msg.

wait_for_reply(Node) ->
  Timeout = get_timeout(Node),
  receive
    {'EXIT',_Pid,normal} ->
      wait_for_reply(Node);
    {value,Val} ->
      Val;
    _Exc={exception,ExceptionValue} ->
      case proplists:get_value(java_exception_as_value,Node#node.options,false) of
	true ->
	  {java_exception,ExceptionValue};
	false ->
	  throw({java_exception,ExceptionValue})
      end
%%    Other -> 
%%      io:format
%%	("~p(~p) at pid ~p~nstrange message ~p received~n",
%%	 [Node#node.symbolic_name,Node#node.node_id,self(),Other]),
%%      wait_for_reply(Node)
  after Timeout -> throw(java_timeout) 
  end.

create_thread(NodeId) ->
  javaCall(NodeId,createThread,0).

get_thread(Node) ->
  case ets:lookup(java_threads,{Node#node.node_id,self()}) of
    [{_,Thread}] -> 
      Thread;
    _ ->
      Thread = create_thread(Node#node.node_id),
      ets:insert(java_threads,{{Node#node.node_id,self()},Thread}),
      Thread
  end.

%% @doc
%% Calls the constructor of a Java class.
%% Returns an object reference.
%% <p>
%% Example: ``java:new(NodeId,'java.util.HashSet',[])'',
%% corresponding to the statement `new HashSet()'.
%% </p>
%% <p>
%% Due to the rules of Java method application (see explanation note in
%% module description)
%% it is possible that the correct constructor
%% for its arguments cannot be found. In that case,
%% `new/4' should be used intead.
%% </p>
-spec new(node_id(),class_name(),[value()]) -> object_ref().
new(NodeId,ClassName,Args) when is_list(Args) ->
  ?LOG("NodeId=~p ClassName=~p~n",[NodeId,ClassName]),
  Module = module(NodeId,ClassName),
  ?LOG("Module is ~p~n",[Module]),
  apply(Module,finalComponent(ClassName),[NodeId|Args]).

%% @doc
%% Calls the constructor of a Java class, explicitely selecting
%% a particular constructor.
%% Returns an object reference.
%% <p>
%% Example: 
%%     ``java:new(NodeId,'java.lang.Integer',[int],[42])'',
%% corresponding to the statement
%% `new Integer(42)'.
%% </p>
-spec new(node_id(),class_name(),[type()],[value()]) -> object_ref().
new(NodeId,ClassName,ArgTypes,Args) when is_list(Args) ->
  ?LOG("NodeId=~p ClassName=~p~n",[NodeId,ClassName]),
  Module = module(NodeId,ClassName),
  ?LOG("Module is ~p~n",[Module]),
  Constructor = Module:constructor(NodeId,ArgTypes),
  apply(Constructor,Args).

%% @doc
%% Calls a Java instance method.
%% Example: 
%%     ``java:call(Object,toString,[])'', 
%% corresponding to the call `Object.toString()'.
-spec call(object_ref(),method_name(),[value()]) -> value().
call(Object,Method,Args) when is_list(Args) ->
  if
    Object==null ->
      format
	(warning,
	 "*** Warning: calling method ~p~nwith arguments ~p~non null object~n",
	 [Method,Args]),
      throw(badarg);
    true ->
      apply(module(Object),Method,[Object|Args])
  end.

%% @doc
%% Calls a Java instance method, explicitely
%% selecting a particular method, using the type argument to
%% distinguish between methods of the same arity.
-spec call(object_ref(),method_name(),[type()],[value()]) -> value().
call(Object,Method,ArgTypes,Args) when is_list(Args) ->
  if
    Object==null ->
      format
	(warning,
	 "*** Warning: calling method ~p~nwith arguments ~p~non null object~n",
	 [Method,Args]),
      throw(badarg);
    true ->
      MethodFun = (module(Object)):method(Method,Object,ArgTypes),
      apply(MethodFun,Args)
  end.

%% @doc
%% Calls a Java static method (a class method).
%% Example:
%%     ``java:call_static(NodeId,'java.lang.Integer',reverseBytes,[22])'', 
%% corresponding to the call `Integer.reverseBytes(22)'.
-spec call_static(node_id(),class_name(),method_name(),[value()]) -> value().
call_static(NodeId,ClassName,Method,Args) when is_list(Args) ->
  apply(module(NodeId,ClassName),Method,[NodeId|Args]).

%% @doc
%% Calls a Java static method (a class method). Explicitely
%% selects which method to call using the types argument.
-spec call_static(node_id(),class_name(),method_name(),[type()],[value()]) -> value().
call_static(NodeId,ClassName,Method,ArgTypes,Args) when is_list(Args) ->
  MethodFun = (module(NodeId,ClassName)):method(Method,NodeId,ArgTypes),
  apply(MethodFun,Args).

%% @doc
%% Retrieves the value of an instance attribute.
%% Example: 
%% ``java:get(Object,v)', corresponding to 'Object.v''.
-spec get(object_ref(), attribute_name()) -> value().
get(Object,Field) ->
  if
    Object==null ->
      format
	(warning,
	 "*** Warning: get on field ~p~non null object~n",
	 [Field]),
      throw(badarg);
    true ->
      apply
	(module(Object),
	 field_get,
	 [Field,Object])
  end.

%% @doc
%% Retrieves the value of a class attribute.
%% Example: 
%% ``java:get_static(NodeId,'java.lang.Integer','SIZE')'', 
%% corresponding to `Integer.SIZE'.
-spec get_static(node_id(), class_name(), attribute_name()) -> value().
get_static(NodeId,ClassName,Field) ->
  apply(module(NodeId,ClassName),
	field_get,
	[Field,NodeId]).

%% @doc
%% Modifies the value of an instance attribute.
-spec set(object_ref(), attribute_name(), value()) -> value().
set(Object,Field,Value) ->
  if
    Object==null ->
      format
	(warning,
	 "*** Warning: set on field ~p with value ~p~non null object~n",
	 [Field,Value]),
      throw(badarg);
    true ->
      apply
	(module(Object),
	 field_set,
	 [Field,Value,Object])
  end.

%% @doc
%% Modifies the value of a static, i.e., class attribute.
-spec set_static(node_id(), class_name(), attribute_name(), value()) -> value().
set_static(NodeId,ClassName,Field,Value) ->
  apply(module(NodeId,ClassName),
	field_set,
	[Field,Value,NodeId]).


%% @doc Initializes the Java interface library
%% providing default options.
%% It is called automatically by `start_node/0' and
%% `standard_node/1'. Calling `init/1' explicitely is
%% useful to customize the library when multiple
%% Java connections are used.
-spec init([option()]) -> ok.
init(UserOptions) ->
  DefaultOptions = default_options(),
  Options = UserOptions++DefaultOptions,
  _ =
    spawn(fun () ->
	      ets:new(java_nodes,[named_table,public]),
	      ets:new(java_classes,[named_table,public]),
	      ets:new(java_threads,[named_table,public]),
	      ets:new(java_objects,[named_table,public]),
	      wait_forever()
	  end),
  wait_until_stable(),
  ets:insert(java_nodes,{options,Options}),
  ok.

wait_until_stable() ->
  case {ets:info(java_nodes),
	ets:info(java_classes),
	ets:info(java_threads),
	ets:info(java_objects)} of
    {Info1,Info2,Info3,Info4}
      when is_list(Info1), is_list(Info2), is_list(Info3), is_list(Info4) ->
      ok;
    _ ->
      timer:sleep(10),
      wait_until_stable()
  end.

wait_forever() ->
  receive _ -> wait_forever() end.
      
%% @doc
%% Returns a list with the default options.
-spec default_options() -> [option()].
default_options() ->
  OtpClassPath =
    case code:priv_dir(jinterface) of
      {error,_} -> [];
      OtpPath when is_list(OtpPath) ->
	[OtpPath++"/OtpErlang.jar"]
    end,
  JavaErlangClassPath =
    case code:priv_dir(java_erlang) of
      {error,_} -> [];
      JavaErlangPath when is_list(JavaErlangPath) ->
	[JavaErlangPath++"/JavaErlang.jar"]
    end,
  ClassPath = OtpClassPath++JavaErlangClassPath,
  JavaExecutable = 
    case os:find_executable("java") of
      false -> "java";
      Executable -> Executable
    end,
  ?LOG("Java classpath is ~p~n",[ClassPath]),
  [{java_sources,"java_sources"},
   {java_beams,"java_sources/ebin"},
   {java_class,"javaErlang.JavaErlang"},
   {java_classpath,ClassPath},
   {java_executable,JavaExecutable},
   {java_verbose,false},
   {call_timeout,10000},
   {log_level,notice},
   {mangle_classnames,false}].


%% @doc
%% Returns the version number of the JavaErlang library.
-spec version() -> string().
version() ->
  ?JAVA_ERLANG_VERSION.


%% @doc Returns the node where the object argument is located.
-spec node_id(object_ref()) -> node_id().
node_id({_,_,NodeId}) ->
  NodeId.

%% @doc
%% Returns the symbolic name of a Java node.
-spec symbolic_name(node_id()) -> string().
symbolic_name(NodeId) ->
  {ok,Node} = node_lookup(NodeId),
  Node#node.symbolic_name.

%% @doc
%% Returns the set of active Java nodes.
-spec nodes() -> [node_id()].
nodes() ->
  case ets:info(java_nodes) of
    undefined -> [];
    _ ->
      lists:map
	(fun ({NodeId,_Node}) when is_integer(NodeId) -> NodeId end,
	 ets:tab2list(java_nodes))
  end.

%% @doc
%% Resets the state of a Java node, i.e., 
%% the object proxy is reset.
%% This operation will cause all Java object references
%% existing to become invalid (i.e., not referring to
%% any Java object), but references to Java methods, constructors
%% or fields are not affected. In addition all threads created are
%% eventually stopped, and a new thread created to service future
%% calls. Note that the function call may return before all threads
%% have stopped.
-spec reset(node_id()) -> any().
reset(NodeId) ->
  %% Threads are removed, so we have to clean up the Erlang thread table
  remove_thread_mappings(NodeId),
  remove_object_mappings(NodeId),
  javaCall(NodeId,reset,void).

%% @doc
%% Shuts down and terminates the connection to a Java node.
-spec terminate(node_id()) -> any().
terminate(NodeId) ->
  remove_thread_mappings(NodeId),
  remove_class_mappings(NodeId),
  remove_object_mappings(NodeId),
  ets:delete(java_nodes,NodeId).

%% @doc
%% Shuts down and terminates the connection to all known Java nodes.
-spec terminate_all() -> any().
terminate_all() ->
  case ets:info(java_nodes) of
    undefined -> ok;
    _ ->
      lists:foreach
	(fun ({NodeId,_Node}) ->
	     javaCall(NodeId,terminate,void);
	     (_) -> ok
	 end, ets:tab2list(java_nodes)),
      ets:delete(java_nodes),
      ets:delete(java_classes),
      ets:delete(java_objects),
      ets:delete(java_threads)
  end.

%% @doc
%% Brutally shuts down and terminates the connection to a Java node.
%% Does not send a termination message to the Java node, instead it
%% attempts to kill the Unix process corresponding to the Java runtime system
%% of the node. This will obviously only work under Unix/Linux.
-spec brutally_terminate(node_id()) -> any().
brutally_terminate(NodeId) ->
  case runs_on_windows() of
    true ->
      java:format
	(error,
	 "*** Error: brutally_terminate not supported under windows~n"),
      throw(nyi);
    _ -> ok
  end,
  {ok,Node} = node_lookup(NodeId),
  remove_thread_mappings(NodeId),
  remove_class_mappings(NodeId),
  remove_object_mappings(NodeId),
  ets:delete(java_nodes,NodeId),
  os:cmd(io_lib:format("kill -9 ~p",[Node#node.unix_pid])),
  ok.

%% @doc
%% Recreates a possibly dead node. Obviously any ongoing computations,
%% object bindings, and so on are forgotten, but the classpaths
%% and other node options are restored.
-spec recreate_node(node_id()) -> {ok,node_id()} | {error,any()}.

recreate_node(NodeId) ->
  {ok,Node} = node_lookup(NodeId),
  PreNode = Node#node{num_start_tries=0},
  spawn_java(PreNode,get_java_node_id()).

%% @doc
%% Brutally shuts down and attempts to terminate 
remove_thread_mappings(NodeId) ->
  lists:foreach
    (fun ({Key={NodeIdKey,_},_}) ->
	 if NodeId==NodeIdKey -> ets:delete(java_threads,Key);
	    true -> ok
	 end
     end, ets:tab2list(java_threads)).

remove_class_mappings(NodeId) ->
  lists:foreach
    (fun ({Key={NodeIdKey,_},_}) ->
	 if NodeId==NodeIdKey -> ets:delete(java_classes,Key);
	    true -> ok
	 end
     end, ets:tab2list(java_classes)).

remove_object_mappings(NodeId) ->
  lists:foreach
    (fun ({Key={_,_,NodeIdKey},_}) ->
	 if NodeId==NodeIdKey -> ets:delete(java_objects,Key);
	    true -> ok
	 end
     end, ets:tab2list(java_objects)).

%% @doc
%% Lets Java know that an object can be freed.
-spec free(object_ref()) -> any().
free(Object) ->
  javaCall(node_id(Object),free,Object).	      

%% @doc Sets the timeout value for Java calls.
%% Calls to Java from the current Erlang process will henceforth
%% fail after Timeout seconds (or never is the argument is
%% the atom infinity).
%% Implementation note: this function stores data in the Erlang 
%% process dictionary.
-spec set_timeout(integer() | infinity) -> any().
set_timeout(Timeout) ->
  set_value(timeout,Timeout).

get_timeout(Node) ->
  get_value(timeout,Node#node.call_timeout).

set_value(ValueName,Value) ->
  PropList =
    case erlang:get({javaErlangOptions,self()}) of
      undefined ->
	[];
      Other ->
	proplists:delete(ValueName,Other)
    end,
  put({javaErlangOptions,self()},[{ValueName,Value}|PropList]).

get_value(ValueName,Default) ->
  case get({javaErlangOptions,self()}) of
    PropList when is_list(PropList) ->
      proplists:get_value(ValueName,PropList,Default);
    _ ->
      Default
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% @doc
%% Returns true if its argument is a Java object reference, false otherwise.
-spec is_object_ref(any()) -> boolean().
is_object_ref({object,_,_}) ->
  true;
is_object_ref({executable,_,_}) ->
  true;
is_object_ref({thread,_,_}) ->
  true;
is_object_ref(_) ->
  false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% @doc
%% Returns the elements of the (one-dimensional) array object argument
%% as an Erlang list of objects.
-spec array_to_list(object_ref()) -> [value()].
array_to_list(ArrayObj) ->
  javaCall(node_id(ArrayObj),array_to_list,ArrayObj).

%% @doc
%% Creates a one-dimensional Java array populated with the elements
%% from the Erlang list argument, using the type specification
%% as an element recipe. Example:
%% ``java:list_to_array(NodeId,"Hello World!",char).''
-spec list_to_array(node_id(),[value()],type()) -> object_ref().
list_to_array(NodeId,List,Type) when is_list(List) ->
  javaCall(NodeId,list_to_array,{Type,list_to_tuple(List)}).

%% @doc
%% Returns the elements of the Java String as an Erlang list.
-spec string_to_list(object_ref()) -> [char()].
string_to_list(String) ->
  Bytes = (module(String)):getBytes(String),
  array_to_list(Bytes).

%% @doc 
%% Converts the Erlang string argument to a Java string.
%% This function is for convenience only; it is implementable using
%% the rest of the Java API.
-spec list_to_string(node_id(),string()) -> object_ref().
list_to_string(NodeId,List) when is_list(List) ->
  java:new(NodeId,'java.lang.String',[List]).

%% @doc Widens or narrows a number.
-spec convert(node_id(),number_type(),java_number()) -> java_number().
convert(NodeId,Class,Number) when is_number(Number), is_atom(Class) ->
  javaCall(NodeId,convert,{Class,Number}).

%% @doc
%% Returns true if the first parameter (a Java object) is an instant
%% of the class named by the second parameter.
%% This function is for convenience only; it is implementable using
%% the rest of the Java API.
-spec instanceof(object_ref(),class_name()) -> boolean().
instanceof(Obj,ClassName) when is_list(ClassName) ->
  instanceof(Obj,list_to_atom(ClassName));
instanceof(Object,ClassName) when is_atom(ClassName) ->
  javaCall(node_id(Object),instof,{Object,ClassName}).

%% @doc Convenience method for determining subype relationship.
%% Returns true if the first argument is a subtype of the second.
-spec is_subtype(node_id(),class_name(),class_name()) -> boolean().
is_subtype(NodeId,Class1,Class2) when is_atom(Class1), is_atom(Class2) ->
  javaCall(NodeId,is_subtype,{Class1,Class2}).

%% @doc
%% Returns the classname (as returned by the method getName() in
%% java.lang.Class)
%% of Java object parameter.
%% This function is for convenience only; it is implementable using
%% the rest of the Java API.
-spec getClassName(object_ref()) -> class_name().
getClassName(Object) ->
  getClassName(node_id(Object),Object).
-spec getClassName(node_id(),object_ref()) -> class_name().
getClassName(NodeId,Obj) ->
  javaCall(NodeId,getClassName,Obj).

%% @doc
%% Returns the simple classname (as returned by the method getSimplename() in
%% java.lang.Class)
%% of Java object parameter.
%% This function is for convenience only; it is implementable using
%% the rest of the Java API.
-spec getSimpleClassName(object_ref()) -> class_name().
getSimpleClassName(Object) ->
  getSimpleClassName(node_id(Object),Object).
-spec getSimpleClassName(node_id(),object_ref()) -> class_name().
getSimpleClassName(NodeId,Obj) ->
  javaCall(NodeId,getSimpleClassName,Obj).

%% @doc
%% Prints the Java stacktrace on the standard error file error descriptor
%% that resulted in the throwable object argument.
%% This function is for convenience only; it is implementable using
%% the rest of the Java API.
-spec print_stacktrace(object_ref()) -> any().
print_stacktrace(Exception) ->
  Err = get_static(node_id(Exception),'java.lang.System',err),
  call(Exception,printStackTrace,[Err]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% @doc
%% Ensures that the Erlang representation of
%% the Java class with name ClassName is loaded into the Erlang
%% runtime system. If necessary, this function will generate
%% the source code for the Erlang module (from the Java class),
%% compile it, and load it. Returns the name of the Erlang module
%% implementing the Java class.
%% Example: 
%% ``` java:acquire_class(NodeId,'java.lang.Integer'),
%%     JavaInt10 = java_lang_Integer:'Integer'(NodeId,10).'''
-spec acquire_class(node_id(),class_name()) -> atom().
acquire_class(NodeId,ClassName) when is_list(ClassName) ->
  acquire_class(NodeId,list_to_atom(ClassName));
acquire_class(NodeId,ClassName) when is_atom(ClassName) ->
  ?LOG("acquire_class(~p,~p)~n",[NodeId,ClassName]),
  Class = acquire_class_int(NodeId,ClassName),
  Class#class.module_name.

acquire_class_int(NodeId,ClassName) ->
  case class_lookup(NodeId,ClassName) of
    {ok,Class} ->
      Class;
    _ ->
      FullClassName = classname(ClassName,NodeId),
      case get_load_permission(FullClassName) of
	ok ->
	  try l(NodeId,ClassName) of
	    Result ->
	      ets:delete(java_classes,{loading,FullClassName}),
	      Result
	  catch _:_ ->
	      try c(NodeId,ClassName) of
		Result ->
		  ets:delete(java_classes,{loading,FullClassName}),
		  Result
	      catch ExceptionClass:Reason ->
		  ets:delete(java_classes,{loading,FullClassName}),
		  erlang:raise(ExceptionClass,Reason,erlang:get_stacktrace())
	      end
	  end
      end
  end.

%% Since classes can be loaded from multiple processes simultaneously
%% we have to serialize such attempts (to prevent getting purge errors
%% for instance).
get_load_permission(ClassName) ->
  case ets:insert_new(java_classes,{{loading,ClassName},self()}) of
    true ->
      ok;
    false ->
      timer:sleep(10),
      get_load_permission(ClassName)
  end.

class_lookup(NodeId,ClassName) when is_atom(ClassName) ->
  Key = {NodeId,ClassName},
  case ets:lookup(java_classes,Key) of
    [{_,Class}] ->
      {ok,Class};
    _ ->
      false
  end.

class_store(NodeId,ClassName,Class) when is_atom(ClassName) ->
  java:format(debug,"Storing class info for class ~p~n",[ClassName]),
  ets:insert(java_classes,{{NodeId,ClassName},Class}),
  Class.

%% @private
node_lookup(NodeId) ->
  case ets:lookup(java_nodes,NodeId) of
    [{_,Node}] ->
      {ok,Node};
    _ ->
      format(error,"node_lookup(~p) failed??~n",[NodeId]),
      false
  end.

node_store(Node) ->
  ets:insert(java_nodes,{Node#node.node_id,Node}).

%% @private
classname(ClassName,NodeId) ->
  MangleNames = get_option(mangle_classnames,NodeId,false),
  FinalClassName =
    if
      MangleNames ->
	%% Maybe we should rather check for a certain package here,
	%% or have it as an option, or...
	case 
	  lists:member
	  (firstComponent(ClassName),
	   ['java','javax','org','net']) of
	  true ->
	    ClassName;
	  false ->
	    list_to_atom
	      ("node_"++integer_to_list(NodeId)++"_"++atom_to_list(ClassName))
	end;
      true -> ClassName
    end,
  if
    FinalClassName=/=ClassName ->
      ?LOG("Computed classname ~p for ~p~n",[FinalClassName,ClassName]);
    true ->
      ok
  end,
  FinalClassName.

%% @private
to_erl_name(ClassName) when is_atom(ClassName) ->
  list_to_atom(to_erl_name(atom_to_list(ClassName)));
to_erl_name(ClassName) when is_list(ClassName) ->
  lists:map
    (fun (Ch) ->
	 if
	   Ch==$. -> $_;
	   Ch==$\$ -> $_;
	   true -> Ch
	 end
     end, ClassName).

class_bind(NodeId,ClassName,ModuleName) when is_atom(ClassName) ->
  case class_lookup(NodeId,ClassName) of
    {ok,Class} ->
      Class;
    _ ->
      class_store(NodeId,ClassName,ModuleName:bind(NodeId))
  end.

l(NodeId,ClassName) when is_atom(ClassName) ->
  ErlModule = to_erl_name(classname(ClassName,NodeId)),
  case code:is_loaded(ErlModule) of
    false ->
      case my_load_file(ErlModule,ClassName,NodeId) of
	{module,ModuleName} -> 
	  FileName = code:which(ModuleName),
	  true = is_list(FileName),
	  Class = class_bind(NodeId,ClassName,ModuleName),
	  ClassLocationStr =
	    if
	      Class#class.class_location==[] ->
		"";
	      true ->
		io_lib:format
		  ("; Java code at ~p",[Class#class.class_location])
	    end,
	  format
	    (info,
	     "~p: [loaded ~p from ~s~s]~n",
	     [symbolic_name(NodeId),ModuleName,FileName,ClassLocationStr]),
	  Class
      end;
    {file,_} ->
      FileName = code:which(ErlModule),
      true = is_list(FileName),
      %%io:format
      %%("~p: [~p already loaded from ~s]~n",
      %%[symbolic_name(NodeId),ClassName,FileName]),
      class_bind(NodeId,ClassName,ErlModule)
  end.

my_load_file(ErlModule,ClassName,NodeId) when is_atom(ErlModule) ->
  java:format
    (debug,
     "~p: my_load_file(~p,~p)~n",
     [symbolic_name(NodeId),ErlModule,NodeId]),
  ErlModuleString = atom_to_list(ErlModule),
  BeamDir = get_option(java_beams,NodeId),
  JavaSources = get_option(java_sources,NodeId),
  case file:read_file_info(BeamDir) of
    {error,_} -> 
      case file:read_file_info(JavaSources) of
	{error,_} ->
	  file:make_dir(JavaSources),
	  file:make_dir(BeamDir);
	_ -> 
	  file:make_dir(BeamDir)
      end;
    _ -> ok
  end,
  LoadFile = BeamDir++"/"++ErlModuleString,
  java:format(debug,"Trying to load ~p~n",[LoadFile]),
  case code:load_abs(LoadFile) of
    Result={module,_} -> 
      %% Check if the loaded file is still up-to-date
      ClassLocation = ErlModule:class_location(),
      CurrentClassLocation = javaCall(NodeId,getClassLocation,ClassName),
      if
	ClassLocation =/= CurrentClassLocation ->
	  java:format
	    (debug,
	     "Class location has changed from ~s to ~s; recompiling...~n",
	     [ClassLocation,CurrentClassLocation]),
	  code:purge(ErlModule);
	true ->
	  CreationTime = {CreationDate,_} = ErlModule:creation_time(),
	  java:format
	    (debug,"File loaded; source was at ~s~nCreation time: ~p~n",
	     [ClassLocation,CreationTime]),
	  if
	    ClassLocation=/="" ->
	      case file:read_file_info(ClassLocation) of
		{ok,FileInfo} -> 
		  MTime = {MTimeDate,_} = FileInfo#file_info.mtime,
		  GregCreatDate =
		    calendar:date_to_gregorian_days(CreationDate),
		  GregCreatSeconds =
		    calendar:datetime_to_gregorian_seconds(CreationTime),
		  GregMTimeDate = 
		    calendar:date_to_gregorian_days(MTimeDate),
		  GregMTimeSeconds = 
		    calendar:datetime_to_gregorian_seconds(MTime),
		  if
		    (GregCreatDate > GregMTimeDate) orelse
		    ((GregCreatDate == GregMTimeDate) andalso
		     (GregCreatSeconds >= GregMTimeSeconds)) ->
		      java:format
			(debug,
			 "Erlang module up-to-date~n",
			 []),
		      Result;
		    true ->
		      java:format
			(debug,
			 "Source file has changed at ~p; have to recompile~n",
			 [MTime]),
		      code:purge(ErlModule)
		  end;
		_ ->
		  java:format
		    (debug,
		     "Source file not found; we are going to recompile ~s "++
		     "which should fail~n",
		     [ClassLocation]),
		  code:purge(ErlModule)
	      end;
	    true -> Result
	  end
	end;
    Other ->
      java:format
	(debug,
	 "Loading of ~p failed~n",[LoadFile]),
      Other
  end.

c(NodeId,ClassName) when is_atom(ClassName) ->
  {FileName,IsTemporary} =
    translate_java_to_erlang:gen_java_erlang_module(NodeId,ClassName),
  format
    (info,
     "~p: c -- generated file ~p from Java class ~p~n",
     [symbolic_name(NodeId),FileName,ClassName]),
  RootName = 
    filename:rootname(FileName),
  BeamDir =
    if 
      not(IsTemporary) -> get_option(java_beams,NodeId);
      true -> "/tmp"
    end,
  format(info,"compile:file(~p,~p)~n",[RootName,[{outdir,BeamDir}]]),
  case compile:file(RootName,[{outdir,BeamDir},debug_info]) of
    {ok,ModuleName} ->
      LoadFile = BeamDir++"/"++filename:basename(RootName),
      case code:load_abs(LoadFile) of
	{module,ModuleName} ->
	  LoadedName = code:which(ModuleName),
	  true = is_list(LoadedName),
	  if 
	    IsTemporary ->
	      ok = file:delete(FileName),
	      ok = file:delete(LoadFile++".beam");
	    true ->
	      ok
	  end,
	  Class = class_bind(NodeId,ClassName,ModuleName),
	  ClassLocationStr =
	    if
	      Class#class.class_location==[] ->
		"";
	      true ->
		io_lib:format
		  ("; Java code at ~p",[Class#class.class_location])
	    end,
	  format
	    (info,
	     "~p: c -- [loaded ~p from ~s~s]~n",
	     [symbolic_name(NodeId),ClassName,LoadedName,ClassLocationStr]),
		Class;
	Other ->
	  format
	    (warning,
	     "~p: c -- loading of compiled file ~p failed due to:~n~p ???~n",
	     [symbolic_name(NodeId),LoadFile,Other]),
	  throw(compile)
      end;
    Other ->
      format
	(warning,
	 "~p: c -- compilation of generated file ~p failed due to:~n~p ???~n",
	 [symbolic_name(NodeId),RootName,Other]),
      throw(compile)
  end.

%% @private
class_info(Arg,ClassName) ->
  NodeId =
    case is_object_ref(Arg) of
      true -> node_id(Arg);
      false -> Arg
    end,
  case class_lookup(NodeId,ClassName) of
    {ok,Class} ->
      Class;
    _ ->
      acquire_class(NodeId,ClassName),
      case class_lookup(NodeId,ClassName) of
	{ok,Class} -> 
	  Class;
	_ ->
	  format
	    (warning,
	     "class_info(~p,~p): class not found??~n",
	     [NodeId,ClassName]),
	  throw(class_info)
      end
  end.

%% @private
constructor(Node,Class,ArgTypes) ->
  {Constructor,Arity} =
    javaCall(Node,getConstructor,{Class,list_to_tuple(ArgTypes)}),
  mk_fun
    (Arity,
     fun (Values) ->
	 javaCall(Node,call_constructor,{Constructor,Values})
     end).

%% @private
field_get(Node,Class,FieldName) ->
  {Field,_FieldType} = javaCall(Node,getField,{Class,FieldName}),
  mk_fun1
    (0,
     fun (Object,Values) ->
	 javaCall(Node,getFieldValue,{Object,Field,Values})
     end).

%% @private
field_set(NodeId,Class,FieldName) ->
  {Field,_FieldType} = javaCall(NodeId,getField,{Class,FieldName}),
  fun (Object,Value) ->
      javaCall(NodeId,setFieldValue,{Object,Field,Value})
  end.

%% @private
get_class_info(NodeId,ObserverInPackage,ClassName) ->
  format(info,"Computing class info for class ~p~n",[ClassName]),
  Constructors =
    get_constructors(ClassName,NodeId,ObserverInPackage),
  Methods =
    get_methods(ClassName,NodeId,ObserverInPackage),
  Classes =
    get_classes(ClassName,NodeId,ObserverInPackage),
  Fields =
    get_fields(ClassName,NodeId,ObserverInPackage),
  format(info,"Found class info for class ~p~n",[ClassName]),
  ClassLocation = javaCall(NodeId,getClassLocation,ClassName),
  #class_info
	{name=ClassName,
	 class_location=ClassLocation,
	 constructors=Constructors,
	 methods=Methods,
	 classes=Classes,
	 fields=Fields}.

get_constructors(ClassName,NodeId,ObserverInPackage) ->
  javaCall(NodeId,getConstructors,{ClassName,ObserverInPackage}).
get_methods(ClassName,NodeId,ObserverInPackage) ->
  javaCall(NodeId,getMethods,{ClassName,ObserverInPackage}).
get_classes(ClassName,NodeId,ObserverInPackage) ->
  javaCall(NodeId,getClasses,{ClassName,ObserverInPackage}).
get_fields(ClassName,NodeId,ObserverInPackage) ->
  javaCall(NodeId,getFields,{ClassName,ObserverInPackage}).

%% @private
method(Node,Class,Method,ArgTypes) ->
  {_MethodObj,Fun} = method_obj_and_fun(Node,Class,Method,ArgTypes),
  Fun.

%% @private
method_obj_and_fun(Node,Class,Method,ArgTypes) ->
  {MethodObj,Arity,_IsStatic} =
    javaCall(Node,getMethod,{Class,Method,list_to_tuple(ArgTypes)}),
  {MethodObj,
   mk_fun1
   (Arity,
    fun (Object,Values) ->
	javaCall(Node,call_method,{Object,MethodObj,Values})
    end)}.

mk_fun(0,Fun) ->
  fun () -> Fun({}) end;
mk_fun(1,Fun) ->
  fun (Arg1) -> Fun({Arg1}) end;
mk_fun(2,Fun) ->
  fun (Arg1,Arg2) -> Fun({Arg1,Arg2}) end;
mk_fun(3,Fun) ->
  fun (Arg1,Arg2,Arg3) -> Fun({Arg1,Arg2,Arg3}) end;
mk_fun(4,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4) -> Fun({Arg1,Arg2,Arg3,Arg4}) end;
mk_fun(5,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5}) end;
mk_fun(6,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6}) end;
mk_fun(7,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7}) end;
mk_fun(8,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8}) end;
mk_fun(9,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9}) end;
mk_fun(10,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10}) end;
mk_fun(11,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11}) end;
mk_fun(12,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12}) end;
mk_fun(13,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13}) end;
mk_fun(14,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14}) end;
mk_fun(15,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14,Arg15) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14,Arg15}) end;
mk_fun(16,Fun) ->
  fun (Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14,Arg15,Arg16) -> Fun({Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14,Arg15,Arg16}) end;
mk_fun(N,_Fun) ->
  format
    (error,
     "*** Error: methods or constructors of arity>16 not supported"),
  throw({arity,N}).

mk_fun1(0,Fun) ->
  fun (Object) -> Fun(Object,{}) end;
mk_fun1(1,Fun) ->
  fun (Object,Arg1) -> Fun(Object,{Arg1}) end;
mk_fun1(2,Fun) ->
  fun (Object,Arg1,Arg2) -> Fun(Object,{Arg1,Arg2}) end;
mk_fun1(3,Fun) ->
  fun (Object,Arg1,Arg2,Arg3) -> Fun(Object,{Arg1,Arg2,Arg3}) end;
mk_fun1(4,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4}) end;
mk_fun1(5,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5}) end;
mk_fun1(6,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6}) end;
mk_fun1(7,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7}) end;
mk_fun1(8,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8}) end;
mk_fun1(9,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9}) end;
mk_fun1(10,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10}) end;
mk_fun1(11,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11}) end;
mk_fun1(12,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12}) end;
mk_fun1(13,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13}) end;
mk_fun1(14,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14}) end;
mk_fun1(15,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14,Arg15) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14,Arg15}) end;
mk_fun1(16,Fun) ->
  fun (Object,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14,Arg15,Arg16) -> Fun(Object,{Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,Arg10,Arg11,Arg12,Arg13,Arg14,Arg15,Arg16}) end;
mk_fun1(N,_Fun) ->
  format
    (error,
     "*** Error: methods or constructors of arity>16 not supported"),
  throw({arity,N}).


%% @private
type_compatible_alternatives(Node,Objs,Alternatives) ->
  tca(Node,Objs,Alternatives).

tca(_Node,Params,[]) ->
  format
    (warning,
     "*** Warning: no constructor/method with arity ~p found, "
     "which accepts parameters ~s~n",
     [length(Params),print_parameters(Params)]),
  throw(badarg);
tca(Node,Params,[{Types,Alternative}|Rest]) ->
  ?LOG("Types=~p Params=~p~n",[Types,Params]),
  Result = 
    javaCall
      (Node,
       objTypeCompat,
       {list_to_tuple(Types),
	list_to_tuple(Params)}),
  if
    Result -> 
      ?LOG("Params ~p matches ~p~n",[Params,Types]),
      Alternative();
    true ->
      ?LOG("Params ~p do not match ~p~n",[Params,Types]),
      tca(Node,Params,Rest)
  end.

%% @doc Returns the name of the Erlang module to which the argument 
%% object belongs. The function will generate an Erlang module, and
%% compile and load it, if necessary. 
-spec module(object_ref()) -> atom().
module(Object) ->
  case ets:lookup(java_objects,Object) of
    [{_,Module}] -> Module;
    _ ->
      ClassName = getClassName(node_id(Object),Object),
      ?LOG("Object ~p has class name ~p~n",[Object,ClassName]),
      Class = acquire_class_int(node_id(Object),ClassName),
      Module = Class#class.module_name,
      ets:insert(java_objects,{Object,Module}),
      Module
  end.
  
%% @doc Returns the name of the Erlang module
%% which implements the class argument.
%% This function will translate the class to an Erlang module,
%% and load the Erlang module, if required.
module(NodeId,ClassName) ->
  acquire_class(NodeId,ClassName).

firstComponent(Atom) when is_atom(Atom) ->
  list_to_atom(firstComponent(atom_to_list(Atom)));
firstComponent(Atom) when is_list(Atom) ->
  case string:chr(Atom,$.) of
    0 -> Atom;
    N -> string:substr(Atom,1,N-1)
  end.

%% @private
finalComponent(Atom) when is_atom(Atom) ->
  list_to_atom(finalComponent(atom_to_list(Atom)));
finalComponent(Atom) when is_list(Atom) ->
  case string:rchr(Atom,$.) of
    0 -> Atom;
    N -> string:substr(Atom,N+1)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


runs_on_windows() ->
  case os:type() of
    {win32,_} ->
      true;
    {win64,_} ->
      true;
    _ -> 
      false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_parameters(L) when is_list(L) ->
  combine_strings
    (",",lists:map(fun (Arg) -> io_lib:format("~p",[Arg]) end, L)).

combine_strings(_Delim,[]) ->
  [];
combine_strings(_Delim,[Str]) ->
  Str;
combine_strings(Delim,[Str|Rest]) when is_list(Delim), is_list(Str) ->
  Str++Delim++combine_strings(Delim,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Rudimentary logging support; in the future we should probably use
%% a standard logger

get_options() ->
  case ets:info(java_nodes) of
    undefined -> default_options();
    _ -> 
      case ets:lookup(java_nodes,options) of
	[{_,Options}] -> Options;
	[] -> default_options()
      end
  end.

%% @doc
%% Determines how much debugging information is displayed.
%%
-spec set_loglevel(Level::loglevel()) -> any().
set_loglevel(Level) ->
  user_level(Level),
  %% Warning. This is racy code.
  case ets:info(java_nodes) of
    undefined ->
      init([{log_level,Level}]);
    _ ->
      Options = get_options(),
      ets:insert(java_nodes,{options,[{log_level,Level}|Options]})
  end.

get_loglevel() ->
  proplists:get_value(log_level,get_options()).

%% @private
format(Level,Message) ->
  level(Level),
  case permit_output(get_loglevel(),Level) of
    true -> io:format(Message);
    _ -> ok
  end.

%% @private
format(Level,Format,Message) ->
  level(Level),
  case permit_output(get_loglevel(),Level) of
    true -> io:format(Format,Message);
    _ -> ok
  end.

permit_output(LevelInterest,LevelOutput) ->
  user_level(LevelInterest) >= level(LevelOutput).

user_level(none) -> -1;
user_level(all) -> 100;
user_level(Other) -> level(Other).

level(emergency) -> 0;
level(alert) -> 1;
level(critical) -> 2;
level(error) -> 3;
level(warning) -> 4;
level(notice) -> 5;
level(info) -> 6;
level(debug) -> 7;
level(_) -> throw(badarg).

