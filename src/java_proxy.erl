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

%% @doc This module implements a highly experimental and probably useless 
%% facility for permitting Erlang to define Java (proxy) classes and 
%% handle calls to such classes. What use is this? 
%% Well, one possibility is to implement ActionListeners for the Swing
%% graphics library in Erlang.
%% @author Lars-Ake Fredlund (lfredlund@fi.upm.es)
%% @copyright 2011 Lars-Ake Fredlund
%%

-module(java_proxy).
-compile(export_all).

-include("class.hrl").

-record(proxy,{method_funs,backing_object,node_id,
	       interface,class,object,handler,erlang_state}).

-export([start/0,create_proxy_server/0]).
-export([new/5,new/3,new/6,new/2,proxySkeleton/4]).

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

%% @private
start() ->
  case ets:info(proxy_table) of
    undefined ->
      spawn(
	fun () ->
	    _ = ets:new(proxy_classes,[named_table,public]),
	    wait_forever()
	end),
      spawn(
	fun () ->
	    _ = ets:new(proxy_objects,[named_table,public]),
	    wait_forever()
	end),
      wait_until_stable(),
      ets:insert
	(proxy_classes,
	 {proxy_pid,spawn_link(fun () -> start_looper() end)}),
      ets:insert
	(proxy_objects,
	 {proxy_counter,0}),
      proxy_table;
    _ ->
      proxy_table
  end.

%% @private
create_proxy_server() ->
  spawn(fun () -> looper() end).

wait_forever() ->
  receive _ -> wait_forever() end.

wait_until_stable() ->
  case {ets:info(proxy_classes), ets:info(proxy_classes)} of
    {Info1,Info2} when is_list(Info1), is_list(Info2) ->
      ok;
    _ ->
      timer:sleep(10),
      wait_until_stable()
  end.

%% Note. It is dangerous to create proxy objects from inside a proxy context.
define_invocation_handler(Proxy,Pid) when is_pid(Pid) ->
  java:javaCall
    (Proxy#proxy.node_id,
     define_invocation_handler,
     {Pid,Proxy#proxy.backing_object}).

class(NodeId,Name,ClassName,Methods,Fun) ->
  class(NodeId,Name,ClassName,Methods,Fun,[]).
class(NodeId,Name,ClassName,Methods,Fun,Options) ->
  IsSynchronized = proplists:get_value(synchronized,Options,false),
  Proxy = 
    java:javaCall
      (NodeId,
       new_proxy_class,
       {ClassName,Methods}),
  ets:insert(proxy_classes,{Name,NodeId,Proxy,Fun,IsSynchronized}),
  Proxy.

new(Name,Init) ->
  case ets:lookup(proxy_classes,Name) of
    [{_,NodeId,Proxy,Fun,Synchronized}] ->
      Counter = ets:update_counter(proxy_objects,proxy_counter,1),
      [{_,ProxyPid}] = ets:lookup(proxy_classes,proxy_pid),
      Object = 
	java:javaCall
	  (NodeId,
	   new_proxy_object,
	   {Proxy,Counter,ProxyPid}),
      ets:insert(proxy_objects,{object,Counter,Init,not_running,Fun,Synchronized}),
      Object
  end.

reply_to_handler(HandlerObj={object,_,NodeId},ValueObj) ->
  java:javaCall(NodeId,proxy_reply,{HandlerObj,ValueObj}).

%% @private
new(Proxy,InitialState,ProxyPid)
  when is_pid(ProxyPid) ->
  InvHandler =
    define_invocation_handler(Proxy,ProxyPid),
  N =
    Proxy#proxy.node_id,
  Class =
    Proxy#proxy.class,
  ClassLoader = 
    java:call(Class,getClassLoader,[]),
  ClassArr =
    java:list_to_array(N,[Class],'java.lang.Class'),
  ProxyObject =
    java:call_static
      (N,
       'java.lang.reflect.Proxy',
       newProxyInstance,
       [ClassLoader,
	ClassArr,
	InvHandler]),
  ProxyWithObject =
    Proxy#proxy
    {handler=InvHandler,object=ProxyObject,erlang_state=InitialState},
  ets:insert(proxy_table,{{proxy,ProxyObject},ProxyWithObject}),
  ProxyObject.

%% @doc
%% Creates an Erlang proxy object corresponding to the Java Interface
%% argument.

%%-spec new(java:node_id(),java:object_ref(),[{{java:method_name(),[java:type()]},fun((...) -> any())}],java:object_ref(),any()) -> any().
-spec new(java:node_id(),java:class_name(),[{{java:method_name(),[java:type()]},fun()}],java:object_ref()|null,any()) -> any().
new(N,Interface,Funs,BackingObject,InitialState) ->
  start(),
  [{_,ProxyPid}] = ets:lookup(proxy_table,default_proxy_pid),
  new(N,Interface,Funs,BackingObject,InitialState,ProxyPid).

%% @private
new(N,Interface,Funs,BackingObject,InitialState,ProxyPid)
  when is_pid(ProxyPid) ->
  start(),
  new
    (proxySkeleton(N,Interface,Funs,BackingObject),
     InitialState,
     ProxyPid).

%% @private
proxySkeleton(N,Interface,Funs,BackingObject) ->
  Class = java:acquire_class(N,Interface),
  {Methods,_} = Class#class.methods,
  InterfaceString = java:list_to_string(N,atom_to_list(Interface)),
  io:format
    ("methods of ~p are~n  ~p~nfuns are~n  ~p~n",
     [Interface,Methods,Funs]),
  MethodFuns =
    lists:map
      (fun ({Spec,Fun}) ->
	   case lists:keyfind(Spec,1,Methods) of
	     {_,JavaMethod} ->
	       {JavaMethod,Fun};
	     _ ->
	       java:format
		 (error,
		  "method ~p of class ~p not found~n",
		  [Spec,Interface]),
	       throw(bad)
	   end
       end, Funs),
  InterfaceClass =
    java:call_static(N,'java.lang.Class',forName,[InterfaceString]),
  #proxy{method_funs=MethodFuns,
	 backing_object=BackingObject,
	 node_id=N,
	 class=InterfaceClass,
	 interface=Interface}.

handle_call(Object,Method,Args,Proxy) ->
  ArgList =
    if Args==null -> [];
       true -> tuple_to_list(Args)
    end,
  ?LOG
    ("~nMethod is ~p~nTable is ~p~n",
     [Method,Proxy#proxy.method_funs]),
  if
    is_list(Proxy#proxy.method_funs) ->
      case lists:keyfind(Method,1,Proxy#proxy.method_funs) of
	{_,Fun} ->
	  ?LOG
	     ("found a specific function, arity will be ~p~n",
	      [length([Object|ArgList])]),
	  apply(Fun,[Proxy#proxy.erlang_state,Object,Method|ArgList]);
	false ->
	  ?LOG("no specific function found, passing the buck...~n",[]),
	  passbuck
      end;
    true ->
      apply(Proxy#proxy.method_funs,[Proxy#proxy.erlang_state,Object,Method|ArgList])
  end.

start_looper() ->
  looper().

looper() ->
  receive
    Msg={proxy_msg,ProxyObject,Method,Arguments} ->
      ?LOG("got message ~p~n",[Msg]),
      Key = {proxy,ProxyObject},
      case ets:lookup(proxy_table,Key) of
	[{_,Proxy}] ->
	  spawn
	    (fun () ->
		 Reply = handle_call(ProxyObject,Method,Arguments,Proxy),
		 do_reply_and_update_state(Reply,Key,Proxy)
	     end),
	  looper();
	[] ->
	  io:format
	    ("*** error: proxy table does not contain object ~p~n"++
	     "All keys: ",
	     [Key]),
	  lists:foreach
	    (fun ({{proxy,KeyT},_}) -> io:format("~p,",[KeyT]);
		 (_) -> ok
	     end,
	     ets:tab2list(proxy_table)),
	  io:format("~n"),
	  throw(proxy)
      end;
    Other -> 
      io:format
	("looper at pid ~p~nstrange message ~p received~n",
	 [self(),Other]),
      looper()
  end.

do_reply_and_update_state(Reply,LocalKey,Proxy) ->
  ?LOG("Reply is ~p~n",[Reply]),
  ResultValue =
    case Reply of
      {reply_local,Result,NewLocalState} ->
	ets:insert
	  (proxy_table,
	   {LocalKey,Proxy#proxy{erlang_state=NewLocalState}}),
	Result;
      {reply,Result} ->
	Result;
      passbuck ->
	passbuck
    end,
  reply_to_handler(Proxy#proxy.handler,ResultValue).
  
