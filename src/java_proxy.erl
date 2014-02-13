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
-export([new/2]).

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
  spawn(fun () -> looper(self()) end).

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

class(NodeId,Name,ClassName,MethodFuns) ->
  {Methods,Funs} = lists:unzip(MethodFuns),
  Proxy = 
    java:javaCall
      (NodeId,
       new_proxy_class,
       {ClassName,Methods}),
  ets:insert(proxy_classes,{Name,NodeId,Proxy,list_to_tuple(Funs)}),
  Proxy.

new(Name,Init) ->
  case ets:lookup(proxy_classes,Name) of
    [{_,NodeId,Proxy,Funs}] ->
      Counter = ets:update_counter(proxy_objects,proxy_counter,1),
      [{_,ProxyPid}] = ets:lookup(proxy_classes,proxy_pid),
      {Object,Handler} = 
	java:javaCall
	  (NodeId,
	   new_proxy_object,
	   {Proxy,Counter,ProxyPid}),
      ets:insert
	(proxy_objects,
	 {{object,Counter},Init,idle,Funs,Handler}),
      Object
  end.

handle_call(Fun,Args,Context,Handler,ProxyServer,ObjectId) ->
  NodeId = java:node_id(Handler),
  Result = Fun([Context|Args]),
  java:javaCall(NodeId,proxy_reply,{Handler,Result}),
  ProxyServer!{done,ObjectId}.

start_looper() ->
  looper(self()).

looper(ProxyServer) ->
  receive
    Msg={proxy_invoke,ObjectId,JavaSelf,Method,FunIndex,Args} ->
      Context = {JavaSelf,Method},
      ?LOG("got message ~p~n",[Msg]),
      case ets:lookup(proxy_objects,{object,ObjectId}) of
	[{_,State,Status,Funs,Handler}] ->
	  spawn
	    (fun () ->
		 handle_call
		   (element(FunIndex,Funs),Args,Context,Handler,ProxyServer,ObjectId)
	     end),
	  looper(ProxyServer);
	[] ->
	  io:format
	    ("*** error: proxy table does not contain object ~p~n",
	     [ObjectId]),
	  throw(proxy)
      end;
    Other -> 
      io:format
	("looper at pid ~p~nstrange message ~p received~n",
	 [self(),Other]),
      looper(ProxyServer)
  end.
  
