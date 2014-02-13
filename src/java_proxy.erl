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

-include("class.hrl").

-record(proxy,{id,state,status,queue,funs,handler}).

-export([start/0]).
-export([class/4,new/2]).

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

%% @private
start() ->
  case ets:info(proxy_classes) of
    undefined ->
      spawn(
	fun () ->
	    _ = ets:new(proxy_classes,[named_table,public]),
	    wait_forever()
	end),
      spawn(
	fun () ->
	    _ = ets:new(proxy_objects,[named_table,public,{keypos,2}]),
	    wait_forever()
	end),
      wait_until_stable(),
      ets:insert
	(proxy_classes,
	 {proxy_pid,spawn_link(fun () -> looper() end)}),
      ets:insert(proxy_objects,{proxy_counter,proxy_counter,0});
    _ ->
      ok
  end.

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
    [{_,NodeId,ProxyClass,Funs}] ->
      Counter = ets:update_counter(proxy_objects,proxy_counter,1),
      [{_,ProxyPid}] = ets:lookup(proxy_classes,proxy_pid),
      {Object,Handler} = 
	java:javaCall
	  (NodeId,
	   new_proxy_object,
	   {ProxyClass,Counter,ProxyPid}),
      Proxy = #proxy{id=Counter,state=Init,status=idle,queue=[],funs=Funs,handler=Handler},
      ets:insert(proxy_objects,Proxy),
      Object
  end.

handle_call(FunIndex,Proxy,Args,Context,ProxyServer) ->
  Fun = element(FunIndex,Proxy#proxy.funs),
  Handler = Proxy#proxy.handler,
  NodeId = java:node_id(Handler),
  io:format("calling function with arguments of length ~p~n",[length(Args)+2]),
  case apply(Fun,[Context,Proxy#proxy.state|Args]) of
    {reply,PreResult,NewState} ->
      Result =
	if
	  PreResult==void -> null;
	  true -> PreResult
	end,
      java:javaCall(NodeId,proxy_reply,{Handler,Result}),
      ProxyServer!{done,Proxy#proxy.id,NewState}
  end.

looper() ->
  receive
    Msg={proxy_invoke,{ObjectId,_,_,_,_}} ->
      ?LOG("got message ~p~n",[Msg]),
      case ets:lookup(proxy_objects,ObjectId) of
	[Proxy] when Proxy#proxy.status==idle ->
	  maybe_run_one(Proxy#proxy{queue=Proxy#proxy.queue++[Msg]}),
	  looper();
	[Proxy] when Proxy#proxy.status==running ->
	  ets:insert(proxy_objects,Proxy#proxy{queue=Proxy#proxy.queue++[Msg]}),
	  looper();
	[] ->
	  io:format
	    ("*** error: proxy table does not contain object ~p~n",
	     [ObjectId]),
	  throw(proxy)
      end;
    {done,ObjectId,NewState} ->
      [Proxy] = ets:lookup(proxy_objects,ObjectId),
      ets:insert(proxy_objects,Proxy#proxy{state=NewState,status=idle}),
      maybe_run_one(Proxy),
      looper();
    Other -> 
      io:format
	("looper at pid ~p~nstrange message ~p received~n",
	 [self(),Other]),
      looper()
  end.
  
maybe_run_one(Proxy) ->
  case Proxy#proxy.queue of
    [{proxy_invoke,{_,JavaSelf,Method,FunIndex,Args}}|Rest] ->
      ProxyServer = self(),
      ets:insert(proxy_objects,Proxy#proxy{status=running,queue=Rest}),
      spawn
	(fun () ->
	     Context = {JavaSelf,Method},
	     handle_call(FunIndex,Proxy,Args,Context,ProxyServer)
	 end);
    _ -> ok
  end.

  
  
