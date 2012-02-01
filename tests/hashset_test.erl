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

%% @doc This module tests the Java HashSet implementation
%% using the Erlang QuickCheck tool [http://www.quviq.com/].
%% @author Lars-Ake Fredlund (lfredlund@fi.upm.es)
%% @copyright 2011 Lars-Ake Fredlund
%%

-module(hashset_test).

-export([test/0,sample_commands/0]).
-export([node_id/0]).
-export([initial_state/0,command/1,precondition/2,next_state/3,postcondition/3]).

-include_lib("eqc/include/eqc.hrl").

-record(state,{sets=[]}).


%% @doc
%% Tests the Java HashSet data structure.
test() ->
  eqc:quickcheck
    (?FORALL(Cmds,eqc_statem:commands(?MODULE),
	     begin
	       start_node(),
	       {H,DS,Res} = eqc_statem:run_commands(?MODULE,Cmds),
	       java:terminate(node_id()),
	       case Res of
		 ok -> true;
		 _ -> 
		   io:format("~nFailing...~nH=~p~nDS=~p~nRes=~p~n",[H,DS,Res]),
		   false
	       end
	     end)).

start_node() ->
  {ok,Node} =
    java:start_node
      ([{java_exception_as_value,true},{add_to_java_classpath,["classes"]}]),
  put(node,Node).

node_id() ->
  get(node).

%% @private
sample_commands() ->
  eqc_gen:sample(eqc_statem:commands(hashset_test)).

%% @private
initial_state() ->
  #state{}.

%% @private
command(State) ->
  eqc_gen:oneof
    (
    [{call,java,new,[{call,?MODULE,node_id,[]},'java.util.HashSet',[]]}] ++
    [{call,java,call,[Set,add,[nat()]]} ||
      {Set,_} <- State#state.sets] ++
    [{call,java,call,[Set,contains,[nat()]]} ||
      {Set,_} <- State#state.sets] ++
    [{call,java,call,[Set,remove,[nat()]]} ||
      {Set,_} <- State#state.sets] ++
    [{call,java,call,[Set,size,[]]} ||
      {Set,_} <- State#state.sets]
    ).

%% @private
precondition(State,Call) ->
  case Call of
    {_,_,_,[Set,add,_]} ->
      lists:keymember(Set, 1, State#state.sets);
    {_,_,_,[Set,contains,_]} ->
      lists:keymember(Set, 1, State#state.sets);
    {_,_,_,[Set,remove,_]} ->
      lists:keymember(Set, 1, State#state.sets);
    {_,_,_,[Set,size,_]} ->
      lists:keymember(Set, 1, State#state.sets);
    _ ->
      true
  end.

%% @private
next_state(State,Var,Call) ->
  case Call of
    {_,_,new,_} ->
      State#state{sets=[{Var,sets:new()}|State#state.sets]};
    {_,_,call,[Set,add,Elem]} ->
      {_,ESet} =
	lists:keyfind(Set, 1, State#state.sets),
      NewESet =
	sets:add_element(Elem,ESet),
      State#state
	{sets = lists:keyreplace(Set, 1, State#state.sets, {Set,NewESet})};
    {_,_,call,[Set,remove,Elem]} ->
      {_,ESet} =
	lists:keyfind(Set, 1, State#state.sets),
      NewESet =
	sets:del_element(Elem,ESet),
      State#state
	{sets = lists:keyreplace(Set, 1, State#state.sets, {Set,NewESet})};
    _ ->
      State
  end.

%% @private 
postcondition(State,Call,Result) ->
  case Call of
    {_,_,_,[Set,contains,Elem]} ->
      {_,ESet} = lists:keyfind(Set,1,State#state.sets),
      expect_eq(Call,sets:is_element(Elem,ESet),Result);
    {_,_,_,[Set,size,_]} ->
      {_,ESet} = lists:keyfind(Set,1,State#state.sets),
      expect_eq(Call,sets:size(ESet),Result);
    _ ->
      not_exception(Call,Result)
  end.

expect_eq(Call,Value,Result) ->
  if Value==Result -> true;
     true ->
      io:format
	("~n***~p: expected postcondition value ~s=/=~s~n",
	 [Call,print_value(Value),print_value(Result)]),
      false
  end.

not_exception(Call,{java_exception,Exc}) ->
  io:format("*** Error: call ~p returns a java exception~n",[Call]),
  java:print_stacktrace(Exc),
  io:format("~n"),
  false;
not_exception(_,_) ->
  true.

print_value({java_exception,Obj}) ->
  io_lib:format("exception ~p",[java:getSimpleClassName(Obj)]);
print_value(Object) ->
  case java:is_object_ref(Object) of
    true -> io_lib:format("~p : ~p",[Object,java:getSimpleClassName(Object)]);
    false -> io_lib:format("~p",[Object])
  end.


      
