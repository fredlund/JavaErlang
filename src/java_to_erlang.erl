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

%% @author Lars-Ake Fredlund (lfredlund@fi.upm.es)
%% @copyright 2011 Lars-Ake Fredlund
%%

-module(java_to_erlang).
%%-compile(export_all).

-export([get_class/3]).
-export([call_constructor_with_type/4,call_constructor_without_type/3]).

-include("classinfo.hrl").

-record(comp_info,{type,className,methodConstructor,methodFieldAccess,methodAccess,genMethodFun,methodFun,node_id}).

-include("debug.hrl").

ensure_string(Atom) when is_atom(Atom) ->
  atom_to_list(Atom);
ensure_string(List) when is_list(List) ->
  List.

get_class(NodeId,ClassName,ClassInfo) when is_list(ClassName) ->
  get_class(NodeId,list_to_atom(ClassName),ClassInfo);
get_class(NodeId,ClassName,ClassInfo) when is_atom(ClassName) ->
  FinalClassName = java:classname(ClassName,NodeId),
  {get_class_constructors_with_type
     (NodeId,ClassName,FinalClassName,ClassInfo#class_info.constructors),
   get_class_constructors_with_arity
     (NodeId,ClassName,FinalClassName,ClassInfo#class_info.constructors)}.

get_class_constructors_with_type(NodeId,ClassName,FinalClassName,Constructors) ->
  SimpleClassName = java:finalComponent(ClassName),
  lists:map
    (fun ({_,_,TypeList}) ->
	 {Constructor,_Arity} =
	   java:javaCall
	     (NodeId,getConstructor,{ClassName,list_to_tuple(TypeList)}),
	 {TypeList,Constructor}
     end, Constructors).

call_constructor_with_type(NodeId,Types,Args,RawConstructors) ->
  case lists:keyfind(Types,1,RawConstructors) of
    {_,Constructor} ->
      java:javaCall(NodeId,call_constructor,{Constructor,list_to_tuple(Args)})
  end.
  
get_class_constructors_with_arity(NodeId,ClassName,FinalClassName,Constructors) ->
  SimpleClassName = java:finalComponent(ClassName),
  lists:foldl
    (fun ({_,_,TypeList},Acc) ->
	 {Constructor,Arity} =
	   java:javaCall
	     (NodeId,getConstructor,{ClassName,list_to_tuple(TypeList)}),
	 add_to_constructors({TypeList,Constructor},Arity,Acc)
     end, [], Constructors).

call_constructor_without_type(NodeId,Args,RawConstructors) ->
  case lists:keyfind(length(Args),1,RawConstructors) of
    {_,Constructors} ->
      case Constructors of
	[{_,Constructor}] ->
	  java:javaCall
	    (NodeId,call_constructor,{Constructor,list_to_tuple(Args)});
	_ ->
	  TupleArgs = list_to_tuple(Args),
	  case type_compatible_alternatives(NodeId,TupleArgs,Constructors) of
	    {ok,{_,Constructor}} ->
	      java:javaCall
		(NodeId,call_constructor,{Constructor,TupleArgs});
	    false ->
	      java:format
		(warning,
		 "*** Warning: no constructor/method with arity ~p found, "
		 "which accepts parameters ~s~n",
		 [length(Args),print_parameters(Args)]),
	      throw(badarg)
	  end
      end;
    _ ->
      java:format
	(warning,
	 "*** Warning: no constructor/method with arity ~p found, "
	 "which accepts parameters ~s~n",
	 [length(Args),print_parameters(Args)]),
      throw(badarg)
  end.	      

type_compatible_alternatives(Node,Objs,Alternatives) ->
  tca(Node,Objs,Alternatives).

tca(_Node,Params,[]) -> false;
tca(Node,Params,[Alternative={Types,_}|Rest]) ->
  ?LOG("Types=~p Params=~p~n",[Types,Params]),
  Result = java:javaCall(Node,objTypeCompat,{list_to_tuple(Types),Params}),
  if
    Result -> 
      ?LOG("Params ~p matches ~p~n",[Params,Types]),
      {ok,Alternative};
    true ->
      ?LOG("Params ~p do not match ~p~n",[Params,Types]),
      tca(Node,Params,Rest)
  end.

add_to_constructors(C,A,[]) ->
  [{A,[C]}];
add_to_constructors(C,A,[{A,Cs}|Rest]) ->
  [{A,[C|Cs]}|Rest];
add_to_constructors(C,A,[CA|Rest]) ->
  [CA|add_to_constructors(C,A,Rest)].


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


  
