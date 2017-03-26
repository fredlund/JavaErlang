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

-export([compute_class/2]).
-export([find_constructor/3,find_constructor_with_type/3]).
-export([find_method/3,find_method_with_type/3]).
-export([find_static_method/4,find_static_method_with_type/4]).
-export([find_field/2]).
-export([find_static_field/3]).

-include("node.hrl").
-include("class.hrl").
-include("tags.hrl").

-include("debug.hrl").

compute_class(NodeId,ClassArg) ->
    ClassName =
        if
	    is_atom(ClassArg) ->
	        ClassArg;
	    is_tuple(ClassArg) ->
	        java:report_java_exception
                  (java:string_to_list(java:call(ClassArg,getName,[])))
        end,
    ClassId =
	java:report_java_exception
	  (lookup_class(NodeId,ClassArg)),
    RawConstructors =
	java:report_java_exception
	  (get_constructors(NodeId,ClassArg,true)),
    ConstructorsWithType = 
	elements_with_type(NodeId,ClassArg,?getConstructor,RawConstructors),
    ConstructorsWithArity = 
	elements_with_arity(NodeId,ClassArg,?getConstructor,RawConstructors),

    RawMethods =
	java:report_java_exception
	  (get_methods(NodeId,ClassArg,false,true)),
    MethodsWithType = 
	elements_with_type(NodeId,ClassArg,?getMethod,RawMethods),
    MethodsWithArity = 
	elements_with_arity(NodeId,ClassArg,?getMethod,RawMethods),

    RawStaticMethods =
	java:report_java_exception
	  (get_methods(NodeId,ClassArg,true,true)),
    StaticMethodsWithType = 
	elements_with_type(NodeId,ClassArg,?getMethod,RawStaticMethods),
    StaticMethodsWithArity = 
	elements_with_arity(NodeId,ClassArg,?getMethod,RawStaticMethods),

    RawFields =
	java:report_java_exception
	  (get_fields(NodeId,ClassArg,false,true)),
    FieldsWithArity = 
	elements_with_arity(NodeId,ClassArg,?getField,RawFields),
    
    RawStaticFields =
	java:report_java_exception
	  (get_fields(NodeId,ClassArg,true,true)),
    StaticFieldsWithArity = 
	elements_with_arity(NodeId,ClassArg,?getField,RawStaticFields),

    #class{
       name=ClassName,
       node_id=NodeId,
       id=ClassId,
       constructors={ConstructorsWithType,ConstructorsWithArity},
       methods={MethodsWithType,MethodsWithArity},
       static_methods={StaticMethodsWithType,StaticMethodsWithArity},
       fields=FieldsWithArity,
       static_fields=StaticFieldsWithArity
      }.

elements_with_type(NodeId,ClassName,Getter,Elements) ->
    lists:map
      (fun ({Name,TypeList}) ->
               Element =
                   java:javaCall
                     (NodeId,Getter,{ClassName,Name,list_to_tuple(TypeList)}),
               {{Name,TypeList},Element}
       end, Elements).

elements_with_arity(NodeId,ClassName,Getter,Elements) ->
    lists:foldl
      (fun ({Name,TypeList},Acc) ->
               Arity =
                   length(TypeList),
               Element =
                   java:javaCall
                     (NodeId,Getter,{ClassName,Name,list_to_tuple(TypeList)}),
               add_to_elements_with_arity(Name,Arity,TypeList,Element,Acc)
       end, [], Elements).

find_element_with_type(Key,RawElements) ->
    case lists:keyfind(Key,1,RawElements) of
        {_,Element} -> {ok,Element};
        _ -> false
    end.

find_element_without_type(NodeId,Key,Args,RawElements) ->
    case lists:keyfind(Key,1,RawElements) of
        {_,Elements} ->
            case Elements of
                [{_,Element}] ->
                    {ok,Element};
                _ ->
                    TupleArgs = list_to_tuple(Args),
                    case type_compatible_alternatives(NodeId,TupleArgs,Elements) of
                        {ok,{_,Element}} -> {ok,Element};
                        false -> false
                    end
            end;
        _ -> false
    end.

add_to_elements_with_arity(N,A,T,E,[]) ->
    [{{N,A},[{T,E}]}];
add_to_elements_with_arity(N,A,T,E,[{{N,A},Cs}|Rest]) ->
    [{{N,A},[{T,E}|Cs]}|Rest];
add_to_elements_with_arity(N,A,T,E,[CA|Rest]) ->
    [CA|add_to_elements_with_arity(N,A,T,E,Rest)].

type_compatible_alternatives(Node,Objs,Alternatives) ->
    tca(Node,Objs,Alternatives).

tca(_Node,_Params,[]) -> false;
tca(Node,Params,[Alternative={Types,_}|Rest]) ->
    ?LOG("Types=~p Params=~p~n",[Types,Params]),
    Result = java:javaCall(Node,?objTypeCompat,{list_to_tuple(Types),Params}),
    if
        Result ->
            java:format(debug,"Params ~p matches ~p~n",[Params,Types]),
            {ok,Alternative};
        true ->
            java:format(debug,"Params ~p do not match ~p~n",[Params,Types]),
            tca(Node,Params,Rest)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_class(NodeId,ClassName) ->
    java:javaCall(NodeId,?lookupClass,ClassName).
get_constructors(NodeId,ClassName,ObserverInPackage) ->
    java:javaCall(NodeId,?getConstructors,{ClassName,ObserverInPackage}).
get_methods(NodeId,ClassName,Static,ObserverInPackage) ->
    java:javaCall
      (NodeId,
       ?getMethods,
       {ClassName,Static,ObserverInPackage}).
%% get_classes(NodeId,ClassName,ObserverInPackage) ->
%%   java:javaCall(NodeId,?getClasses,{ClassName,ObserverInPackage}).
get_fields(NodeId,ClassName,Static,ObserverInPackage) ->
    java:javaCall
      (NodeId,
       ?getFields,
       {ClassName,Static,ObserverInPackage}).

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

find_constructor(NodeId,ClassName,Args) ->
    Class = java:acquire_class(NodeId,ClassName),
    LenArgs = length(Args),
    case
        find_element_without_type
        (NodeId,{ClassName,LenArgs},Args,element(2,Class#class.constructors)) of
        {ok,Constructor} -> Constructor;
        false ->
            java:format
              (warning,
               "*** Warning: cannot find constructor for class ~p with arity ~p "++
                   "that matches arguments ~s~n",
               [ClassName,LenArgs,print_parameters(Args)]),
            throw(badarg)
    end.

find_constructor_with_type(NodeId,ClassName,ArgTypes) ->
    Class = java:acquire_class(NodeId,ClassName),
    case
        find_element_with_type
        ({ClassName,ArgTypes},element(1,Class#class.constructors)) of
        {ok,Constructor} -> Constructor;
        false ->
            java:format
              (warning,
               "*** Warning: cannot find constructor for class ~p with types ~s~n",
               [ClassName,print_parameters(ArgTypes)]),
            throw(badarg)
    end.

find_method(Object,Name,Args) ->
    Class = java:find_class(Object),
    LenArgs = length(Args),
    case
        find_element_without_type
        (java:node_id(Object),{Name,LenArgs},Args,element(2,Class#class.methods)) of
        {ok,Method} -> Method;
        false ->
            java:format
              (warning,
               "*** Warning: cannot find method ~p for class ~p with arity ~p "++
                   "that matches arguments ~s~n",
               [Name,Class#class.name,LenArgs,print_parameters(Args)]),
            throw(badarg)
    end.

find_method_with_type(Object,Name,ArgTypes) ->
    Class = java:find_class(Object),
    case
        find_element_with_type
        ({Name,ArgTypes},element(1,Class#class.methods)) of
        {ok,Method} -> Method;
        false ->
            java:format
              (warning,
               "*** Warning: cannot find method ~p for class ~p with types ~s~n",
               [Name,Class#class.name,print_parameters(ArgTypes)]),
            throw(badarg)
    end.

find_static_method(NodeId,ClassName,Name,Args) ->
    Class = java:acquire_class(NodeId,ClassName),
    LenArgs = length(Args),
    case
        find_element_without_type
        (NodeId,{Name,LenArgs},Args,element(2,Class#class.static_methods)) of
        {ok,Static_Method} -> Static_Method;
        false ->
            java:format
              (warning,
               "*** Warning: cannot find static method ~p for class ~p with arity ~p "++
                   "that matches arguments ~s~n",
               [Name,ClassName,LenArgs,print_parameters(Args)]),
            throw(badarg)
    end.

find_static_method_with_type(NodeId,ClassName,Name,ArgTypes) ->
    Class = java:acquire_class(NodeId,ClassName),
    case
        find_element_with_type
        ({Name,ArgTypes},element(1,Class#class.static_methods)) of
        {ok,Static_Method} -> Static_Method;
        false ->
            java:format
              (warning,
               "*** Warning: cannot find static method for class ~p with types ~s~n",
               [ClassName,print_parameters(ArgTypes)]),
            throw(badarg)
    end.

find_field(Object,Name) ->
    Class = java:find_class(Object),
    case
        find_element_without_type
        (java:node_id(Object),{Name,1},[],Class#class.fields) of
        {ok,Field} -> Field;
        false ->
            java:format
              (warning,
               "*** Warning: cannot find field ~p for class ~p~n",
               [Name,Class#class.name]),
            throw(badarg)
    end.

find_static_field(NodeId,ClassName,Name) ->
    Class = java:acquire_class(NodeId,ClassName),
    case
        find_element_without_type
        (NodeId,{Name,1},[],Class#class.static_fields) of
        {ok,Static_Field} -> Static_Field;
        false ->
            java:format
              (warning,
               "*** Warning: cannot find static field ~p for class ~p~n",
               [Name,Class#class.name]),
            throw(badarg)
    end.







