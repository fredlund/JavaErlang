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

-module(translate_java_to_erlang).
%%-compile(export_all).

-export([get_class/3]).

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
  get_class_constructors
    (NodeId,ClassName,FinalClassName,ClassInfo#class_info.constructors).

get_class_constructors(NodeId,ClassName,FinalClassName,Constructors) ->
  SimpleClassName = java:finalComponent(ClassName),
  Constructors.
