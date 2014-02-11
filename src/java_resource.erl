-module(java_resource).

-export([init/0]).
-export([create/1,create/2]).

init() ->
  java_int_resource:init().

create(Term) ->
  java_int_resource:create(Term,self()).

create(Term,Pid) ->
  java_int_resource:create(Term,Pid).

	


		    

