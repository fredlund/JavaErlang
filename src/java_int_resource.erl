-module(java_int_resource).

-export([init/0]).
-export([create/2]).

init() ->
  PrivDir = code:lib_dir(java_erlang,priv),
  ok = erlang:load_nif(PrivDir++"/java_int_resource",0).

create(_Term,_Pid) ->
  void.



