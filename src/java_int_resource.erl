-module(java_int_resource).

-export([is_loaded/0]).
-export([init/0]).
-export([create/2]).

is_loaded() ->
  false.

init() ->
  case ?MODULE:is_loaded() of
    true -> ok;
    false -> 
      PrivDir = code:lib_dir(java_erlang,priv),
      ok = erlang:load_nif(PrivDir++"/java_int_resource",0)
  end.

create(_Term,_Pid) ->
  void.



