-module(java_resource).

-export([create/1, create/2]).

-ifdef(enable_gc).
-on_load(init/0).

init() ->
    PrivDir = code:priv_dir(java_erlang),
    ok = erlang:load_nif(PrivDir++"/java_resource", 0).

-define(NIF_STUB, nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

create(Term) ->
    create(Term, self()).

create(_Term, _Pid) ->
    ?NIF_STUB.

-else. % ifdef(enable_gc).

create(_Term) ->
    undefined.

create(_Term, _Pid) ->
    undefined.

-endif. % ifdef(enable_gc).
