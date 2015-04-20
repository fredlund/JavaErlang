-module(java_resource).

-export([create/1, create/2]).
-on_load(init/0).

-define(NIF_STUB, nif_stub_error(?LINE)).

init() ->
    PrivDir = code:priv_dir(java_erlang),
    ok = erlang:load_nif(PrivDir++"/java_int_resource", 0).

create(Term) ->
    create(Term, self()).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

create(_Term, _Pid) ->
    ?NIF_STUB.

