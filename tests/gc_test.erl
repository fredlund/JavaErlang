-module(gc_test).

-compile(export_all).

proxy(Pid, M) ->
  spawn(fun () -> do_receive_forward(Pid,M) end).

proxy(0, Pid, _M) ->
     Pid;
proxy(N, Pid, M) ->
     Proxy = proxy(N-1, Pid, M),
     proxy(Proxy, M).

start(N,M,J) ->
  {ok,Nid} = java:start_node([{enable_gc,true}]),
  Seq =
    lists:map
      (fun (I) -> java:new(Nid,'java.lang.Integer',[I]) end,
       lists:seq(1,J)),
  lists:foreach
    (fun (_) ->
	 D = self(),
	 B = proxy(N, D, M),
	 do_send(B,Seq),
	 do_receive(M)
     end,
     lists:seq(1,M)).

do_send(_B,[]) ->
  ok;
do_send(B,L) ->
  N = random:uniform(length(L)),
  {Before,Item,After} = split(N,L),
  B!Item,
  do_send(B,Before++After).

do_receive(0) ->
  ok;
do_receive(N) when is_integer(N), N>0 ->
  receive
    _ -> do_receive(N-1)
  end.

do_receive_forward(_P,0) ->
  ok;
do_receive_forward(P,N) when is_integer(N), N>0 ->
  receive
    Msg -> P!java:identity(Msg), do_receive_forward(P,N-1)
  end.

split(I,L) ->
  split(I,L,[]).
split(1,[First|Rest],Seen) ->       
  {lists:reverse(Seen),First,Rest};
split(N,[First|Rest],Seen) when is_integer(N),N>0 -> 
  split(N-1,Rest,[First|Seen]).

  
  
