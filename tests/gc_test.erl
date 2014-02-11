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
  io:format
    ("\nAt start: #(Erlang objects)=~p #(Java objects)=~p~n~n",
     [java:memory_usage(),java:memory_usage(Nid)]),
  Seq =
    lists:map
      (fun (I) -> java:new(Nid,'java.lang.Integer',[I]) end,
       lists:seq(1,J)),
  lists:foreach
    (fun (_) ->
	 spawn
	   (fun () ->
		D = self(),
		B = proxy(N,D,J),
		do_send(B,Seq),
		do_receive(Seq)
	    end)
     end,
     lists:seq(1,M)),
  io:format
    ("\nAt end, before gc: #(Erlang objects)=~p #(Java objects)=~p~n~n",
     [java:memory_usage(),java:memory_usage(Nid)]),
  erlang:garbage_collect(),
  receive after 10000 -> ok end,
  erlang:garbage_collect(),
  io:format
    ("\nAt end, after gc: #(Erlang objects)=~p #(Java objects)=~p~n~n",
     [java:memory_usage(),java:memory_usage(Nid)]).

do_send(_B,[]) ->
  ok;
do_send(B,L) ->
  N = random:uniform(length(L)),
  {Before,Item,After} = split(N,L),
  java:call(Item,intValue,[]),
  B!Item,
  do_send(B,Before++After).

do_receive([]) ->
  ok;
do_receive(L) ->
  receive
    Msg ->
      case lists:any(fun (Obj) -> java:eq(Obj,Msg) end, L) of
	true ->
	  do_receive(lists:filter(fun (Obj) -> not(java:eq(Obj,Msg)) end,L));
	false ->
	  io:format
	    ("Object ~p not in ~p~n",[Msg,L]),
	  throw(bad)
      end
  after
    5000 ->
      io:format("very slow in receiving values~n  ~p~n",[L]),
      throw(bad)
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

  
  
