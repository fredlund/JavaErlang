-module(gc_test).

-compile(export_all).

proxy(Pid,_Nid) ->
   spawn(fun() ->
     receive
       Msg ->  Pid ! java:identity(Msg)
     end
   end).

proxy(0, Pid, _Nid) ->
     Pid;
proxy(N, Pid, Nid) ->
     Proxy = proxy(N-1, Pid, Nid),
     proxy(Proxy, Nid).

start(N,M) ->
  {ok,Nid} = java:start_node([{enable_gc,true}]),
  lists:foreach
    (fun (I) ->
	 D = self(),
	 B = proxy(N, D, Nid),
	 B ! java:new(Nid,'java.lang.Integer',[I]),
	 receive _ -> ok end
     end,
     lists:seq(1,M)).

       
  
  
