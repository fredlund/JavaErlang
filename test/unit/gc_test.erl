%% Copyright (c) 2014, Lars-Ake Fredlund
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
%% @copyright 2014 Lars-Ake Fredlund
%%

%% An example to test the garbage collection handling in JavaErlang.

-module(gc_test).

-export([start/3]).

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
    do_test(Nid,N,M,J).

do_test(Nid,N,M,J) ->
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
    receive after 1000 -> ok end,
    io:format
      ("\nAt end, after gc: #(Erlang objects)=~p #(Java objects)=~p~n~n",
       [java:memory_usage(),java:memory_usage(Nid)]).

do_send(_B,[]) ->
    ok;
do_send(B,L) ->
    N = rand:uniform(length(L)),
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



