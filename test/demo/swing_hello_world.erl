%% Copyright (c) 2009, Lars-Ake Fredlund
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

%% A simple Swing example using the proxy facilities of JavaErlang.

-module(swing_hello_world).
-export([start/0]).

start() ->
    try
        {ok,N} = java:start_node([{java_verbose,"WARNING"},
				  {enable_proxies,true}]),
        _ActionListenerClass =
            java_proxy:class
              (N, 'myActionListener', 'javax.swing.AbstractAction',
               [{{actionPerformed,['java.awt.event.ActionEvent']},fun actionPerformed/3}]),
        _WindowListenerClass =
            java_proxy:class
              (N, 'myWindowListener', 'java.awt.event.WindowAdapter',
               [{{windowClosed,['java.awt.event.WindowEvent']},fun windowClosed/3}]),
        Frame = java:new(N,'javax.swing.JFrame',[java:list_to_string(N,"HelloWorldSwing")]),
        java:call
          (Frame,setDefaultCloseOperation,
           [java:get_static(N,'javax.swing.JFrame','DISPOSE_ON_CLOSE')]),
        Pane = java:call(Frame,getContentPane,[]),
        Button = java:new(N,'javax.swing.JButton',[]),
        java:call(Button,setAction,[java_proxy:new(N,'myActionListener',false)]),
        java:call(Pane,add,[Button]),
        java:call(Button,setText,[java:list_to_string(N,"Hello World")]),
        java:call(Frame,pack,[]),
        java:call(Frame,addWindowListener,[java_proxy:new(N,'myWindowListener',void)]),
        java:call(Frame,setVisible,[true])
    catch {java_exception,Exc} ->
            erlang:display(erlang:get_stacktrace()),
            java:print_stacktrace(Exc)
    end.

actionPerformed(_Context,Reversed,Event) ->
    io:format("An action was performed!~n",[]),
    String =
        if Reversed -> "Hello World";
           true -> "World Hello"
        end,
    Button = java:call(Event,getSource,[]),
    java:call(Button,setText,[java:list_to_string(java:node_id(Button),String)]),
    {reply,void,not(Reversed)}.

windowClosed(_Context,_State,_Event) ->
    io:format("The main window was closed.~n",[]),
    halt(),
    {reply,void,void}.



