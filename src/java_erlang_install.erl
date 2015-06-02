%% Copyright (c) 2011, Lars-Ake Fredlund
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
%% Install JavaErlang in the Erlang lib directory.  This program
%% should be run in the root directory of a JavaErlang distribution,
%% which ought to contain a java_erlang-xxx directory.
%% Inspired by eqc_install for the QuickCheck tool
%% (thanks to John Hughes for his kind assistance).

-module(java_erlang_install).
-export([install/0,install/3]).

install() ->
    Erlang = code:where_is_file("erlang.beam"),
    Ebin = filename:dirname(Erlang),
    Erts = filename:dirname(Ebin),
    Lib = filename:dirname(Erts),
    ThisModule = code:where_is_file("java_erlang_install.beam"),
    ThisModuleLocation = filename:dirname(filename:dirname(ThisModule)),
    Version = find_version(),
    install(Version,ThisModuleLocation,Lib).

install(Version,BuildDir,Lib) ->
    io:format("Installation program for JavaErlang.~n~n",[]),
    ToDir = Lib++"/java_erlang-"++Version,
    if
	BuildDir == ToDir ->
	    io:format("*** Error: source and destination are the same~n"),
	    throw(bad);
	true ->
	    ok
    end,
    ToDelete = conflicts(ToDir),
    io:format("This will install ~s~nin the directory ~s~n",[Version,Lib]),
    if
	ToDelete=/=[] ->
	    io:format
	      ("This will delete conflicting versions of JavaErlang, namely\n"++
		   "    ~p\n",
	       [ToDelete]);
	true ->
	    ok
    end,
    case io:get_line("Proceed? ") of
	"y\n" ->
	    delete_conflicts(ToDelete),
	    install(BuildDir,ToDir);
	_ ->
	    io:format("Cancelling install--answer \"y\" at this point to proceed.\n"),
	    throw(installation_cancelled)
    end.

conflicts(ToDir) ->
    case file:read_file_info(ToDir) of
	{ok,_} ->
	    [ToDir];
	_ ->
	    []
    end.

install(From,ToDir) ->
    copy_java_erlang(From,ToDir),
    io:format("JavaErlang is installed successfully.\n",[]),
    code:add_paths([ToDir++"/ebin"]).

find_version() ->
    ok = application:ensure_started(java_erlang),
    java:version().

copy_java_erlang(From,ToDir) ->
    case copy(From,ToDir) of
	ok ->
	    ok;
	eaccess ->
	    io:format
	      ("*** Error: failed to copy JavaErlang -- "++
		   "rerun as Administrator or superuser?\n",
	       []),
	    exit(eaccess);
	{error,eaccess} ->
	    io:format
	      ("*** Error: failed to copy JavaErlang -- "++
		   "rerun as Administrator or superuser?\n",
	       []),
	    exit(eaccess);
	Error ->
	    io:format
	      ("*** Error: failed to copy JavaErlang -- "++
		   "copy returned~n~p??~n",
	       [Error]),
	    exit(Error)
    end.

copy(From,To) ->
    case file:list_dir(From) of
	{ok,Files} ->
	    case file:make_dir(To) of
		ok ->
		    lists:foldl
		      (fun (File,ok) ->
			       FromFile = From++"/"++File,
			       ToFile = To++"/"++File,
			       copy(FromFile,ToFile);
			   (_,Status) ->
			       Status
		       end, ok, Files);
		OtherMkDir -> 
		    io:format
		      ("*** Error: failed to create directory ~s due to ~p~n",
		       [To,OtherMkDir]),
		    OtherMkDir
	    end;
	_ -> 
	    case file:copy(From,To) of
		{ok,_} -> ok;
		OtherCopy -> 
		    io:format
		      ("*** Error: failed to copy ~s to ~s due to ~p~n",
		       [From,To,OtherCopy]),
		    OtherCopy
	    end
    end.

delete_conflicts(ToDelete) ->
    lists:foreach
      (fun (Version) ->
	       delete_recursive(Version)
       end, ToDelete).

delete_recursive(F) ->
    case file:list_dir(F) of
	{ok,Files} ->
	    lists:foreach
	      (fun (File) -> delete_recursive(F++"/"++File) end,
	       Files),
	    case file:del_dir(F) of
		ok ->
		    ok;
		Err ->
		    io:format
		      ("*** Error: could not delete directory ~s: ~p\n",
		       [F,Err]),
		    Err
	    end;
	_ ->
	    case file:delete(F) of
		ok ->
		    ok;
		Err ->
		    io:format
		      ("*** Error: could not delete file ~s: ~p\n",
		       [F,Err]),
		    Err
	    end
    end.

