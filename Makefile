compile:
	rebar3 compile

install:
	make compile
	make doinstall

doinstall:
	erl -pa _build/default/lib/java_erlang/ebin/ -noshell -run java_erlang_install install -run erlang halt
