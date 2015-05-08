-ifdef(debug).
-define(LOG(X,Y),
        java:format(debug,"{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.
