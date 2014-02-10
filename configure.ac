AC_INIT([javaErlang],[1.1],[lfredlund@fi.upm.es])

AC_PREREQ([2.64])  

AC_COPYRIGHT([Copyright (C) 2006-2014 Lars-Åke Fredlund]) 

AC_ERLANG_NEED_ERLC

AC_ERLANG_CHECK_LIB([jinterface],
     [echo "jinterface version \"$ERLANG_LIB_VER_jinterface\""
      echo "is installed in \"$ERLANG_LIB_DIR_jinterface\""],
     [AC_MSG_ERROR([stdlib was not found!])])

AC_CONFIG_FILES([ \
        Makefile \
        ]) 

AC_OUTPUT 
