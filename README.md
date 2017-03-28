

# Welcome to the JavaErlang interface. #

__Authors:__ [Lars-Åke Fredlund](lfredlund@fi.upm.es).
JavaErlang is a library that attempts to facilitate the
communication between Java and Erlang nodes, implemented
as an additional layer on top
of the JInterface Java interface.
The library makes heavy use of the Java reflection capability,
provided in the package java.lang.reflect.


The primary functionality is that the library provides
functions for creating and interacting with Java objects
without the need to write any boilerplate code.




An example:<br />

```
$ erl -sname anyname
> application:ensure_started(java_erlang).
> {ok,NodeId} = java:start_node().
> Int10 = java:new(NodeId,'java.lang.Integer',[10]).
> String10 = java:call(Int10,toString,[]).
```





The above code starts a Java node with a specified name (note that the name itself does not matter, just that we start erlang with the `-sname` or `-name` option), and establishes
a connection to it. Then, a Java integer storing the value 10 is created,
and finally a Java string representing "10" is returned
from the Java integer just created.




To make your Java classes (and Jar files) visible to the library
the option `add_to_java_classpath` should be provided to
`java:start_node/1`. An example:<br />

```
{ok,NodeId} = java:start_node([{add_to_java_classpath,["classes"]}]).
```

This adds the directory `classes`
to the classpath of the started Java interpreter



## Build, Test, and Generate Markdown Docs ##


JavaErlang requires [rebar3](http://www.rebar3.org) for
building and testing.  See [here](http://www.rebar3.org/v3.0/docs/getting-started) for
getting started with rebar3.




To compile, execute the following command:<br />
```
$ rebar3 compile
```


After compilation Erlang beam files will be left in the
directory _build/default/lib/java_erlang/ebin/.



Should you wish to install the JavaErlang library in the standard
Erlang library structure, the following commands can be used:<br />
```
$ erl -pa _build/default/lib/java_erlang/ebin/

> java_erlang_install:install().
```
</p>

<p>
To test, execute the following command:<br />
```
$ rebar3 eunit -v
```


If the eunit tests timeout, try re-running the test to see if the
timeout repeats.



To generate Markdown docs, execute the following command:<br />

```
$ env ERL_LIBS=$PWD/_build/default/lib/edown rebar3 edoc
```




## Examples ##


The `test` directory contains a number of examples. The subdirectory
`qc` contains a simple Quviq QuickCheck test model for the Java `HashSet`
class -- if QuickCheck is installed the class can be verified by
invoking the `hashset_test:test()` function. The `demo` subdirectory
contains a simple example of using the Swing GUI library from Erlang.



## News ##

The library now provides a facility to construct
Java classes using Erlang, see the `java_proxy`
module for details.
To disable this
(the option is enabled by default)
provide `{enable_proxies,false}`
as an option to `start_node/2`.


Garbage collection is now implemented. To enable this
(the option is currently disabled by default, but the default
can change without warning)
provide `{enable_gc,true}`
as an option to `start_node/2`.




The code for generating an Erlang module corresponding to
a Java class has been removed in this version of the library.
Thus, Java objects can be created and accessed only
using the functions exported from the `java` module.




Java nodes can now be started on remote Erlang nodes --
see the options for the `java:start_node/1` function.



## Source code and precompiled libraries ##

The source code of the library is available on GitHub
at [
`git://github.com/fredlund/JavaErlang.git`](git://github.com/fredlund/JavaErlang.git).


Precompiled versions of the library are
[
available too](http://babel.ls.fi.upm.es/~fred/JavaErlang/downloads.md).



## Java Values ##

The Java null value is represented as the Erlang atom `null`,
the Java boolean values true and false are represented as the
corresponding atoms `true` and `false`,
and integer-like types and floating-point-like types are
represented as normal Erlang integers and floats. Arrays are
constructed using the normal Erlang syntax for lists and strings.
Values can be explicitely type cast using the notation
`{Type,Value}`. For example, `{short,5}`, `{char,$a}`,
`{{array,char,1},"Hello World"}`.
Java one-dimensional arrays can also be constructed
using the funcion `java:list_to_array/3`.
Examples:<br />

```
> {ok,NodeId} = java:start_node().
{ok,9231}
> False = java:new(NodeId,'java.lang.Boolean',[false]).
{object,0,9231}
> HelloWorldString = java:new(NodeId,'java.lang.String',[java:list_to_array(NodeId,"Hello World!",char)]).
{object,2,9231}
> Zero = java:new(NodeId,'java.lang.Integer',[0]).
{object,3,9232}
> java:call(Zero,intValue,[]).
0
> java:string_to_list(java:new(NodeId,'java.lang.String',[{{array,char,1},"Hello World"}])).
"Hello World"
> java:string_to_list(java:new(NodeId,'java.lang.String',["Hello World"])).
"Hello World"
```



## Boxing and Unboxing ##

Boxing and unboxing of primitive method and constructor arguments is done by
the library. Examples:

```
> {ok,NodeId} = java:start_node().
{ok,9231}
> Zero = java:new(NodeId,'java.lang.Integer',[0]).
{object,3,9232}
> java:call(Zero,equals,[0]).
true
> java:call(Zero,equals,[2]).
false
> java:call(Zero,equals,[0.0]).
false
```



Primitive values returned from a method are represented as normal Erlang
values, whereas objects are returned as objects.

```
> {ok,N} = java:start_node().
{ok,923}
> Zero = java:new(N,'java.lang.Integer',[0]).
{object,3,923}
> java:call(Zero,intValue,[]).
0
```

Since intValue returnes an int, a primitive type, the library lets
the corresponding Erlang function return an Erlang integer.




## Java Exceptions ##

A Java exceptions Exc is manifest as an Erlang exception
{java_exception,Exc}. Example:<br />

```
  {ok,NodeId} = java:start_node(),
  try java:new(NodeId,'hola',[])
  catch {java_exception,Exc} ->
      io:format("Exception is of type ~p~n",[java:getClassName(Exc)]),
      java:print_stacktrace(Exc)
  end.
```


The option `java_exception_as_value` (which can be passed
as an argument to `java:start_node/1`) determines whether Java exceptions are
indeed returned as exceptions (the default), as shown in code excerpt above,
or whether they are returned as Erlang values (when the option is set
to true).


## Creating Java Objects ##


An object of class "c" can be
created by calling `java:new(NodeId,c,Args)`,
where `Args` is the list
of arguments of the constructor.
An example:<br />

```
{ok,NodeId} = java:start_node(),
I2 = java:new(NodeId,'java.lang.Integer',[2]).
```


## Calling Java Instance and Static Methods ##


A public method "m" of a Java object "o" of class "c" can be
called using the function call
`java:call(o,m,Args)` (or `java:get_static(c,m,Args)` if it is
a static, i.e., class method) where `Args` is the list
of arguments of the method.
An example:<br />

```
{ok,NodeId} = java:start_node(),
I2 = java:new(NodeId,'java.lang.Integer',[2]),
I2b = java:new(NodeId,'java.lang.Integer',[2]),
true = java:call(I2,equals,[I2b]).
```

This code excerpt creates two Java Integers (of value 2),
and checks that the method `equals` returns true.


## Accessing Java fields ##


A public field "f" of a Java object "o" of class "c" can be
accessed using the function call
`java:get(o,f)` (or `java:get_static(c,f)` if it is a class field).
Similarly, the call `java:set(o,f,v)` is used to assign the value
`v` to the field (and `java:set_static(o,f,v)` is used
for class fields).
An example:<br />

```
{ok,NodeId} = java:start_node(),
Err = java:get_static(NodeId,'java.lang.System',err),
java:call(Err,println,[{int,2}]).
```


This code excerpt retrieves the Java standard error stream from
the field `err` in `java.lang.System`, and prints
the integer 2.


## Visibility of Java Constructor, Methods and Fields ##


Currently only the public methods and fields of Java classes
are accessible. It is likely that in the future we will
provide an option to permit calling methods that are
`protected`, i.e., methods callable only from inside a Java package.


## Java Nodes ##

A Java node is created by calling the function
java:start_node/0 or java:start_node/1.
The JavaErlang interface supports communication
between one Erlang node and multiple Java nodes,
i.e., java:start_node can be called multiple times
from the same Erlang process, and the resulting Java
nodes are completely separated. Naturally, a Java object residing on
one Java node should not be communicated to a different Java
node.


## Architecture ##

A Java node identifier is an Erlang node (global) resource. That
is, an Erlang process P1 can pass a Java node identifier to another
Erlang process P2, and then P2 can also call Java code on
that Java node.


### Java Threads ###

There is a 1-1 mapping between Erlang processes and Java threads.
That is, an Erlang process making a call to Java
will not block because another Erlang
process is making a Java call.


### Timeouts ###

If the Java code fails to respond to a call from Erlang,
and a time limit has been set,
an exception `java_timeout` will be thrown.
Note that timeouts can be set on a per-process, per-call basis, using
the function `set_timeout/1`.


## Licencing ##

The source code for the JavaErlang library is generally licensed
under the modified 3-clause BSD software license. See individual files
for detailed licensing conditions or exceptions.


## Limitations ##

The library has a number of practical limitations:

* Speed: the scheme used for communicating between Erlang and
Java is quite slow.

* Inability to use the calling context of a Java call to determine
which Java method to invoke.



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/fredlund/JavaErlang/blob/master/doc/java.md" class="module">java</a></td></tr>
<tr><td><a href="https://github.com/fredlund/JavaErlang/blob/master/doc/java_erlang_app.md" class="module">java_erlang_app</a></td></tr>
<tr><td><a href="https://github.com/fredlund/JavaErlang/blob/master/doc/java_erlang_install.md" class="module">java_erlang_install</a></td></tr>
<tr><td><a href="https://github.com/fredlund/JavaErlang/blob/master/doc/java_erlang_sup.md" class="module">java_erlang_sup</a></td></tr>
<tr><td><a href="https://github.com/fredlund/JavaErlang/blob/master/doc/java_proxy.md" class="module">java_proxy</a></td></tr>
<tr><td><a href="https://github.com/fredlund/JavaErlang/blob/master/doc/java_resource.md" class="module">java_resource</a></td></tr>
<tr><td><a href="https://github.com/fredlund/JavaErlang/blob/master/doc/java_to_erlang.md" class="module">java_to_erlang</a></td></tr></table>

