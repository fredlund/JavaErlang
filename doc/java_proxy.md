

# Module java_proxy #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


This module implements a facility for creating Java object
with an implementation in Erlang, using the [Javassist](http://www.csg.ci.i.u-tokyo.ac.jp/~chiba/javassist/) byte code manipulation library.
Copyright (c) 2014 Lars-Ake Fredlund

__Authors:__ [`Lars-Ake Fredlund (lfredlund@fi.upm.es)`](mailto:Lars-Ake Fredlund (lfredlund@fi.upm.es)).

<a name="types"></a>

## Data Types ##




### <a name="type-reply">reply()</a> ###



<pre><code>
reply() = {reply, <a href="java.md#type-value">java:value()</a>, any()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#class-4">class/4</a></td><td></td></tr><tr><td valign="top"><a href="#class-5">class/5</a></td><td>Creates a new proxy class with the given name and superclass.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Creates a new instance of a proxy class.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="class-4"></a>

### class/4 ###


<pre><code>
class(NodeId::<a href="java.md#type-node_id">java:node_id()</a>, Name::atom(), SuperClassName::atom(), MethodFuns::[{[{atom(), <a href="java.md#type-type">java:type()</a>}], fun((...) -&gt; <a href="#type-reply">reply()</a>)}]) -&gt; <a href="java.md#type-obj_ref">java:obj_ref()</a>
</code></pre>
<br />


<a name="class-5"></a>

### class/5 ###


<pre><code>
class(NodeId::<a href="java.md#type-node_id">java:node_id()</a>, Name::atom(), SuperClassName::atom(), MethodFuns::[{[{atom(), <a href="java.md#type-type">java:type()</a>}], fun((...) -&gt; <a href="#type-reply">reply()</a>)}], DefaultInit::any()) -&gt; <a href="java.md#type-obj_ref">java:obj_ref()</a>
</code></pre>
<br />

Creates a new proxy class with the given name and superclass.
A method of that class is handled by including in the list in the last
argument a tuple `{Methodname,Types,Fun}`
where ''Methodname'' is the atom corresponding to a method in the class,
`Types` is a list corresponding to the types of the arguments to the method,
and `Fun` is a function of arity `length(Types)+2` which provides the
implementation of the method. Any method not listed is handled by the superclass.
The function implementing the method receives a first argument a
binary tuple where the first element is the Java object on which the method was
invoked, and the second element is the Java representation of the method invoked.
The second argument is the state of the (Erlang) object, which is kept between
successive invocations of methods of the object.
The rest of the arguments correspond to the arguments of the method.
The function should return a tuple `{reply,Result,NewState}`
where `Result` is the result of the method call, and `NewState`
is the new (Erlang) object state.
An example:<br />

```
  _ActionListenerClass =
     java_proxy:class
     (N,
      'myActionListener',
      'javax.swing.AbstractAction',
      [{{actionPerformed,['java.awt.event.ActionEvent']},fun actionPerformed/3}]).
 actionPerformed(_Context,NumCalls,Event) ->
   io:format("An action was performed!~n",[]),
   ..
   {reply,void,NumCalls+1}.
```

creates a new Java class that handles invocations of `actionPerformed`
(typically in a Java Swing application).
<a name="new-2"></a>

### new/2 ###


<pre><code>
new(NodeId::<a href="java.md#type-node_id">java:node_id()</a>, Name::atom()) -&gt; <a href="java.md#type-obj_ref">java:obj_ref()</a>
</code></pre>
<br />


<a name="new-3"></a>

### new/3 ###


<pre><code>
new(NodeId::<a href="java.md#type-node_id">java:node_id()</a>, Name::atom(), Init::any()) -&gt; <a href="java.md#type-obj_ref">java:obj_ref()</a>
</code></pre>
<br />

Creates a new instance of a proxy class.
The third argument corresponds to the initial state of the (Erlang) object created.
An example:<br />

```
  java_proxy:new(N,'myActionListener',0).
```

