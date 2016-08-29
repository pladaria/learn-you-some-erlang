# 2. Modules

A module is a bunch of functions grouped together in a single  le, under a single name.

`erlang module is automatically imported

```
1> erlang:element(2, {a,b,c}). b
2> element(2, {a,b,c}).
b
3> lists:seq(1,4).
[1,2,3,4]
4> seq(1,4).
** exception error: undefined shell command seq/2
```

A module contains _attributes_ (metadata about the module) and _functions_.

All module attributes follow the form `-Name(Attribute).`.

## Module example

```erlang
%%% Module general comment
%%% License, author, etc

%% Module name
-module(helloworld).

%% Exports

-export([add/2, hello/0, greet_and_add_two/1]).

add(A,B) ->
    A + B.

%% Shows greetings.
%% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello, world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X,2).
```

## Compile

From the command line

```bash
erlc flags file.erl
```

From the repl

```erlang
compile:file(Filename)
% or
c()
```

```
1> cd("/path/to/where/you/saved/the-module/").
"Path Name to the directory you are in"
ok
2> c(helloworld). {ok,helloworld}
3> helloworld:add(7,2). 9
4> helloworld:hello(). Hello, world!
ok
5> helloworld:greet_and_add_two(-3).
Hello, world!
-1
6> helloworld:not_a_real_function().
** exception error: undefined function helloworld:not_a_real_function/0
```

### Compiler options

