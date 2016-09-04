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

#### -debug_info
#### -{outdir,Dir}

By default, erlang creates the `.beam` file in the current directory. This option allows you to change it.

#### -export_all

Ignores the `-export` module attribute and exports all functions.

#### -{d,Macro} or {d,Macro,Value}

Defines a macro to be used in the module. By default `Value` is `true`.

#### Example

```
7> compile:file(mymodule, [debug_info, export_all]). {ok,mymodule}
8> c(mymodule, [debug_info, export_all]). {ok,mymodule}
```

Compile flags can be defined from within a module:

```erlang
-compile([debug_info, export_all]).
```

## Defining Macros

Macros can be defined as module attributes:

```erlang
-define(MACRO, some_value).
```

Then can you use the macro as `?MACRO` inside any function of the module.

```erlang
-define(HOUR, 3600). % in seconds
```

Defining a function macro:

```erlang
-define(sub(X,Y), X-Y).
```

### Predefined macros

- `?MODULE`: current module name as atom
- `?FILE`: the filename as string
- `?LINE`: the line number

### Checking if a macro is defined

```erlang
-ifdef(DEBUGMODE).
-define(DEBUG(S), io:format("dbg: "++S)).
-else.
-define(DEBUG(S), ok).
-endif.
```

Another example:

```erlang
-ifdef(TEST).
my_test_function() ->
  run_some_tests().
-endif.
```

## More about modules

### Metadata

`module_info/0` and `module_info/1` return the metadata of the module, for example:

```
9> mymodule:module_info(). [{exports,[{add,2},
                            {hello,0},
                            {greet_and_add_two,1},
                            {module_info,0},
                            {module_info,1}]},
                            {imports,[]},
                            {attributes,[{vsn,[174839656007867314473085021121413256129]}]},
                            {compile,[{options,[]},
                            {version,"4.8"},
                            {time,{2013,2,13,2,56,32}},
                            {source,"/path/mymodule.erl"}]}]
10> mymodule:module_info(attributes). [{vsn,[174839656007867314473085021121413256129]}]
```

### Circular dependencies

Avoid them.
