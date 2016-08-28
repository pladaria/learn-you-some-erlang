# 1. Starting out

## Run the shell

    erl

### Shell

- CTRL-A: move cursor to beginning of the line
- CTRL-E: move cursor to the end of the line
- TAB autocompletion. Eg: `li<TAB>`
- Exit with `q()` which is a shorthand of `init:stop()`
- CTRL-G: abort shell
    - then `h` to get help
- A sequence of expressions must end with a period
- Expressions can be separatad by commans. All get executed but only the result of last one is returned

### Erlang basics

#### Numbers

##### Doesn't cares about integers or floats

```
1> 2 + 15.
17
2> 49 * 100.
4900
3> 1892 - 1472.
420
4> 5 / 2.
2.5
```

##### Integer division

```
5> 5 div 2.
2
6> 5 rem 2.
1

7> (50 * 100) – 4999.
1
8> -(50 * 100 – 4999).
-1
9> -50 * (100 – 4999).
244950
```

##### Base conversion

base#value

```
10> 2#101010.
42
11> 8#0677.
447
12> 16#AE.
174
```

#### (Invariable) Variables

- Variable names begin with uppercase
- Variables are immutable
- Variables can also start with underscore but, by convention, this is used for values you don't care about

```
1> One.
* 1: variable 'One' is unbound
2> One = 1.
1
3> Un = Uno = One = 1.
1
4> Two = One + One.
2
5> Two = 2.
2
6> Two = Two + 1.
** exception error: no match of right hand side value 3
```

The `=` operator compares values and complains if not equal.
If the left side is an unbounded variable, the right value is assigned to it.

```
7> 47 = 45 + 2.
47
8> 47 = 45 + 3.
** exception error: no match of right hand side value 48
```

#### Atoms

- Start with lowercase
- They are constants whose value is its own name
- Must be enclosed in single quotes if:
    - Start with uppercase
    - Contains non alphanumeric characters or `@`
- Atoms are not garbage collected. Do not create them dynamically
- Some reserved words cannot be used as atoms: `after, and, andalso, band, begin, bnot, bor, bsl, bsr, bxor, case, catch, cond, div, end, fun, if, let, not, of, or, orelse, query, receive, rem, try, when,` and `xor`


```
1> atom.
atom
2> atoms_rule.
atoms_rule
3> atoms_rule@erlang.
atoms_rule@erlang
4> 'Atoms can be cheated!'.
'Atoms can be cheated!'
5> atom = 'atom'.
atom
```
