# 1. Starting out

## Run the shell

    erl

## Shell

- CTRL-A: move cursor to beginning of the line
- CTRL-E: move cursor to the end of the line
- TAB autocompletion. Eg: `li<TAB>`
- Exit with `q()` which is a shorthand of `init:stop()`
- CTRL-G: abort shell
    - then `h` to get help
- A sequence of expressions must end with a period
- Expressions can be separatad by commans. All get executed but only the result of last one is returned

## Erlang basics

### Numbers

#### Doesn't cares about integers or floats

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

#### Integer division

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

#### Base conversion

base#value

```
10> 2#101010.
42
11> 8#0677.
447
12> 16#AE.
174
```

### (Invariable) Variables

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

### Atoms

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

### Boolean algebra

Both sides are evaluated. To short circuit use `andalso` or `orelse`

```
1> true and false.
false
2> false or true.
true
3> true xor false.
true
4> not false.
true
5> not (true and true).
false
```

`=:=` is for exact equality

```
6> 5 =:= 5.
true
7> 1 =:= 0.
false
8> 1 =/= 0.
true
9> 5 =:= 5.0.
false
10> 5 == 5.0.
true
11> 5 /= 5.0.
false
```

Greater-than looks weird (`=<`)

```
12> 1 < 2.
true
13> 1 < 1.
false
14> 1 >= 1.
true
15> 1 =< 1.
true
```

Keep an eye when doing boolean comparisions

```
14> 0 == false.
false
15> 1 < false.
true
```

Types have an order

```
number < atom < reference < fun < port < pid < tuple < list < bit string
```

### Tuples

```
1> X = 10, Y = 4.
4
2> Point = {X,Y}.
{10,4}
3> Point = {4,5}.
{4,5}
4> {X,Y} = Point.
{4,5}
5> X.
4
6> {X,_} = Point.
{4,5}
7> {_,_} = {4,5}.
{4,5}
8> {_,_} = {4,5,6}.
** exception error: no match of right hand side value {4,5,6}
9> Temperature = 23.213.
23.213
10> PreciseTemperature = {celsius, 23.213}.
{celsius,23.213}
11> {kelvin, T} = PreciseTemperature.
** exception error: no match of right hand side value {celsius,23.213}
```

### Lists

Erlang will print lists of numbers as numbers only when at least one of them could not also represent a letter

```
1> [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].
[1,2,3,{numbers,[4,5,6]},5.34,atom]
2> [97, 98, 99].
"abc"
3> [97,98,99,4,5,6].
[97,98,99,4,5,6]
4> [233].
"é"
5> [1,2,3] ++ [4,5].
[1,2,3,4,5]
6> [1,2,3,4,5] -- [1,2,3].
[4,5]
7> [2,4,2] -- [2,4].
[2]
8> [2,4,2] -- [2,4,2].
[]
```

`--` and `++` are right-associative

```
9> [1,2,3] -- [1,2] -- [3].
[3]
10> [1,2,3] -- [1,2] -- [2].
[2,3]
```

Head and Tail

```
11> hd([1,2,3,4]).
1
12> tl([1,2,3,4]).
[2,3,4]
```

Alternative syntax

```
13> List = [2,3,4].
[2,3,4]
14> NewList = [1|List].
[1,2,3,4]
15> [Head|Tail] = NewList.
[1,2,3,4]
16> Head.
1
17> Tail.
[2,3,4]
18> [NewHead|NewTail] = Tail.
[2,3,4]
19> NewHead.
2
```

`|` is the `cons` operator

```
20> [1 | []].
[1]
21> [2 | [1 | []]].
[2,1]
22> [3 | [2 | [1 | []]]].
[3,2,1]
```

Lists must end with an empty list. If not, it is an "improper list". For example: `[1|2]`. This is valid but many functions, like `length` will not work on it.

### List Comprehensions

For all n in [1, 2, 3, 4], give me n*2

```
1> [2*N || N <- [1,2,3,4]].
[2,4,6,8]
```

All even numbers from 1 to 10

```
2> [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
[2,4,6,8,10]
```

```
3> RestaurantMenu = [{steak, 5.99}, {beer, 3.99}, {poutine, 3.50}, {kitten, 20.99}, {water, 0.00}].
[{steak,5.99},
{beer,3.99},
{poutine,3.5},
{kitten,20.99},
{water,0.0}]
4> [{Item, Price*1.07} || {Item,Price} <- RestaurantMenu, Price >= 3, Price =< 10].
[{steak,6.409300000000001},{beer,4.2693},{poutine,3.745}]
```

Many generator expressions

```
5> [X+Y || X <- [1,2], Y <- [3,4]].
[4,5,5,6]
```

With pattern matching

```
6> Weather = [{toronto, rain}, {montreal, storms}, {london, fog},
6> {paris, sun}, {boston, fog}, {vancouver, snow}].
[{toronto,rain},
 {montreal,storms},
 {london,fog},
 {paris,sun},
 {boston,fog},
 {vancouver,snow}]
7> FoggyPlaces = [X || {X, fog} <- Weather].
[london,boston]
```

### Binary data

#### Binary segments

```
1> Color = 16#F09A29.
15768105
2> Pixel = <<Color:24>>.
<<240,154,41>>
```

#### Pattern matching

```
3> Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
4> <<Pix1,Pix2,Pix3,Pix4>> = Pixels.
** exception error: no match of right hand side value <<213,45,132,64,76,32,76,0,0,234,32,15>>
5> <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
```

#### Unpack

```
6> <<R:8, G:8, B:8>> = <<Pix1:24>>.
<<213,45,132>>
7> R.
213
```

#### Rest

```
8> <<R:8, Rest/binary>> = Pixels.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
9> R.
213
```

#### Ways to describe a binary segment

- Value
- Value:Size
- Value/TypeSpecifierList
- Value:Size/TypeSpecifierList

##### Type

Possible types: `integer`, `float`, `binary`, `bytes`, `bitstring`, `bits`, `utf8`, `utf16`, `utf32`

Default `integer`

##### Signedness

`signed` and `unsigned`

##### Endianness

Possible: `big`, `little`, `native`

Default: `big`

##### Unit

Usually used to ensure byte alignment

##### Examples

```
10> <<X1/unsigned>> = <<-44>>.
<<"Ô">>
11> X1.
212
12> <<X2/signed>> = <<-44>>.
<<"Ô">>
13> X2.
-44
14> <<X2/integer-signed-little>> = <<-44>>.
<<"Ô">>
15> X2.
-44
16> <<N:8/unit:1>> = <<72>>.
<<"H">>
17> N.
72
18> <<N/integer>> = <<72>>.
<<"H">>
19> <<Y:4/little-unit:8>> = <<72,0,0,0>>.
<<72,0,0,0>>
20> Y
```

#### Bitwise Binary Operations

