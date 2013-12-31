# Basics

Follow along - we'll be using `iex` (interactive elixir).

## Operators
comparison, boolean, arithmetic, and inclusion
  - ==, !=, ===, !==, >, <, <=, >=
  - and, or, not, !
  - +, -, *, /
  - in (inclusion operator)

## Types (int, float, atom, tuple, list, binary)

**numbers**

```
iex> is_number 1
true

iex> is_integer 1
true

iex> is_number 1.0
true

iex> is_integer 1.0
false

iex> is_float 1.0
true
```

### boolean

```
iex> is_boolean true && false
true

iex> is_boolean true and false
true
```

**atoms** are like Ruby *symbols*, but named after Lisp's *atom*

```
iex> is_atom :thing
true

iex> is_atom true
true

iex> :false == false
true
```

**lists** are linked lists
```
iex> is_list [1,2,3]
true

iex> length [1,2,3]
3

iex> [head | tail] = [1,2,3]
[1,2,3]

iex> head
1

iex> tail
[2,3]

iex> [1,2] ++ [3,4]
[1,2,3,4]

iex> [1,2,3] -- [3,4]
[1,2]

iex> h Enum
...

iex> h Stream
...
```

**tuples**
```
iex> is_tuple {1,"b", :c}
true

iex> elem {1, "b", :c}, 1
"b"

iex> {x, y, _} = {1, "b", :c}
{1, "b", :c}

iex> x
1

iex> y
"b"

iex> t = {1, "b", :c}
{1, "b", :c}

iex> set_elem t, 2, :d
{1, "b", :d}

iex> t
{1, "b", :c}

iex> elem t, 2
"b"

iex> Integer.parse "1.0a~22c"
{1, ".0a~22c"}

iex> Integer.parse "codemash"
:error
```

**strings (binaries) and char lists (lists)**
```
iex> is_list 'hello'
true

iex> is_list "hello"
false

iex> is_binary 'world'
false

iex> is_binary "world"
true

iex> "hello" <> " " <> "world"
"hello world"

iex> who = "codemash"
iex> "hello #{who}"
"hello codemash"
```

**keyword lists**
```
iex> presenter = [name: 'Chris', project: 'Atlas']
iex> Keyword.get presenter, :project
"Atlas"

iex> [head | _] = presenter

iex> head
{ :name, 'Chris' }
```

**equality**

```
iex> 1 == 1.0
true

iex> 1 === 1.0
false

iex> 1 < :thing
true
```

## Functions

Like most functional languages, functions in Elixir are first class and can be passed around 
and invoked from other functions.

### Anonymous Functions

Anonymous functions are defined with the `fn arg1, arg2 -> end` syntax and invoked via the "dot notation".

```elixir
iex(1)> add = fn num1, num2 ->
...(1)>   num1 + num2
...(1)> end
#Function<12.17052888 in :erl_eval.expr/5>

iex(2)> subtract = fn num1, num2 ->
...(2)>   num1 - num2
...(2)> end
#Function<12.17052888 in :erl_eval.expr/5>

iex(3)> perform_calculation = fn num1, num2, func ->
...(3)>   func.(num1, num2)
...(3)> end
#Function<18.17052888 in :erl_eval.expr/5>

iex(4)> perform_calculation.(5, 5, add)
10
iex(5)> perform_calculation.(5, 5, subtract)
0
iex(6)> perform_calculation.(5, 5, &(&1 * &2))
25
iex(7)>
```

The last examples showcases Elixir's **shorthand function** syntax and is sugar for:

```elixir
iex(6)> perform_calculation.(5, 5, fn a, b -> a * b end)
25
```

The shorthand syntax is useful when the function takes one or two arguments and performs a simple operation. 
More complex functions should use the general purpose syntax to optimize for clarity.

### Named Functions

Functions defined on Modules are referred to as **named functions**. Named functions are defined with the `def` syntax.

```elixir
iex(4)> defmodule Weather do
...(4)>   def celsius_to_fahrenheit(celsius) do
...(4)>     (celsius * 1.8) + 32
...(4)>   end
...(4)>
...(4)>   def high, do: 50
...(4)>   def low, do: 32
...(4)> end
{:module, Weather,
 <<70, 79, 82, 49, 0, 0, 8, 116, 66, 69, 65, 77, 65, 116, 111, 109, 0, 0, 0, 133, 0, 0, 0, 13, 14, 69, 108, 105, 120, 105, 114, 46, 87, 101, 97, 116, 104, 101, 114, 8, 95, 95, 105, 110, 102, 111, 95, 95, 4, 100, ...>>,
 {:low, 0}}
iex(5)> Weather.high
50
iex(6)> Weather.celsius_to_fahrenheit(20)
68.0
iex(7)>
```

### Captured Functions

Named functions can be **captured** and bound to a variable for cases where a reference is desired instead of invocation.

```elixir
iex(2)> add = &Kernel.+/2
&Kernel.+/2
iex(3)> add.(1, 2)
3
iex(4)>
```

Functions in Elixir are referenced by name and **arity**, or the number of arguments. The previous example captures 
the `+` function from the `Kernel` module with arity 2, binds it to the `add` variable, and invokes it via the 
dot notation.


## Modules

Our `Weather` module showcased Elixir Modules. Modules hold named functions, can import functions from other Modules,
and use Macro's for extended functionality.

```elixir
iex(12)> defmodule Converter do
...(12)>   alias :math, as: Math
...(12)>   import Math, only: [pi: 0]
...(12)>
...(12)>   def degrees_to_radians(degrees) do
...(12)>     degrees * (pi / 180)
...(12)>   end
...(12)>
...(12)>   def sin_to_cos(x) do
...(12)>     Math.cos(x - (pi / 2))
...(12)>   end
...(12)> end
{:module, Converter,
 <<70, 79, 82, 49, 0, 0, 8, 140, 66, 69, 65, 77, 65, 116, 111, 109, 0, 0, 0, 143, 0, 0, 0, 14, 16, 69, 108, 105, 120, 105, 114, 46, 67, 111, 110, 118, 101, 114, 116, 101, 114, 8, 95, 95, 105, 110, 102, 111, 95, 95, ...>>,
 {:sin_to_cos, 1}}
iex(13)> Converter.degrees_to_radians(90)
1.5707963267948966
iex(14)> Converter.sin_to_cos(120)
0.5806111842123187
iex(15)>

```

