# Basics

Follow along - we'll be using `iex` (interactive elixir).

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


- Operators
- Functions (built in / anonymous)
- and / &&
- ...
- Modules (def / defp)
