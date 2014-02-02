## Pattern Matching

Pattern matching lives at the heart of the Erlang Virtual Machine. When binding or invoking a function, the VM is pattern matching on the provided expression.

```elixir
a = 1 # 1

1 = a # 1

b = 2 # 2

^a = b
** (MatchError) no match of right hand side value: 2
iex(5)> ^a = 1
1
iex(6)> [first, 2, last] = [1, 2, 3]
[1, 2, 3]
iex(7)> first
1
iex(8)> last
3
```

`^a = b` shows the syntax for pattern matching against a variable's value instead of performing assignment.

A common technique is to return a tuple from a function with the atom `:ok`, followed by the requested value. This allows the caller to pattern match against the return value and let the program crash when an unexpected error occurs:

```elixir
iex(1)> {:ok, logger_pid} = DB.start_logger
```

If `DB.start_logger` failed to open the log file and returned `{:error, :enoent}`, the program would crash with a `MatchError` since pattern matching was performed solely by the use of the `=` operator. This is useful where programs can take no other sane action than crashing, such as when a database's logger fails to open.


### Control Flow Pattern Matching

Pattern matching can be used with the `case` keyword for powerful control flow handling. `case` pattern matches against each clause and invokes the first match. At least one clause must be matched or a `CaseClauseError` is raised.

Let's write a mini calculation parser to perform a few basic operations:

```elixir
iex(1)> calculate = fn expression ->
...(1)>   case expression do
...(1)>     {:+, num1, num2} -> num1 + num2
...(1)>     {:-, num1, num2} -> num1 - num2
...(1)>     {:*, num1, 0}    -> 0
...(1)>     {:*, num1, num2} -> num1 * num2
...(1)>   end
...(1)> end
#Function<6.17052888 in :erl_eval.expr/5>

iex(2)> calculate.({:+, 8, 2})
10
iex(3)> calculate.({:-, 8, 2})
6
iex(4)> calculate.({:*, 8, 2})
16
iex(5)> calculate.({:^, 8, 2})
** (CaseClauseError) no case clause matching: {:^, 8, 2}
iex(5)>
```

### Function Pattern Matching
Pattern matching is not just limited to bindings. Elixir uses pattern matching for anonymous and named function invocation as well.

[Run it][src_countdown] (./src/countdown.ex)
```elixir
defmodule Countdown do
  def run(from, to) when from >= to do
    run(from, to, from)
  end

  def run(_from, to, current) when to == current do
    IO.puts to
    IO.puts "Done!"
  end

  def run(from, to, current) do
    IO.puts current
    run(from, to, current - 1)
  end
end

```

```elixir
iex(10)> c "countdown.ex"
[Countdown]
iex(11)> Countdown.run(10, 2)
10
9
8
7
6
5
4
3
2
Done!
:ok

iex(13)> Countdown.run(10, 11)
** (FunctionClauseError) no function clause matching in Countdown.run/2
    countdown.ex:3: Countdown.run(10, 11)
```

*Guard Clauses* are used with the postfix `when` on function definitions. Multiple functions with the same name can be defined because the guard becomes part of the function's unique signature. Each definition is pattern matched against when calling `run` until the first match is found. Pattern matching on functions is run from top to bottom, in the order the functions are defined.

### Record Pattern Matching

[Run it][src_record_pattern_match] (./src/pattern_match_records.exs)
```elixir
defrecord Person, gender: nil, name: ""

defmodule PersonPrefixer do
  def prefix(p = Person[gender: :male]), do: "Mr."
  def prefix(p = Person[gender: :female]), do: "Mrs."
end

defmodule SharingMatchLogic do
  defmacro is_male(p) do
    ix = Person.__record__(:index, :gender)
    quote do: elem(unquote(p), unquote(ix)) == :male
  end
  defmacro is_female(p) do
    ix = Person.__record__(:index, :gender)
    quote do: elem(unquote(p), unquote(ix)) == :female
  end

  def prefix(p = Person[]) when is_male(p), do: "Mr." # won't work
  def prefix(p = Person[]) when is_female(p), do: "Mrs." # won't work
end

john = Person.new gender: :male, name: "John"
jane = Person.new gender: :female, name: "Jane"

PersonPrefixer.prefix(john) |> IO.puts
PersonPrefixer.prefix(jane) |> IO.puts

SharingMatchLogic.prefix(john) |> IO.puts
SharingMatchLogic.prefix(jane) |> IO.puts

```

[src_record_pattern_match]: ./src/pattern_match_records.exs
[src_countdown]: ./src/countdown.ex
