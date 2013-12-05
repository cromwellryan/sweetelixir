## Pattern Matching

Pattern matching lives at the heart of the Erlang Virtual Machine. When binding or invoking a function, the VM is pattern matching on the provided expression.

```elixir
iex(1)> a = 1
1
iex(2)> 1 = a
1
iex(3)> b = 2
2
iex(4)> ^a = b
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

`^a = b` shows the syntax for pattern matching against a variable's value instead of performing assignment. Pattern matching is used throughout Elixir and Erlang programs for destructuring assignment as well as simple failure modes where a program is expected to crash unless a specific pattern is returned. Consider a program that opens a log file for writing database logs:

```elixir
iex(1)> {:ok, logger_pid} = DB.start_logger
```

A common pattern is to return a tuple from a function with the atom `:ok`, followed by the requested value, as well as any set of tuples and error messages that the caller could be interested in. In the previous example, if `DB.start_logger` failed to open the log file and returned `{:error, :enoent}`, the program would crash with a `MatchError` since pattern matching was performed solely by the use of the `=` operator. This is useful where programs can take no other sane action than crashing, such as when a database's logger fails to open.


### Function Pattern Matching
Pattern matching is not just limited to bindings. Elixir uses pattern matching for anonymous and named function invocation as well.

`src/countdown.ex`:
```elixir
defmodule Countdown do
  def run(from, to) when from > to do
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

By using *guard clauses* with the postfix `when` on function definitions. Multiple functions with the same name can be defined because the guard becomes part of the functions unique signature and each definition is pattern matched against when calling `run` until the first match is found.

