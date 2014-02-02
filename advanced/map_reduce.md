## Map

Two big modules:

### [Enum][enum]

```elixir
ages  = [35, 34, 6, 5, 1, 1]
add_10 = fn (age) -> age + 10 end
Enum.map ages, add_10
```

Results in...
```elixir
[55, 54, 26, 25, 21, 21]
```

The Pipeline alternative might look like this:

```elixir
[2, 4, 6] |>
  Enum.map(&IO.inspect(&1)) |>
  Enum.map(&(&1 * 10)) |>
  Enum.map(&IO.inspect(&1))
```

### [Stream][stream]

```elixir
ages  = [35, 34, 6, 5, 1, 1]
add_20 = fn (age) -> age + 20 end
Stream.map ages, add_20
```

Results in...
```elixir
Stream.Lazy[enum: [35, 34, 6, 5, 1, 1],
  funs: [#Function<32.133702391 in Stream.map/2>], accs: [], after: [],
  last: nil]
```

Streams are composable and don't execute until asked to provide a value.
```elixir
stream = [2, 4, 6] |>
  Stream.map(&IO.inspect(&1)) |>
  Stream.map(&(&1 *2)) |>
  Stream.map(&IO.inspect(&1))

Enum.to_list stream
```

Results in...
```elixir
2
4
4
8
6
12
[4, 8, 12]
```

The `[2, 4, 6]` showcases the ability to compose a stream as a series of transformations and evalute the operation as a single iteration. If you look closely, the order of operations changed from the Enum example because instead of iterating the list three separate times as the first example, the second transformation happens with a single iteration of the set.

```elixir
next = fn (x) ->
  x + 1
end
sleep = fn(x) -> 
  :timer.sleep 1000
  x
end
tap = fn(x) ->
  IO.puts "Hey #{x}"
end


Stream.iterate(0,next) |> Stream.each(tap) |> Stream.each(sleep) |> Enum.take(10)

Hey 0
Hey 1
Hey 2
Hey 3
Hey 4
Hey 5
Hey 6
Hey 7
Hey 8
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

## Reduce

```elixir
Enum.reduce 1..10, 0, &(&1 + &2)
Enum.map_reduce([1, 2, 3], 0, fn(x, acc) -> { x * 2, 1 + acc } end)
```

Results in...
```elixir
{[2,4,6], 3}
```

[enum]: http://elixir-lang.org/docs/master/Enum.html
[stream]: http://elixir-lang.org/docs/master/Stream.html
