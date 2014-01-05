## Map

Two big modules:

### [Enum][enum]

```elixir
iex(1)> ages  = [35, 34, 6, 5, 1, 1]
[35, 34, 6, 5, 1, 1]
iex(2)> add_20 = fn (age) -> age + 20 end
#Function<6.80484245 in :erl_eval.expr/5>
iex(3)> Enum.map ages, add_20
[55, 54, 26, 25, 21, 21]

iex(4)>
[2, 4, 6] |>
  Enum.map(&IO.inspect(&1)) |>
  Enum.map(&(&1 *2)) |>
  Enum.map(&IO.inspect(&1))

2
4
6
4
8
12
[4, 8, 12]
```

### [Stream][stream]

```elixir
iex(1)> ages  = [35, 34, 6, 5, 1, 1]
[35, 34, 6, 5, 1, 1]
iex(2)> add_20 = fn (age) -> age + 20 end
#Function<6.80484245 in :erl_eval.expr/5>
iex(3)> Stream.map ages, add_20
Stream.Lazy[enum: [35, 34, 6, 5, 1, 1],
 funs: [#Function<32.133702391 in Stream.map/2>], accs: [], after: [],
  last: nil]
  
iex(2)>
stream = [2, 4, 6] |>
  Stream.map(&IO.inspect(&1)) |>
  Stream.map(&(&1 *2)) |>
  Stream.map(&IO.inspect(&1))

Stream.Lazy[enum: [2, 4, 6],
 funs: [#Function<10.131393772 in Stream.map/2>,
  #Function<10.131393772 in Stream.map/2>,
  #Function<10.131393772 in Stream.map/2>], accs: []]
iex(46)> Enum.to_list stream
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
iex(1)>
next = fn (x) ->
 :timer.sleep 1000
 IO.puts "Hey #{x}"
 x + 1
end

#Function<6.80484245 in :erl_eval.expr/5>
iex(2)> Enum.take( Stream.iterate(0,next), 10)
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
iex(1)> Enum.reduce 1..10, 0, &(&1 + &2)
55
iex(2)> Enum.map_reduce([1, 2, 3], 0, fn(x, acc) -> { x * 2, 1 + acc } end)
{[2,4,6], 3}
```

[enum]: http://elixir-lang.org/docs/master/Enum.html
[stream]: http://elixir-lang.org/docs/master/Stream.html
