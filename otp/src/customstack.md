## Show them what it will look like
1. `iex customstack.exs`
1. `iex> :custom_server <- { :push, 10 }`
2. `iex> :custom_server <- { :self, :pop }`
3. iex manual receive 

```elixir
receive do
  val -> IO.puts "Pop resulted in #{val}"
end
```

## Guided customstack.exs
1. Module receive loop with after outputting "nothing received"

```
defmodule Stack.CustomServer do
  def start(initial_stack) do
    spawn_link fn -> 
      :process.register_name :custom_server, self
      listen initial_stack
    end
  end

  def listen(stack) do
    receive do
    after 2000 ->
      IO.puts "nothing to do"
      listen stack
    end
  end
end
```

1. add receive push

```
  {:push, value} -> listen([value|stack])
```

1. run iex and push value (cast())

```
:custom_server <- { :push, 10 }
```

1. add receive pop

```
  {sender, :pop}         -> handle_pop(sender, stack)
  
  #...
  def handle_pop(sender, [head|tail]) do
    sender <- head
    listen tail
  end
```

1. run iex and pop value (call() - req/reply)

```
:custom_server <- {self, :pop}
```

1. crash - lots of logic leads us to use OTP applications
