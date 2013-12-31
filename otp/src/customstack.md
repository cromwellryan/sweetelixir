## Show them what it will look like

### Start our customserver
```bash
$> iex customstack.exs
```
```elixir
iex> Stack.CustomServer.start []
iex> :custom_server <- { :push, 10 }
iex> :custom_server <- { :self, :pop }
```

###iex manual receive

```elixir
receive do
  val -> IO.puts "Pop resulted in #{val}"
end
```

## Guided customstack.exs
### Module receive loop with after outputting "nothing received"

```elixir
defmodule Stack.CustomServer do
  def start(initial_stack) do
    spawn_link fn ->
      Process.register self, :custom_server
      listen initial_stack
    end
  end

  def listen(stack) do
    receive do
    after 2000 ->
      IO.puts "Nothing to do"
      listen stack
    end
  end
end
```

### add receive push

```elixir
  {:push, value} -> listen([value|stack])
```

### run iex and push value (cast())

```elixir
:custom_server <- { :push, 10 }
```

### add receive pop

```elixir
  {sender, :pop}         -> handle_pop(sender, stack)

  #...
  def handle_pop(sender, [head|tail]) do
    sender <- head
    listen tail
  end
```

### run iex and pop value (call() - req/reply)

```elixir
:custom_server <- {self, :pop}
```

### crash - lots of logic leads us to use OTP applications
