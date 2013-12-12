defmodule Stack.CustomServer do

  def start(initial_stack) do
    spawn_link fn -> 
      :global.register_name :custom_server, self
      listen initial_stack
    end
  end

  def listen(stack) do
    receive do
      {sender, :pop}         -> handle_pop(sender, stack)
      {sender, :push, value} -> listen([value|stack])
    end
  end

  def handle_pop(sender, []) do
    sender <- nil
    listen []
  end
  def handle_pop(sender, stack) do
    sender <- hd(stack)
    listen tl(stack)
  end
end


defmodule Stack.CustomClient do
  def push(value) do
    server_pid <- {self, :push, value}
  end

  def pop do
    server_pid <- {self, :pop}
    receive do
      value -> value
    end
  end

  defp server_pid do
    :global.whereis_name :custom_server
  end
end
