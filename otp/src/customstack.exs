defmodule Stack.CustomServer do

  def start(initial_stack) do
    spawn_link fn -> 
      Process.register :custom_server, self
      listen initial_stack
    end
  end

  def listen(stack) do
    receive do
      {sender, :pop} -> handle_pop(sender, stack)
      {:push, value} -> listen([value|stack])
    end
  end

  def handle_pop(sender, [head|tail]) do
    sender <- head
    listen tail
  end
end


defmodule Stack.CustomClient do
  def push(value) do
    server_pid <- {:push, value}
  end

  def pop do
    server_pid <- {self, :pop}
    receive do
      value -> value
    end
  end

  defp server_pid do
    Process.whereis :custom_server
  end
end
