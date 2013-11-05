defmodule Counter.Worker do
  use GenServer.Behaviour

  def init(initial_value) do
    IO.puts "starting worker"
    { :ok, initial_value }
  end

  def handle_call(:stat, _from, count ) do
    IO.puts ":stat - #{count}"

    { :reply, count, count }
  end

  def handle_cast(:increment, count) do
    IO.puts ":increment - #{count + 1}"
    { :noreply, count + 1 }
  end

  def start_link() do
    IO.puts ":start_link"
    :gen_server.start_link( { :global, :counter }, __MODULE__, 0, [] )
  end
end
