defmodule Counter.Worker do
  use GenServer.Behaviour

  def init(initial_value) do
    IO.puts "starting worker"
    { :ok, initial_value }
  end

  def start_link() do
    IO.puts ":start_link"
    :gen_server.start_link( { :global, :counter }, __MODULE__, 0, [] )
  end

  def handle_call(:stat, _from, count ) do
    IO.puts "stat - #{count}"

    { :reply, count, count }
  end

  def handle_cast(:increment, count) do
    IO.puts "increment - #{count + 1}"
    { :noreply, count + 1 }
  end

  def handle_info(:timeout, _state) do
    IO.puts "handle_timeout"
  end

  def handle_info(_msg, _state) do
  end

  def terminate(_reason, _state) do
  end
end
