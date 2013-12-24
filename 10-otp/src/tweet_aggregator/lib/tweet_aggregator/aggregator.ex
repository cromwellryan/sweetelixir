defmodule TweetAggregator.Aggregator do
  use GenServer.Behaviour

  def start_link do
    :gen_server.start_link({:global, :aggregator}, __MODULE__, [], [])
  end

  def init(_) do
    {:ok, []}
  end

  def handle_cast({:status, status}, statuses) do
    IO.puts status.text
    {:noreply, [status | statuses]}
  end

  def notify(status) do
    :gen_server.cast {:global, :aggregator}, {:status, status}
  end
end

