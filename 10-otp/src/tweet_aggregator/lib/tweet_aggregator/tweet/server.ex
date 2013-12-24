defmodule TweetAggregator.Tweet.Server do
  use GenServer.Behaviour
  alias TweetAggregator.Tweet.Query
  alias TweetAggregator.Tweet.Client

  @poll_every_ms 60000

  def start_link(server_name, query) do
    :gen_server.start_link({:local, server_name}, __MODULE__, [query], [])
  end

  def init([query]) do
    :timer.send_interval(@poll_every_ms, :poll)
    {:ok, query}
  end

  def handle_info(:poll, query) do
    {:ok, tweets} = Client.search(query.keywords, query.options)
    IO.puts "poll"
    query.subscriber <- {:results, tweets}

    {:noreply, query}
  end
end

