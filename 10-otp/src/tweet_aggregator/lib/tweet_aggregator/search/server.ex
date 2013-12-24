defmodule TweetAggregator.Search.Server do
  use GenServer.Behaviour
  alias TweetAggregator.Search.Client
  alias TweetAggregator.Search.Client.Query

  @poll_every_ms 10000

  def start_link(server_name, query) do
    :gen_server.start_link({:local, server_name}, __MODULE__, [query], [])
  end

  def init([query]) do
    :timer.send_interval(@poll_every_ms, :poll)
    {:ok, query}
  end

  def handle_info(:poll, query) do
    {:ok, statuses} = Client.search(query.keywords, query.options)
    if new_results?(statuses, query) do
      IO.puts "New results"
      query.subscriber <- {:results, statuses}
      {:noreply, record_seen_ids(statuses, query)}
    else
      IO.puts "No new results"
      {:noreply, query}
    end
  end

  def record_seen_ids(statuses, query = Query[seen_ids: seen_ids]) do
    query.seen_ids(query.seen_ids ++ seen_ids(statuses))
  end

  def new_results?(statuses, query) do
    !Enum.find(seen_ids(statuses), fn id -> id in query.seen_ids end)
  end

  def seen_ids(statuses) do
    statuses |> Enum.map(&(&1.id))
  end
end

