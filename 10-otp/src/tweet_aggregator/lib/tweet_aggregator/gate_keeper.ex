defmodule TweetAggregator.GateKeeper do
  @moduledoc """
  GateKeeper is a module that allows Nodes to retreive credentials from a 
  leader node containing evironment variables for oauth signing

  The follow Twitter OAuth environment variables are held by the GateKeeper:

  - TWEET_CONSUMER_KEY
  - TWEET_CONSUMER_SECRET
  - TWEET_ACCESS_TOKEN
  - TWEET_ACCESS_TOKEN_SECRET

  A single node must register as the leader to make API calls:

  ## Examples

     Leader Node:
     $ export TWEET_ACCESS_TOKEN="foo"
     iex> TweetAggregator.GateKeeper.register_as_leader

     Remote Client Node
     iex> TweetAggregator.GateKeeper.access_token
     "foo"

  """

  @params [:access_token, :access_token_secret, :consumer_key, :consumer_secret]

  def register_as_leader do
    :global.register_name :gate_keeper, spawn(fn -> listen end)
  end

  def leader_pid do
    :global.whereis_name(:gate_keeper) || raise "No registered GateKeeper"
  end

  def access_token,        do: get(:access_token)
  def access_token_secret, do: get(:access_token_secret)
  def consumer_key,        do: get(:consumer_key)
  def consumer_secret,     do: get(:consumer_secret)

  defp listen do
    receive do
      {sender, param} when param in @params ->
        sender <- System.get_env("TWEET_#{String.upcase(to_string(param))}")
      {sender, _} -> {sender <- nil}
    end
    listen
  end

  defp get(param) do
    leader_pid <- {self, param}
    receive do
      param -> param
    end
  end
end

