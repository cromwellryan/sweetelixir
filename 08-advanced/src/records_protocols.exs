defrecord Tweet, text: "", hash_tags: [], user_mentions: []

defimpl String.Chars, for: Tweet do
  def to_string(tweet) do
    "'#{tweet.name}' mentions #{inspect tweet.user_mentions}"
  end
end
