defrecord Tweet, text: "", hash_tags: [], user_mentions: []

defimpl String.Chars, for: Tweet do
  def to_string(Tweet[text: text, user_mentions: []]), do: "'#{text}'"

  def to_string(Tweet[text: text, user_mentions: user_mentions]) do
    "'#{text}' mentions #{inspect user_mentions}"
  end
end
