## Records
Records provide a way to store and access structured data.  Think of them as a
hybrid between tuples and Keyword lists.  They can have default values
convenient accessors

```
defrecord Tweet, text: "", hash_tags: [], user_mentions: []
```

## Protocols

Protocols provide ad-hoc polymorphism.  This pattern allows library consumers
to implement protocols on third party modules and records.

```
defimpl String.Chars, for: Tweet do
  def to_string(tweet) do
    "'#{tweet.name}' mentions #{inspect tweet.user_mentions}"
  end
end
```


