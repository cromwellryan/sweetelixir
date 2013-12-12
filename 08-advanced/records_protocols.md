## Records
... some description of records purposes, et-al

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


