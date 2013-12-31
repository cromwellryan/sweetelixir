## Records
Records provide a way to store and access structured data.  Think of them as a
hybrid between tuples and Keyword lists.  They can have default values
convenient accessors

```
defrecord Status, id: nil, text: "", username: nil, hash_tags: [], mentions: []
```

## Protocols

Protocols provide ad-hoc polymorphism.  This pattern allows library consumers
to implement protocols on third party modules and records.

```
defimpl String.Chars, for: Status do
  def to_string(status) do
    "'#{status.username}' mentions #{inspect status.mentions}"
  end
end
```


