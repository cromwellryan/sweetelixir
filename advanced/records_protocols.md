## Records
Records provide a way to store and access structured data.  Think of them as a
hybrid between tuples and Keyword lists.  They can have default values as well as convenient accessors

```elixir
iex(1)>
defrecord Status, id: nil, text: "", username: nil, hash_tags: [], mentions: []

iex(2)> status = Status.new text: "Sweet Elixir!"
Status[id: nil, text: "Sweet Elixir!", username: nil, hash_tags: [],
 mentions: []]
iex(3)> status.text
"Sweet Elixir!"
iex(4)> status = status.text("RT Sweet Elixir!")
Status[id: nil, text: "RT Sweet Elixir!", username: nil, hash_tags: [],
 mentions: []]
iex(5)> status.text
"RT Sweet Elixir!"
iex(6)> status.update(text: "@elixir-lang rocks!", username: "chris_mccord")
Status[id: nil, text: "@elixir-lang rocks!", username: "chris_mccord",
 hash_tags: [], mentions: []]
```

## Protocols

Protocols provide ad-hoc polymorphism.  This pattern allows library consumers
to implement protocols on third party modules and records.

```elixir
defimpl String.Chars, for: Status do
  def to_string(status = Status[mentions: []]) do
    "#{status.username}: #{status.text}"
  end
  def to_string(status) do
    """
    #{status.username}': #{status.text}
    mentions: #{inspect status.mentions}
    """
  end
end



iex(2)> status = Status.new username: "chris_mccord", text: "Sweet Elixir!"
Status[id: nil, text: "Sweet Elixir!", username: nil, hash_tags: [],
 mentions: []]

iex(4)> IO.puts status
chrismccord: Sweet Elixir!
:ok
iex(5)>
```




