## Pipeline

```elixir
defmodule Classifier do
  def classify( {a,b,c} ) when a == b and b == c, do: :equilateral
  def classify( {a,b,c} ) when a == b or b == c or a == c, do: :isosceles
  def classify( {a,b,c} ) when a != b and b != c, do: :scalene
end

get_message = fn (classification) ->
  case classification do
    :equilateral -> "All the same"
    :isocelees -> "Two the same"
    :scalene -> "All unique"
    _ -> "dunno"
  end
end

# With pipelines
triangle_type = Classifier.classify { 1,1,1 }
message = get_message.(triangle_type)
IO.puts message

# With Pipelines
{ 1,1,1 } |> Classifier.classify |> get_message.() |> IO.puts
```

