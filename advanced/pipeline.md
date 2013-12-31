## Pipeline

The pipeline is a very simple, but powerful tool in Elixir.  Using the pipeline operator `|>` we can easily visualize the transformation of data - which is what our programs do!


**Before Pipeline**
```elixir
lines = String.split file_content, "\n"
lines = Stream.filter lines, &(String.length(&1) > 0)
lengths = Stream.map lines, &(String.split &1)
triangles = Stream.map lengths, &(list_to_tuple &1)
classifications = Stream.map triangles, &({ &1, Classifier.classify &1 })

messages = Stream.map classifications, user_message
result = Enum.join(messages, "\n")

IO.puts result
```

**After Pipeline**
```elixir
file_content
  |> String.split("\n")
  |> Stream.filter( &(String.length(&1) > 0) )
  |> Stream.map( &(String.split &1) )
  |> Stream.map( &(list_to_tuple &1) )
  |> Stream.map( &( { &1, Classifier.classify &1 } ) )
  |> Stream.map( user_message )
  |> Enum.join("\n")
  |> IO.puts
```

[Run it (./src/pipeline.exs)](./src/pipeline.exs)

**Bonus**
```elixir
%w( gray, white )
  |> Stream.cycle
  |> Stream.zip(%w(Customer1 Customer2 Customer3 Customer 4)
  |> Stream.map( fn {color, value} ->
       %s{<tr class="#{color}"><td>#{value}</td></tr>}
     end)
  |> Enum.join "\n"
"<tr class=\"gray,\"><td>1</td></tr>\n<tr class=\"white\"><td>2</td></tr>\n<tr class=\"gray,\"><td
>3</td></tr>\n<tr class=\"white\"><td>4</td></tr>\n<tr class=\"gray,\"><td>5</td></tr>\n<tr class=
\"white\"><td>6</td></tr>\n<tr class=\"gray,\"><td>7</td></tr>\n<tr class=\"white\"><td>8</td></tr
>\n<tr class=\"gray,\"><td>9</td></tr>\n<tr class=\"white\"><td>10</td></tr>"
```

[Run it (./src/pipeline_bonus.exs)](./src/pipeline_bonus.exs)
