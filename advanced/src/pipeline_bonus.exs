%w( gray, white )
  |> Stream.cycle
  |> Stream.zip(%w(Customer1 Customer2 Customer3 Customer4))
  |> Stream.map( fn {color, value} ->
       %s{<tr class="#{color}"><td>#{value}</td></tr>}
     end)
  |> Enum.join("\n")
  |> IO.puts
