defmodule Maps do

  def map([], _), do: []
  def map([h|t], func), do: [ func.(h) | map(t, func) ]

  def child(element, func, parent) do
    parent <- func.(element)
  end
  defp spawn_children(collection, func) do
    map collection, fn element -> spawn(__MODULE__, :child, [element, func, self]) end
  end

  defp collect_results(pids) do
    map pids, fn _ -> receive do: ( value -> value) end
  end

  def pmap(collection, func) do
    collection |> spawn_children(func) |> collect_results
  end
end

defmodule ClassifyShapes do
  defp slow(result, delay) do
    :timer.sleep delay
    result
  end
  defp slow(result) do
    slow(result, 1000)
  end

  def classify( {a,b,c} ) when a == b and b == c, do: :equilateral
  def classify( {a,b,c} ) when a == b or b == c or a == c, do: slow(:isosceles, 1100)
  def classify( {a,b,c} ) when a != b and b != c, do: slow(:scalene)

  def classify( [h|t] ), do: [classify(h) | classify(t)]
  def classify( [] ) do
    IO.puts 'asdf'
    []
  end
  def classify( _ ), do: :unclassified
end

triangles = [
              "blue",
              { 9, 10, 11 },
              { 9, 10, 11 },
              { 9, 10, 11 },
              { 9, 10, 11 },
              { 9, 10, 11 },
              { 9, 10, 11 },
              { 9, 10, 11 },
              { 10, 11, 11 },
              { 10, 11, 10 },
              { 11, 11, 10 },
              { 12, 12, 12 },
            ]

IO.inspect triangles |> Maps.pmap fn x -> {x, ClassifyShapes.classify(x)} end

