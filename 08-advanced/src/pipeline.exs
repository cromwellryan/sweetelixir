file_content = """
1 1 1
33.8 31.9 29
2 1.7 2
"""

defmodule Classifier do
  def classify( {a,a,a} ), do: :equilateral
  def classify( {a,b,c} ) when a == b or b == c or a == c, do: :isosceles
  def classify( {_a,_b,_c} ), do: :scalene
end

user_message = fn (triangle_classifications) ->
  sides = elem(triangle_classifications, 0)
  classification = elem(triangle_classifications, 1)
  "Triangle #{inspect sides} is #{to_string classification}"
end

# **Before pipelines**
lines = String.split file_content, "\n"
lines = Stream.filter lines, &(String.length(&1) > 0)
lengths = Stream.map lines, &(String.split &1)
triangles = Stream.map lengths, &(list_to_tuple &1)
classifications = Stream.map triangles, &({ &1, Classifier.classify &1 })

messages = Stream.map classifications, user_message
result = Enum.join(messages, "\n")

IO.puts result

# **After pipelines**
file_content
  |> String.split("\n")
  |> Stream.filter( &(String.length(&1) > 0) )
  |> Stream.map( &(String.split &1) )
  |> Stream.map( &(list_to_tuple &1) )
  |> Stream.map( &( { &1, Classifier.classify &1 } ) )
  |> Stream.map( user_message )
  |> Enum.join("\n")
  |> IO.puts

