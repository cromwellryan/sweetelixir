defrecord Person, gender: nil, name: ""

defmodule Blah do
  defmacro is_male(p) do
    ix = Person.__record__(:index, :gender)
    quote do: elem(unquote(p), unquote(ix)) == :male
  end
  defmacro is_female(p) do
    ix = Person.__record__(:index, :gender)
    quote do: elem(unquote(p), unquote(ix)) == :female
  end

  def prefix(p = Person[]) when is_male(p), do: "Mr." # won't work
  def prefix(p = Person[]) when is_female(p), do: "Mrs." # won't work
end

john = Person.new gender: :male, name: "John"
jane = Person.new gender: :female, name: "Jane"

IO.puts ""
Blah.prefix(john) |> IO.puts
Blah.prefix(jane) |> IO.puts