defmodule Countdown do

  def run(from, to) when from >= to do
    run(from, to, from)
  end

  def run(_from, to, current) when to == current do
    IO.puts to
    IO.puts "Done!"
  end

  def run(from, to, current) do
    IO.puts current
    run(from, to, current - 1)
  end
end
