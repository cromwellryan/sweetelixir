defmodule Presenter do
  def start do
    open '07-basics/README.md#basics'
  end

  defp open(section) do
    System.cmd 'open https://github.com/cromwellryan/sweetelixir/blob/master/#{section}'
  end
end
