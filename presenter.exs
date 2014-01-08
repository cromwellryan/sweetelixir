defmodule Presenter do
  @moduledoc """
  Presenter can be used in `iex` to open sections of the session.

  ## Example Usage

      $ iex presenter.exs

      iex> import Presenter; import Presenter.Basics
      nil

      iex> start
      nil

      iex> tools
      nil

      iex> Basics.pipeline
      nil
  """
  def start, do: open "README.md#getting-started"
  def setup, do: open "setup/README.md"
  def tools, do: open "tools/README.md"

  def basics, do: open "basics/README.md"

  defmodule Basics do

    def operators, do: open "operators"
    def types, do: open "types"
    def numbers, do: open "operators"
    def boolean, do: open "boolean"
    def atoms, do: open "atoms"
    def list, do: open "lists"
    def tuples, do: open "tuples"
    def strings, do: open "strings"
    def json_parser, do: System.cmd "open https://gist.github.com/cromwellryan/6349503/raw/0bac9dcbe6ea2c68f27d91c2a44034c7e4d3fb2c/parser.ex"
    def keywordlists, do: open "keyword-lists"
    def equality, do: open "equality"
    def functions, do: open "functions"
    def modules, do: open "modules"

    defp open(hash), do: Presenter.open "basics/README.md##{hash}"

  end

  def open(section) do
    System.cmd 'open https://github.com/cromwellryan/sweetelixir/blob/master/#{section}'
    nil
  end
end
