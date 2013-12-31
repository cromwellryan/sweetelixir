# Elixir Tooling

Elixir ships with four binaries for compilation, project building, and more.

- `iex` a shell for evaluating elixir expressions and experimenting with
  your source code in real-time.
- `elixir` executes compiled elixir files or elixir "scripts" (.exs files)
- `elixirc` elixir compiler
- `mix` elixir's fantastic build tool for project creation,
   dependency management, tasks, and more.


## Interactive Elixir - The Elixir Shell

`iex` is an essential tool for exploring language features and quickly
experimenting with code. Any valid Elixir expression can be evaluated and
using the `h` helper function provides instant access to module documentation.


    $ iex
    Interactive Elixir (0.11.2) - press Ctrl+C to exit (type h() ENTER for help)
    iex(1)> 1 + 1
    2
    iex(2)> h Enum.first

                                 def first(collection)

    Returns the first item in the collection or nil otherwise.

    Examples

    ┃ iex> Enum.first([])
    ┃ nil
    ┃ iex> Enum.first([1, 2, 3])
    ┃ 1


    iex(3)>


The `c` helper function compiles files and loads them into the shell.

```elixir
#hello.ex
defmodule Hello do
  def world do
    IO.puts "Hello Word!"
  end
end
```

    iex(2)> c "hello.ex"
    [Hello]
    iex(3)> Hello.world
    Hello Word!
    :ok
    iex(4)>


## Creating your first project with mix

    $ mix new hello_elixir
      * creating README.md
      * creating .gitignore
      * creating mix.exs
      * creating lib
      * creating lib/hello_elixir.ex
      * creating lib/hello_elixir
      * creating lib/hello_elixir/supervisor.ex
      * creating test
      * creating test/test_helper.exs
      * creating test/hello_elixir_test.exs

      Your mix project was created successfully.
      You can use mix to compile it, test it, and more:

          cd hello_elixir
          mix compile
          mix test

      Run `mix help` for more information.


Mix creates a directory structure and application manifest that follows
Erlang/OTP conventions and plays nicely with Erlang applications.

- Source code lives in `lib/hello_elixir/src/`. Any `.ex` file you place in lib
  or subdirectories of lib will be automatically compiled and loaded by mix.
- Test are defined as *elixir scripts* with `.exs` extensions in the `src/test`
  directory. Any `.exs` file loaded within test and test subdirectories will be
  automatically included in the test suite.
- `mix.exs` contains your projects configuration and is where you define your
   dependencies.



## Exploration

To see what mix has to offer, simply invoke `mix help` and start exploring.
Most useful commands include:

- `mix deps.get` Retrieve and compile all dependencies
- `mix run` Runs your project, compiling sources where neccessary
- `mix test` Runs all tests located in `src/test/*/**.exs`
- `mix test [test_file_path]` - Runs a specific test
- `iex -S mix` Loads Elixir's interactive shell with all mix dependencies and
   is incredibly useful for experimenting your project code

