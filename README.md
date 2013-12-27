# Sweet Elixir!
## A Gentle Introduction to Erlang’s cute younger brother Elixir

![elixir-lang][elixir-logo]

## Getting Started
1. What is Erlang?
1. What is Elixir?
1. Why?
1. [Setup][setup]
1. [Tools][tools]

## How to use
Most of this guide will use `iex` (interactive elixir).  We'll try to let you know when we don't, but here are some guidelines...

* `iex> command` means we're executing `command` in interactive elixir.
* `$> elixir <filename>` means run the `elixir` command at terminal to execute the file.  These files will most likely be an `.exs`  or elixir script file.  These will usually be found within the `src/` folder for that particular module.

## [Basics][basics]
1. Numbers, atoms, strings, lists, tuples, ...
1. Functions
1. [Operators][operators]
1. [Modules][modules]

[Cheatsheet][cheetsheet]

## Advanced
1. [Records & Protocols][records_protocols]
1. [Pattern Matching][pattern_matching]
1. [Map/Reduce][map_reduce]
1. [Pipeline][pipeline]
1. Processes

## [Common Patterns/OTP][otp]

1. Stack Server (manual)
1. GenServer, Supervisors

## Distributed Elixir

## Web & DB

[elixir-logo]: ./elixir-logo.png
[setup]: ./04-setup/README.md
[tools]: ./06-tools/README.md
[basics]: ./07-basics/README.md
[otp]: ./10-otp/README.md
[pattern_matching]: ./08-advanced/pattern_matching.md
[records_protocols]: ./08-advanced/records_protocols.md
[map_reduce]: ./08-advanced/map_reduce.md
[pipeline]: ./08-advanced/pipeline.md
[cheetsheet]: http://media.pragprog.com/titles/elixir/ElixirCheat.pdf
[operators]: ./07-basics/README.md#operators
[modules]: ./07-basics/README.md#modules
