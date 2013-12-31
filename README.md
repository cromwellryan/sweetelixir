# Sweet Elixir!
## A Gentle Introduction to Erlang’s cute younger brother Elixir

![elixir-lang][elixir-logo]

## Getting Started
1. **What is Erlang?**

  Erlang is a functional language with focuses on concurrency and fault tolerance. It first appeared in 1986 as part of Ericsson's quest to build fault tolerant, scalable telecommunication systems. It was open-sourced in 1998 and has gone on to power large portions of the worlds telecom systems, in addition to some of the most popular online services.

1. **Why Elixir?**

  Elixir is a language is built on top of the Erlang VM. Elixir exists with the goal to build concurrent, distributed, fault-tolerant systems with productive and powerful language features. Mainstream languages were constructed around limited CPUs, threading, and shared-everything state. This concurrency models can become fragile and can make concurrent programs difficult to reason about. Other languages skirt these issues by running on a single CPU with multiple processes to achieve concurrency; however, as Moore's Law pushes us towards increasing multicore CPUs, this approach becomes unmanageable. Elixir's immutable state, Actors, and processes produce a concurrency model that is easy to reason about and allows code to be written distributively without extra fanfare. Additionally, by building on top of Erlang, Elixir can take advantage of all the libraries and advantages that the Erlang ecosystem has to offer. 

1. [Setup][setup]
1. [Tools][tools]

## [Basics][basics]
1. [Numbers, atoms, strings, lists, tuples, ...][types]
1. [Functions][functions]
1. [Operators][operators]
1. [Modules][modules]

[Cheatsheet][cheetsheet]

## Advanced
1. [Records & Protocols][records_protocols]
1. [Pattern Matching][pattern_matching]
1. [Map/Reduce][map_reduce]
1. [Pipeline][pipeline]
1. [Processes][processes]

## [Common Patterns/OTP][otp]

1. Stack Server (manual)
1. GenServer, Supervisors
1. [Distributed Elixir, Tweet Aggregator][distributed_tweets]

## Web & DB

[elixir-logo]: ./elixir-logo.png
[setup]: ./setup/README.md
[tools]: ./06-tools/README.md
[basics]: ./07-basics/README.md
[pattern_matching]: ./08-advanced/pattern_matching.md
[records_protocols]: ./08-advanced/records_protocols.md
[processes]: ./08-advanced/processes.md
[map_reduce]: ./08-advanced/map_reduce.md
[pipeline]: ./08-advanced/pipeline.md
[cheetsheet]: http://media.pragprog.com/titles/elixir/ElixirCheat.pdf
[operators]: ./07-basics/README.md#operators
[functions]: ./07-basics/README.md#functions
[types]: ./07-basics/README.md#types-int-float-atom-tuple-list-binary
[modules]: ./07-basics/README.md#modules
[otp]: ./otp/README.md
[distributed_tweets]: ./otp/src/tweet_aggregator

