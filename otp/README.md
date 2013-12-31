# Open Telecom Platform (OTP)
Patterns (behaviors) for structuring programs.

[Elixir-lang.org -> Getting Started -> Building OTP Applications with Mix](http://elixir-lang.org/getting_started/mix/2.html)

## Holding state

- [Custom Stack Server](src/genserver_stack_example)
- [GenServer Stack Server](src/genserver_stack_example)

## Supervisor Tree
Supervisors = Monitor workers / restart worker when bad things happen

Workers = Doers

![Supervisor Tree](images/supervisor_tree.png)

## Behaviors
Contractual callbacks specific to your application.

* **Supervisor**
* **GenServer**
* GenFSM
* GenEvent
* **Application**

## Supervisor.Behavior

#### Strategies
* *one_for_one* 1 failure = 1 restart
* *one_for_all* 1 failure = all restart
* *rest_for_one* 1 failure = all restart

## GenServer.Behavior

``init(initial_state)``

``handle_cast``
Asyncronous fire and forget call

``handle_call``
Syncronous call that (usually) implies a return value.

``start_link``
Called by the supervisor to get things running and return a pid for it to monitor

## Running

#### Locally
```
iex -S mix

:gen_server.call(:counter, :stat)
#> 0

:gen_server.cast(:counter, :increment)
:gen_server.cast(:counter, :increment)

:gen_server.call(:counter, :stat)
#> 2

# worker blows up
:gen_server.cast(:counter, :boom)

:gen_server.call(:counter, :stat)
#> 0
```

#### Distributed

*Note:* Erlang/Elixir communicates over port 4369 by default.  On locked down networks like public wifi run `epmd -port 80 -daemon` to tell Erlang/Elixir to communicate over port 80.

```
# Start our host
mix --name stack@<ip> --cookie foo
```

```
# Start client
iex --name lastname@<ip> foo --cookie bar

Node.connect :"stack@<ip>"
#> true

:gen_server.cast( {:global, :counter}, :increment )

:gen_server.call( {:global, :counter}, :stat )
```
