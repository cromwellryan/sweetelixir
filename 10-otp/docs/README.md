# *TODO*
* Manual gen server (Chris's Stack.Custom)
* What happens when they fail (pop with empty stack?)
* Demo :global.whereis_name 

# Open Telecom Platform (OTP)
Patterns (behaviors) for structuring programs.

[Elixir-lang.org -> Getting Started -> Building OTP Applications with Mix](http://elixir-lang.org/getting_started/mix/2.html)

## Supervisor Tree
Workers = Doers
Supervisors = Monitor workers / restart worker when bad things happen

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

```
# Start our host
iex --name counter@<ip> -S mix
```

```
# Start client
iex --name client1@<ip>

Node.connect :"host@<ip>"
#> true

:gen_server.cast( {:global, :counter}, :increment )

:gen_server.call( {:global, :counter}, :stat )
```
