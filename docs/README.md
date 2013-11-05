# Open Telecom Platform (OTP)
Patterns (behaviors) for structuring programs.

[Elixir-lang.org -> Getting Started -> Building OTP Applications with Mix](http://elixir-lang.org/getting_started/mix/2.html)

## Supervisor Tree
Workers = Doers
Supervisors = Monitor workers / restart worker when bad things happen

![Supervisor Tree](images/supervisor_tree.png)

## Behaviors
Contractual callbacks specific to your application.

* Supervisor
* GenServer
* Application

## GenServer.Behavior

handle_cast // async - no response

handle_call // sync - response
-- Replies --
