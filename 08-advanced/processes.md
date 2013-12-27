# Processes - Elixir's Unit of Concurrency
Elixir processes are fast and lightweight units of concurrency. Not to be confused with OS processes, millions of them can be spawned on a single machine, and each are managed entirely by the Erlang VM. Processes live at the core of Elixir application architectures and can send and receive messages to other processes located locally, or remotely on another connected Node.

### spawn
Spawn creates a new process and returns the Pid, or Process ID of the new process. Messages are sent to the processes using the `<-` operator.

### Mailboxes
Processes all contain a *mailbox* where messages are passively kept until consumed via a `receive` block. `receive` processes message in the order received and allows messages to be pattern matched. A common pattern is to send a message to a process with a tuple containing `self` as the first element. This allows the receiving process to have a reference to message's "sender" and respond back to the sender Pid with its own response messages.

```elixir
iex(3)> pid = spawn fn ->
...(3)>   receive do
...(3)>     {sender, :ping} ->
...(3)>       IO.puts "Got ping"
...(3)>       sender <- :pong
...(3)>   end
...(3)> end
#PID<0.79.0>

iex(4)> pid <- {self, :ping}
{#PID<0.58.0>, :ping}
Got ping

iex(5)> receive do
...(5)>   message -> IO.puts "Got #{message} back"
...(5)> end
Got pong back
:ok
iex(6)>
```

`receive` blocks the current process until a message is received that matches a message clause. An `after` clause can optionally be provided to exit the receive loop if no messages are receive after a set amount of time.

```elixir
iex(7)> receive do
...(7)>   message -> IO.inspect message
...(7)> after 5000 ->
...(7)>   IO.puts "Timeout, giving up"
...(7)> end
Timeout, giving up
:ok
iex(8)>
```

## spawn_link
Similar to `spawn`, `spawn_link` creates a new process, but links the current and new process so that if one crashes, both processes terminate. Linking processes is essential to the Elixir and Erlang philosophy of letting programs crash instead of trying to rescue from errors. Since Elixir programs exist as a hierarchy of many processes, linking allows a predictable process dependency tree where failures in one process cascade down to all other dependent processes.

```elixir
iex(1)> pid = spawn_link fn ->
...(1)>   receive do
...(1)>     :boom -> raise "boom!"
...(1)>   end
...(1)> end
#PID<0.64.0>

iex(2)> pid <- :boom
:boom
iex(3)>
=ERROR REPORT==== 27-Dec-2013::16:49:14 ===
Error in process <0.64.0> with exit value: {{'Elixir.RuntimeError','__exception__',<<5 bytes>>},[{erlang,apply,2,[]}]}

** (EXIT from #PID<0.64.0>) {RuntimeError[message: "boom!"], [{:erlang, :apply, 2, []}]}




iex(3)> pid = spawn fn ->
...(3)>   receive do
...(3)>     :boom -> raise "boom!"
...(3)>   end
...(3)> end
#PID<0.71.0>
iex(4)> pid <- :boom
:boom

=ERROR REPORT==== 27-Dec-2013::16:49:50 ===
Error in process <0.71.0> with exit value: {{'Elixir.RuntimeError','__exception__',<<5 bytes>>},[{erlang,apply,2,[]}]}

iex(5)>
```

The first example above using `spawn_link`, we see the process termination cascade to our own iex session from the `** (EXIT from #PID<0.64.0>)` error. Our iex session stays alive because it is internally restarted by a process Supervisor. Supervisors are covered in the next section on OTP.

## Holding State
Since Elixir is immutable, you may be wondering how state is held. Holding and mutating state can be performed by spawning a process that exposes its state via messages and infinitely recurses on itself with its current state. For example:

```elixir
iex(6)> defmodule Counter do
...(6)>   def start(initial_count) do
...(6)>     spawn fn -> listen(initial_count) end
...(6)>   end
...(6)>
...(6)>   def listen(count) do
...(6)>     receive do
...(6)>       :inc -> listen(count + 1)
...(6)>       {sender, :val} ->
...(6)>         sender <- count
...(6)>         listen(count)
...(6)>     end
...(6)>   end
...(6)> end
{:module, Counter,...

iex(8)> counter_pid = Counter.start(10)
#PID<0.140.0>
iex(9)> counter_pid <- :inc
:inc
iex(10)> counter_pid <- :inc
:inc
iex(11)> counter_pid <- :inc
:inc
iex(12)> counter_pid <- {self, :val}
{#PID<0.40.0>, :val}
iex(13)> receive do
...(13)>   value -> value
...(13)> end
13
```

## Registered Processes
Pids can be registered under a name for easy lookup by other processes

```elixir
iex(26)> pid = Counter.start 10
iex(27)> Process.register pid, :count
true
iex(28)> Process.whereis(:count) == pid
true
iex(29)> :count <- :inc
:inc
iex(30)> receive do
...(30)>   value -> value
...(30)> end
11
```
