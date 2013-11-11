# Setting up your environment for Elixir support

The only prerequisite for Elixir is Erlang, version R16B or later.

Check your installed erlang version

```bash
$ erl
Erlang R16B (erts-5.10.1) ...
```


# OSX

### Install via [homebrew](http://brew.sh/)

```bash
$ brew update
$ brew install elixir
```


# Linux

### Fedora 17+ and Fedora Rawhide

```bash
$ sudo yum -y install elixir
```

### Arch Linux (on AUR)

```bash
$ yaourt -S elixir
```

### openSUSE (and SLES 11 SP3+)

  - Add Erlang devel repo with zypper ar -f obs://devel:languages:erlang/ erlang
  - Install Elixir: `$ zypper in elixir`

### Gentoo

```bash
$ emerge --ask dev-lang/elixir
```

# Windows

Install Erlang (R16B02) from the official [precompiled packages](https://www.erlang-solutions.com/downloads/download-erlang-otp).

### Direct Erlang installtion links
  - [Windows (32bit)](http://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_16.b.2-1~windows_i386.exe)
  - [Windows (64bit)](http://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_16.b.2-1~windows_amd64.exe)

### Install Elixir through [Chocolatey](http://chocolatey.org/)

```bash
> cinst elixir
```

# Test your setup

Elixir ships with three executables, `iex`, `elixir`, and `elixirc`.
Fire up `iex` to run the Elixir shell. In iex, or "Iteractive Elixir," we can
execute any valid Elixir expression and see the evaluated result.

```bash
$ iex
Erlang R16B (erts-5.10.1) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Interactive Elixir (0.10.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> IO.puts "Elixir Rocks!"
Elixir Rocks!
:ok
iex(2)>
```

