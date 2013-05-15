%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Utilities for parsing and quoting.

-module(mochiweb_util).
-author('bob@mochimedia.com').
-export([shell_quote/1, cmd/1, cmd_string/1, cmd_port/2, cmd_status/1, cmd_status/2]).

%% @spec shell_quote(string()) -> string()
%% @doc Quote a string according to UNIX shell quoting rules, returns a string
%%      surrounded by double quotes.
shell_quote(L) ->
    shell_quote(L, [$\"]).

%% @spec cmd_port([string()], Options) -> port()
%% @doc open_port({spawn, mochiweb_util:cmd_string(Argv)}, Options).
cmd_port(Argv, Options) ->
    open_port({spawn, cmd_string(Argv)}, Options).

%% @spec cmd([string()]) -> string()
%% @doc os:cmd(cmd_string(Argv)).
cmd(Argv) ->
    os:cmd(cmd_string(Argv)).

%% @spec cmd_string([string()]) -> string()
%% @doc Create a shell quoted command string from a list of arguments.
cmd_string(Argv) ->
    string:join([shell_quote(X) || X <- Argv], " ").

%% @spec cmd_status([string()]) -> {ExitStatus::integer(), Stdout::binary()}
%% @doc Accumulate the output and exit status from the given application,
%%      will be spawned with cmd_port/2.
cmd_status(Argv) ->
    cmd_status(Argv, []).

%% @spec cmd_status([string()], [atom()]) -> {ExitStatus::integer(), Stdout::binary()}
%% @doc Accumulate the output and exit status from the given application,
%%      will be spawned with cmd_port/2.
cmd_status(Argv, Options) ->
    Port = cmd_port(Argv, [exit_status, stderr_to_stdout,
                           use_stdio, binary | Options]),
    try cmd_loop(Port, [])
    after catch port_close(Port)
    end.

%% @spec cmd_loop(port(), list()) -> {ExitStatus::integer(), Stdout::binary()}
%% @doc Accumulate the output and exit status from a port.
cmd_loop(Port, Acc) ->
    receive
        {Port, {exit_status, Status}} ->
            {Status, iolist_to_binary(lists:reverse(Acc))};
        {Port, {data, Data}} ->
            cmd_loop(Port, [Data | Acc])
    end.


shell_quote([], Acc) ->
    lists:reverse([$\" | Acc]);
shell_quote([C | Rest], Acc) when C =:= $\" orelse C =:= $\` orelse
                                  C =:= $\\ orelse C =:= $\$ ->
    shell_quote(Rest, [C, $\\ | Acc]);
shell_quote([C | Rest], Acc) ->
    shell_quote(Rest, [C | Acc]).
