-module(unrest).

-export([start/0]).

%% API ------------------------------------------------------------------------

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(giallo),
    ok = application:start(lager),
    ok = application:start(unrest).
