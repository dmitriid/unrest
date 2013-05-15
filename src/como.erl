-module(como).

-export([start/0]).

%% API ------------------------------------------------------------------------

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(giallo),
    ok = application:start(giallo_session),
    ok = application:start(como).
