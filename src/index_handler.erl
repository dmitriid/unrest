%%%=============================================================================
%%% @doc Entry point. SHould probably be just a static resource with index.html
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(index_handler).

-export([index_/4]).

%%_* API =======================================================================

index_(<<"GET">>, [], _Extra, Req) ->
    {output, <<"There is no REST for the wicked">>}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
