%%%=============================================================================
%%% @doc Function chaining
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_chain).

-export([chain/2]).

%%_* API =======================================================================

chain(List, Data) ->
  lists:foldl(fun process/2, {ok, Data}, List).


%%_* Internal ==================================================================

process(_, {error, _, _} = Error) ->
  Error;
process(F, {ok, Data}) ->
  case F(Data) of
    {error, Reason, UpdatedData} -> {error, Reason, UpdatedData};
    {error, Reason}              -> {error, Reason, Data};
    {ok, Result}                 -> {ok, [Result | Data]};
    ok                           -> {ok, Data}
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
