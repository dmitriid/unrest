%%%=============================================================================
%%% @doc Generic result handler for resources
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_result_handler).
-compile([{parse_transform, lager_transform}]).

-export([handle_result/1]).

%%_* API =======================================================================

handle_result({error, not_found, Data}) ->
  lager:error("Requested data not found: ~p~n", [Data]),
  not_found;
handle_result({error, conflict, Data}) ->
  lager:error("Conflict in updating data: ~p~n", [Data]),
  {error, 409};
handle_result({ok, Data}) ->
  {json, Data}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
