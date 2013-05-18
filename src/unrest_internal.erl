%%%=============================================================================
%%% @doc Entry point. SHould probably be just a static resource with index.html
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_internal).

-export([errands/4]).

%%_* API =======================================================================
errands(<<"GET">>, [Id], _Extra, _Req) ->
    get_errand(Id);
errands(<<"GET">>, _, _Extra, _Req) ->
    not_found.

%%_* Internal ==================================================================

get_errand(Id) ->
  Result = unrest_chain:chain( [ fun retrieve_errand/1
                               , fun update/1
                               ]
                             , [{errand_id, Id}]
                             ),
  unrest_result_handler:handle_result(Result).

retrieve_errand(Data) ->
  case proplists:get_value(errand_id, Data) of
    <<"not_found">> ->
      {error, not_found, Data};
    _  ->
      ok
  end.

update(Data) ->
  case proplists:get_value(errand_id, Data) of
    <<"conflict">>  ->
      {error, conflict, Data};
    _  ->
      ok
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
