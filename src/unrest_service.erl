%%%=============================================================================
%%% @doc A "service" is just a function that accepts an unrest_context and
%%%      outputs result that unrest_flow can understand
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_service).

%%_* Exports ===================================================================
-export([ output_get/1
        , output_post/1
        ]).

%%_* API =======================================================================
-spec output_get(unrest_context:context()) -> unrest_flow:flow_result().
output_get(Ctx) ->
  io:format("Context get: ~p~n", [Ctx]),
  {ok, Ctx}.

-spec output_post(unrest_context:context()) -> unrest_flow:flow_result().
output_post(Ctx) ->
  io:format("Context post: ~p~n", [Ctx]),
  {ok, Ctx}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
