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
        , do_output/1
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

-spec do_output(unrest_context:context()) -> unrest_flow:flow_result().
do_output(Ctx) ->
  io:format("Context do: ~p~n", [Ctx]),
  Trace = iolist_to_binary(io_lib:format("~p", [unrest_context:callstack(Ctx)])),
  {ok, Req0} = unrest_context:get(req, Ctx),
  {ok, Req} = cowboy_req:reply(200, [], Trace, Req0),
  {respond, Req}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
