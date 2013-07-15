%%%=============================================================================
%%% @doc A "service" is just a function that accepts an unrest_context and
%%%      outputs result that unrest_flow can understand
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_service).

%%_* Exports ===================================================================
-export([ get/1
        , patch/1
        , post/1
        , output/1
        ]).

%%_* API =======================================================================
-spec get(unrest_context:context()) -> unrest_flow:flow_result().
get(Ctx) ->
  io:format("Context GET: ~p~n", [Ctx]),
  {ok, Ctx}.

-spec patch(unrest_context:context()) -> unrest_flow:flow_result().
patch(Ctx) ->
  io:format("Context PATCH: ~p~n", [Ctx]),
  {ok, Ctx}.

-spec post(unrest_context:context()) -> unrest_flow:flow_result().
post(Ctx) ->
  io:format("Context POST: ~p~n", [Ctx]),
  {ok, Ctx}.

-spec output(unrest_context:context()) -> unrest_flow:flow_result().
output(Ctx) ->
  io:format("Context OUTPUT: ~p~n", [Ctx]),
  Trace = iolist_to_binary(io_lib:format("~p", [unrest_context:callstack(Ctx)])),
  {ok, Req0} = unrest_context:get(req, Ctx),
  {ok, Req} = cowboy_req:reply(200, [], Trace, Req0),
  {respond, Req}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
