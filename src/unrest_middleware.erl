%%%=============================================================================
%%% @doc Unrest middleware handles the control flow
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_middleware).

%%_* Exports ===================================================================
-export([ execute/2
        ]).

%%_* API =======================================================================
-spec execute(Req0, Env) ->
    {ok, Req, Env} | {error, 500, Req} | {halt, Req} when
    Req0 :: cowboy_req:req()
    ,Req :: cowboy_req:req()
    ,Env :: cowboy_middleware:env().
execute(Req, Env) ->
  io:format("Req ~p~n, Env ~p~n", [Req, Env]),
  {handler, Handler}        = lists:keyfind(handler, 1, Env),
  {handler_opts, Arguments} = lists:keyfind(handler_opts, 1, Env),
  io:format("Handler ~p~n, Args ~p~n", [Handler, Arguments]),
  {ok, Req, Env}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
