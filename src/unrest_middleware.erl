%%%=============================================================================
%%% @doc Unrest middleware handles the control flow
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
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
  {handler_opts, Arguments} = lists:keyfind(handler_opts, 1, Env),
  handle_request(Arguments, Req, Env).


%%_* Internal ==================================================================

handle_request(Options, Req, Env) ->
  Config = proplists:get_value(config, Options),
  Flows  = proplists:get_value(flows, Options),
  {Method, _} = cowboy_req:method(Req),
  MethodOptions = proplists:get_value(Method, Config),
  run_flow(MethodOptions, Req, Env, Flows).

run_flow(undefined, Req, _Env, _Flows) ->
  {ok, Req2} = cowboy_req:reply(400, [], "Invalid request!", Req),
  {halt, Req2};
run_flow(Module, Req, Env0, _Flows) when is_atom(Module) ->
  Env = lists:keyreplace(handler, 1, Env0, {handler, Module}),
  {ok, Req, Env};
run_flow(Options, Req0, Env0, Flows) ->
  case proplists:get_value(<<"__flow__">>, Options) of
    Module when is_atom(Module) ->
      Env = lists:keyreplace(habdler, 1, Env0, {handler, Module}),
      {ok, Req0, Env};
    Flow when is_list(Flow) ->
      io:format("FLOW ~p~n~n~n", [Flow]),
      Config = [E || E = {K, _} <- Options, K /= <<"__flow__">>],
      Ctx = unrest_context:new([ {req, Req0}
                               , {flows, Flows}
                               | Config
                               ]),
      {ok, Req} = unrest_flow:run(Flow, Ctx),
      {halt, Req}
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
