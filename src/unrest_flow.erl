%%%=============================================================================
%%% @doc Flow is a list of functions called in succession. Each function is
%%%      passed an unrest_context:context(). Possible return values for each
%%%      function are:
%%%
%%%          {ok, unrest_context:context()}
%%%                     Function completed successfully and updated the passed in
%%%                     context with new values or left it intact
%%%
%%%          {error, unrest_context:context(), ErrorData:any()}
%%%                     Function generated a non-critical error. The next
%%%                     function in chain will still be called, however, errors
%%%                     will be accumulated and will be accessible via:
%%%                         unrest_context:is_error_state/1 will return true
%%%                         unrest_context:errors/1 will return the error list
%%%
%%%          {stop_flow, unrest_context:context()}
%%%                     Interrupt the flow without generating an error. Context
%%%                     returned from this function will be treated as the final
%%%                     value of the entire flow call
%%%
%%%          {stop_flow, unrest_context:context(), ErrorData:any()}
%%%                     Interrupt the flow and generate the error. Context
%%%                     returned from this function will be treated as the final
%%%                     value of the entire flow call.
%%%                         unrest_context:is_error_state/1 will return true
%%%                         unrest_context:errors/1 will return the error list
%%%
%%%          {flow, FlowName:binary(), unrest_context:context()}
%%%                     Run the FlowName named flow. This flow must exist in the
%%%                     flows proplist in the context
%%%
%%%          {respond, Req::cowboy_req::req()}
%%%                     Interrupt the flow and return response to the client.
%%%                     Use cowboy_req:reply/2,3,4 to produce Req
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_flow).

%%_* Exports ===================================================================
-export([ run/1
        , run/2
        ]).

%%_* Types =====================================================================
-type service_spec() ::   {Module::atom(), Function::atom()}
                        | fun().

-type flow_result()  ::   {ok,        unrest_context:context()              }
                        | {error,     unrest_context:context(), Error::any()}
                        | {stop_flow, unrest_context:context()              }
                        | {stop_flow, unrest_context:context(), Error::any()}
                        | {respond,   cowboy_req:req()                      }.

-export_type([flow_result/0]).

%%_* API =======================================================================
-spec run([service_spec()]) -> {ok, unrest_context:context()}.
run(List) ->
  run(List, unrest_context:new()).

-spec run([service_spec()], unrest_context:context()) ->
                                                 {ok, unrest_context:context()}.
run([], Context) ->
  {ok, Context};
run([{Module, Fun} | Rest], Context) when is_atom(Module), is_atom(Fun) ->
  {ok, Ctx} = unrest_context:callstack_push({Module, Fun}, Context),
  try
    handle_result(Module:Fun(Ctx), Rest)
  catch
    Error:Reason ->
      {ok, Ctx1} = unrest_context:errors_push({Error, Reason}, Ctx),
      {ok, Ctx1}
  end;
run([Fun | Rest], Context) when is_function(Fun, 1) ->
  {ok, Ctx} = unrest_context:callstack_push(Fun, Context),
  try
    handle_result(Fun(Ctx), Rest)
  catch
    Error:Reason ->
      {ok, Ctx1} = unrest_context:errors_push({Error, Reason}, Ctx),
      {ok, Ctx1}
  end.

%%_* Internal ==================================================================
-spec handle_result(flow_result(), [service_spec()]) ->
                                                  {ok, unrest_context:context()}.
handle_result({flow, FlowName, Context}, _) ->
  Flows = unrest_context:get(flows, Context),
  case Flows of
    [_] ->
      Flow = unrest_context:get(FlowName, Flows),
      case Flow of
        [_] -> run(Flow, Context);
        _   -> error({undefined_flow, FlowName})
      end;
    _   ->
      error({undefined_flow, FlowName})
  end;
handle_result({respond, Req}, _) ->
  {ok, Req};
handle_result({ok, Context}, Rest) ->
  run(Rest, Context);
handle_result({error, Context, Error}, Rest) ->
  {ok, UpdatedContext} = unrest_context:errors_push(Error, Context),
  run(Rest, UpdatedContext);
handle_result({stop_flow, Context}, _) ->
  {ok, Context};
handle_result({stop_flow, Context, Error}, _) ->
  unrest_context:errors_push(Error, Context).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
