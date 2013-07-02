%%%=============================================================================
%%% @doc Handle various configuration parameters for an unrest application
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_config).

%%_* Exports ===================================================================
-export([ get_dispatch/0
        ]).

%%_* API =======================================================================
-spec get_dispatch() -> unrest_flow:flow().
get_dispatch() ->
  Config = get_config(),
  DispatchList = get_dispatches(Config),
  [
   %% {HostMatch, list({PathMatch, Handler, Opts})}
   {'_', DispatchList}
  ].

%%_* Internal===================================================================
-spec get_config() -> list().
get_config() ->
  File   = filename:join([code:priv_dir(unrest), "config.yml"]),
  Result = yamerl_constr:file(File),
  Mappings = case Result of
               [_Document, M] -> M;
               Map            -> Map
             end,
  io:format("MAPPINGS ~n~p~n~n~n", [Mappings]),
  Mappings.

-spec get_dispatches(list()) -> cowboy_router:dispatch_rules().
get_dispatches(Config0) ->
  {Flows, Config} = get_named_flows(Config0),
  get_dispatches(Config, Flows, []).

-spec get_dispatches(list(), list(), list()) -> cowboy_router:dispatch_rules().
get_dispatches([], _, Acc) ->
  lists:reverse(Acc);
get_dispatches([{Path, Options} | Rest], Flows, Acc) ->
  Opts = handle_methods(Options, Flows),
  Dispatch = {Path, unrest_handler, Opts},
  get_dispatches(Rest, Flows, [Dispatch | Acc]).

get_named_flows(Config0) ->
  {_, Flows0} = lists:keyfind("__flows__", 1, Config0),
  io:format("Flows ~p~n~n~n", [Flows0]),
  Flows1 = expand_flows(Flows0),
  io:format("Expanded Flows ~p~n~n~n", [Flows1]),
  Config = lists:keydelete("__flows__", 1, Config0),
  {Flows1, Config}.

expand_flows(Flows) ->
  expand_flows(Flows, []).

expand_flows([], Acc) ->
  lists:reverse(Acc);
expand_flows([Flow0 | Rest], Acc) ->
  Flow = expand_flow(Flow0, Rest, Acc),
  expand_flows(Rest, [Flow | Acc]).

expand_flow({Key0, [{_,_} | _] = Values0}, NonExpanded, Expanded) ->
  Key    = list_to_binary(Key0),
  Values = expand_flow_values(Values0, NonExpanded, Expanded),
  {Key, Values};
expand_flow({Key, Module}, _, _) ->
  {list_to_binary(Key), list_to_atom(Module)}.

expand_flow_values(Values, NonExpanded, Expanded) ->
  lists:flatten([expand_flow_value(V, NonExpanded, Expanded) || V <- Values]).

expand_flow_value({"__flow__", [Name | _] = Flows}, NonExpanded, Expanded)
  when is_list(Name) ->
  [get_flow_by_name(NamedFlow, NonExpanded, Expanded) || NamedFlow <- Flows];
expand_flow_value({"__flow__", Name}, NonExpanded, Expanded) ->
  get_flow_by_name(Name, NonExpanded, Expanded);
expand_flow_value({Module, [Fun | _] = Funs}, _, _) when is_list(Fun) ->
  [{list_to_atom(Module), list_to_atom(F)} || F <- Funs];
expand_flow_value({Module, Fun}, _, _) ->
  {list_to_atom(Module), list_to_atom(Fun)};
expand_flow_value(F, _, _) ->
  F.

get_flow_by_name(Name, NonExpanded, Expanded) ->
  case lists:keyfind(Name, 1, NonExpanded) of
    {_, _} = NonExpandedFlow ->
      {_, Flow} = expand_flow(NonExpandedFlow, NonExpanded, Expanded),
      Flow;
    _ -> case lists:keyfind(list_to_binary(Name), 1, Expanded) of
           {_, ExpandedFlow} -> ExpandedFlow;
           _                 -> error({flow_not_found, Name})
         end
  end.

handle_methods(Methods, Flows) ->
  [handle_method(Method, Flows) || Method <- Methods].

handle_method({Method, List}, Flows) ->
  case io_lib:printable_list(List) of
    true  ->
      {maybe_to_binary(Method), list_to_atom(List)};
    false ->
      {maybe_to_binary(Method), handle_options(List, Flows)}
  end.

handle_options(Options, Flows) ->
  io:format("OPTIONS ~p~n~n~n", [Options]),
  [handle_option(Option, Flows) || Option <- Options].

handle_option({"__flow__", Flow0}, ExpandedFlows) ->
  Flow = expand_flow_values(Flow0, [], ExpandedFlows),
  {<<"__flow__">>, Flow};
handle_option(O, _) ->
  O.

maybe_to_binary(V) ->
  try list_to_binary(V)
  catch _:_ -> V
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
