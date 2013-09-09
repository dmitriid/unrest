%%%=============================================================================
%%% @doc Handle various configuration parameters for an unrest application
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(unrest_config).
-compile([{parse_transform, lager_transform}]).
-compile(export_all).

%%_* Exports ===================================================================
-export([ get_dispatch/0
        , get_dispatch/1
        , get_dispatch/2
        ]).

%%_* Includes ==================================================================

-include_lib("eunit/include/eunit.hrl").

%%_* API =======================================================================
-spec get_dispatch() -> unrest_flow:flow().
get_dispatch() ->
  File = filename:join([code:priv_dir(unrest), "config.yml"]),
  get_dispatch(File).

-spec get_dispatch( HostsOrFiles :: [{string(), file_or_files()}]
                                  | file_or_files()
                  ) -> list().
-type file_or_files() :: string() | [string()].

get_dispatch([{_, _} | _] = HostFiles) ->
  lists:flatten([  get_dispatch(Host, FileOrFiles)
                || {Host, FileOrFiles} <- HostFiles
                ]
               );
get_dispatch(FileOrFiles) ->
  get_dispatch('_', FileOrFiles).

-spec get_dispatch(string() | '_', string() | [string()]) -> unrest_flow:flow().
get_dispatch(Host, FileOrFiles) ->
  [{Host, get_dispatches(get_config(FileOrFiles))}].

%%_* Internal ==================================================================
-spec get_config(string() | [string()]) -> list().
get_config([F|_] = Files) when is_list(F)->
  Results = [  get_configuration(yamerl_constr:file(File, [{str_node_as_binary, true}]))
            || File <- Files
            ],
  merge_results(Results);
get_config(File) ->
  Result = yamerl_constr:file(File, [{str_node_as_binary, true}]),
  get_configuration(Result).

get_configuration([Frontmatter, Mapping]) when is_binary(Frontmatter) ->
  Mapping;
get_configuration([Mapping]) when is_list(Mapping) ->
  Mapping.

-spec get_dispatches(list()) -> cowboy_router:dispatch_rules().
get_dispatches(Config0) ->
  {Flows, Config} = get_named_flows(Config0),
  get_dispatches(Config, Flows, []).

-spec get_dispatches(list(), list(), list()) -> cowboy_router:dispatch_rules().
get_dispatches([], _, Acc) ->
  lists:reverse(Acc);
get_dispatches([{Path, Options} | Rest], Flows, Acc) ->
  Opts = handle_methods(Options, Flows),
  Dispatch = {Path, unrest_handler, [{config, Opts}, {flows, Flows}]},
  get_dispatches(Rest, Flows, [Dispatch | Acc]).

get_named_flows(Config0) ->
  case lists:keyfind(<<"__flows__">>, 1, Config0) of
    {_, Flows0} ->
      Flows1 = expand_flows(Flows0),
      Config = lists:keydelete(<<"__flows__">>, 1, Config0),
      {Flows1, Config};
    false      ->
      {[], Config0}
  end.

expand_flows(Flows) ->
  expand_flows(Flows, []).

expand_flows([], Acc) ->
  lists:reverse(Acc);
expand_flows([Flow0 | Rest], Acc) ->
  Flow = expand_flow(Flow0, Rest, Acc),
  expand_flows(Rest, [Flow | Acc]).

%%
%% @doc Handle two possible deifintions for a flow:
%%          FLOW_NAME:
%%              list of modules
%%              and flows
%%          FLOW_NAME: module_name
%%
-spec expand_flow( { FlowName::binary()
                   , [{binary(), binary()}] | binary()
                   }
                 , list(), list()) -> {binary(), list()}.
expand_flow({Key, Values0}, NonExpanded, Expanded) when is_list(Values0) ->
  Values = expand_flow_values(Values0, NonExpanded, Expanded),
  {Key, Values};
expand_flow({Key, Module}, _, _) when is_binary(Module)->
  {Key, unrest_util:binary_to_atom(Module)}.

expand_flow_values(Values, NonExpanded, Expanded) ->
  lists:flatten([expand_flow_value(V, NonExpanded, Expanded) || V <- Values]).

%%
%% @doc A flow value is one of:
%%        - A list of flows in a __flow__
%%        - A single flow name in a __flow__
%%        - A list of parameters
%%        - A module:function
%%
-spec expand_flow_value({binary(), list() | binary()}, list(), list()) ->
                                                       list() | tuple() | any().
expand_flow_value({<<"__flow__">>, [Name | _] = Flows}, NonExpanded, Expanded)
  when is_binary(Name) ->
  [get_flow_by_name(NamedFlow, NonExpanded, Expanded) || NamedFlow <- Flows];
expand_flow_value({<<"__flow__">>, Name}, NonExpanded, Expanded) ->
  get_flow_by_name(Name, NonExpanded, Expanded);
expand_flow_value({Module, [Fun | _] = Funs}, _, _) when is_binary(Fun) ->
  [  {unrest_util:binary_to_atom(Module), unrest_util:binary_to_atom(F)}
  || F <- Funs];
expand_flow_value({Module, Fun}, _, _) ->
  {unrest_util:binary_to_atom(Module), unrest_util:binary_to_atom(Fun)};
expand_flow_value(F, _, _) ->
  F.

get_flow_by_name(Name, NonExpanded, Expanded) ->
  case lists:keyfind(Name, 1, NonExpanded) of
    {_, _} = NonExpandedFlow ->
      {_, Flow} = expand_flow(NonExpandedFlow, NonExpanded, Expanded),
      Flow;
    _ -> case lists:keyfind(Name, 1, Expanded) of
           {_, ExpandedFlow} ->
             ExpandedFlow;
           _                 ->
             Flows = [E || {E, _} <- Expanded] ++ [N || {N, _} <- NonExpanded],
             lager:error( "Undefined flow ~p in ~p. Available flows: ~p"
                        , [Name, ?MODULE, Flows]),
             error({flow_not_found, Name})
         end
  end.

%%
%% @doc Handle configurations for methods defined per route (GET/POST/PATCH etc.)
%%
handle_methods(Methods, Flows) ->
  [handle_method(Method, Flows) || Method <- Methods].


%%
%% @doc A method may contain either a list of configuration parameters or a
%%      single module name
%%
-spec handle_method({binary(), binary() | list()}, list()) ->
                                                     {binary(), atom() | list()}.
handle_method({Method, Module}, _) when is_binary(Module) ->
  {Method, unrest_util:binary_to_atom(Module)};
handle_method({Method, List}, Flows) when is_list(List) ->
  {Method, handle_options(List, Flows)}.

%%
%% @doc Handle config parameters for a single method
%%
handle_options(Options, Flows) ->
  [handle_option(Option, Flows) || Option <- Options].

%%
%% @doc An option can be the special value __flow__ which needs to be expanded
%%      into an actual flow. We leave the rest of options intact
%%
-spec handle_option({binary(), list()} | any(), list()) ->
                                                      {binary(), list()} | any().
handle_option({<<"__flow__">>, Flow0}, ExpandedFlows) ->
  Flow = expand_flow_values(Flow0, [], ExpandedFlows),
  {<<"__flow__">>, Flow};
handle_option(O, _) ->
  O.


%%
%% @doc Given a list of parsed results from different files, merge them
%%      This means:
%%        - flows are appended to a single __flow__
%%        - other keys are just appended
%%
-spec merge_results([proplists:proplist()]) -> proplists:proplist().
merge_results(Results) ->
  {Paths, Flows} = lists:foldl(fun merge_results/2, {[], []}, Results),
  [{<<"__flows__">>, lists:flatten(Flows)} | lists:flatten(Paths)].

merge_results(Result, {Paths, Flows}) ->
  ResultPaths = [Path || {Key, _} = Path <- Result, Key =/= <<"__flows__">>],
  ResultFlows = proplists:get_value(<<"__flows__">>, Result, []),
  {[ResultPaths | Paths], [ResultFlows | Flows]}.

%%_* Unit tests ================================================================
-ifdef(EUNIT).

handle_option_test() ->
  Flow = [{test_module, test_function}, {test_module_2, test_function_2}],
  ExpandedFlows = [{<<"test">>, Flow}],

  Result1 = handle_option( {<<"__flow__">>, [{<<"__flow__">>, <<"test">>}]}
                         , ExpandedFlows
                         ),
  ?assertEqual({<<"__flow__">>, Flow}, Result1),

  Result2 = (catch handle_option( {<<"__flow__">>, [{<<"__flow__">>, <<"test">>}]}
                                 , []
                                )
            ),
  ?assertMatch({'EXIT', {{flow_not_found, <<"test">>}, _}}, Result2),

  Result3 = handle_option({a, b}, []),
  ?assertEqual({a, b}, Result3).

handle_method_test() ->
  Result1 = handle_method({<<"GET">>, <<"module">>}, []),
  ?assertEqual({<<"GET">>, module}, Result1),

  Result2 = handle_method({<<"GET">>, [{a, b}]}, []),
  ?assertEqual({<<"GET">>, [{a,b}]}, Result2).

get_flow_by_name_test() ->
  NonExpandedFlow = {<<"nonex">>, [{<<"test">>, <<"f">>}]},
  ExpandedFlow = {<<"ex">>, [{test, f}]},

  Result1 = get_flow_by_name(<<"nonex">>, [NonExpandedFlow], []),
  ?assertEqual([{test, f}], Result1),

  Result2 = get_flow_by_name(<<"ex">>, [], [ExpandedFlow]),
  ?assertEqual([{test, f}], Result2),

  Result3 = (catch get_flow_by_name(any, [], [])),
  ?assertMatch({'EXIT', {{flow_not_found, any}, _}}, Result3).

expand_flow_value_test() ->
  ExpandedFlow = {<<"ex">>, [{test, f}]},

  Result1 = expand_flow_value({<<"__flow__">>, [<<"ex">>]}, [], [ExpandedFlow]),
  ?assertEqual([[{test, f}]], Result1),

  Result2 = expand_flow_value({<<"__flow__">>, <<"ex">>}, [], [ExpandedFlow]),
  ?assertEqual([{test, f}], Result2),

  Result3 = expand_flow_value({<<"module">>, [<<"f1">>, <<"f2">>]}, [], []),
  ?assertEqual([{module, f1}, {module, f2}], Result3),

  Result4 = expand_flow_value({<<"module">>, <<"f">>}, [], []),
  ?assertEqual({module, f}, Result4),

  Result5 = (catch expand_flow_value({<<"__flow__">>, any}, [], [])),
  ?assertMatch({'EXIT', {{flow_not_found, any}, _}}, Result5).

expand_flow_test() ->
  ExpandedFlow = {<<"ex">>, [{test, f}]},
  NonExpandedFlow = {<<"nonex">>, [{<<"test2">>, <<"f2">>}]},

  Flow = [ {<<"module1">>, [<<"f1">>, <<"f2">>]}
         , {<<"module2">>, <<"f">>}
         , {<<"__flow__">>, [<<"nonex">>, <<"ex">>]}
         , {<<"__flow__">>, <<"ex">>}
         ],
  ResultFlow = [ {module1, f1}
               , {module1, f2}
               , {module2, f}
               , {test2, f2}
               , {test, f}
               , {test, f}
               ],
  Result1 = expand_flow({<<"flow">>, Flow}, [NonExpandedFlow], [ExpandedFlow]),
  ?assertEqual({<<"flow">>, ResultFlow}, Result1),

  Result2 = (catch expand_flow({<<"flow">>, Flow}, [], [])),
  ?assertMatch({'EXIT', {{flow_not_found, <<"nonex">>}, _}}, Result2).

get_named_flows_test() ->
  Config = [{ <<"__flows__">>
            , [ {<<"flow1">>, [{<<"module1">>, <<"f1">>}]}
              , {<<"flow2">>, [{<<"__flow__">>, <<"flow1">>}]}
              , {<<"flow3">>, [{<<"__flow__">>, [<<"flow1">>, <<"flow2">>]}]}
              ]
            }
           ],

  ResultFlow = [ {<<"flow1">>, [{module1, f1}]}
               , {<<"flow2">>, [{module1, f1}]}
               , {<<"flow3">>, [{module1, f1}, {module1, f1}]}
               ],
  Result1 = get_named_flows(Config),
  ?assertEqual({ResultFlow, []}, Result1),

  Result2 = get_named_flows([]),
  ?assertEqual({[], []}, Result2).

get_configuration_test() ->
  Result1 = get_configuration([<<"stuff">>, [mapping]]),
  ?assertEqual([mapping], Result1),

  Result2 = get_configuration([[mapping]]),
  ?assertEqual([mapping], Result2).

merge_result_test() ->
  Result1 = [ {<<"/">>, [{a, b}]}
            , {<<"__flows__">>, [{<<"flow1">>, [{aa, bb}]}]
              }
            ],
  Result2 = [ {<<"/b">>, [{c, d}]}
            , {<<"__flows__">>, [{<<"flow2">>, [{cc, dd}]}]
              }
            ],
  Result = merge_results([Result1, Result2]),
  ?assertEqual( [ {<<"__flows__">>, [ {<<"flow2">>, [{cc, dd}]}
                                    , {<<"flow1">>, [{aa, bb}]}
                                    ]
                  }
                , {<<"/b">>, [{c, d}]}
                , {<<"/">>, [{a, b}]}
                ]
              , Result
              ).
-endif. %% EUNIT

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
