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
               [_Document, [M]] -> M;
               [[Map]]          -> Map
             end,
  io:format("MAPPINGS ~n~p~n", [Mappings]),
  Mappings.

-spec get_dispatches(list()) -> cowboy_router:dispatch_rules().
get_dispatches(Config) ->
  get_dispatches(Config, []).

-spec get_dispatches(list(), list()) -> cowboy_router:dispatch_rules().
get_dispatches([], Acc) ->
  lists:reverse(Acc);
get_dispatches([{Path, Options} | Rest], Acc) ->
  Opts = [{list_to_binary(Key), handle_list(List)} || {Key, List} <- Options],
  Dispatch = {Path, unrest_handler, Opts},
  get_dispatches(Rest, [Dispatch | Acc]).

%%
%% @doc Whe might want to handle a request exclusively ourselves, so instead of
%%      a list of options we pass in module name
%%
-spec handle_list(proplists:proplist()) -> proplists:proplist();
                 (string())             -> atom().
handle_list([{_, _}|_] = List) ->
  List;
handle_list(List) when is_list(List) ->
  list_to_atom(List).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
