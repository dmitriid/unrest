%%%=============================================================================
%%% @doc Handle various configuration parameters for an unrest application
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_config).

%%_* Exports ===================================================================
-export([ get_flow/2
        , get_flow/3
        ]).

%%_* API =======================================================================
-spec get_flow(string(), binary()) -> unrest_flow:flow().
get_flow(Path, Method) ->
  get_flow(Path, Method, <<>>).
get_flow(Path, Method, ContentType) ->
  Flows = get_config(),
  io:format("Flows: ~p~n", [Flows]),
  Flow = proplists:get_value({Path, Method, ContentType}, Flows),
  unrest_flow:run(Flow).

%%_* Internal===================================================================
-spec get_config() -> list().
get_config() ->
  File   = filename:join([code:priv_dir(unrest), "config.yml"]),
  Result = yamerl_constr:file(File),
  Mappings = case Result of
               [_Document, M] -> M;
               [Map]          -> Map
             end,
  get_config(Mappings, []).

get_config([], Acc) ->
  orddict:from_list(Acc);
get_config([Config | Rest], Acc) ->
  Path        = list_to_binary(proplists:get_value("path", Config, "")),
  Method      = list_to_binary(proplists:get_value("method", Config, "")),
  ContentType = list_to_binary(proplists:get_value("content-type", Config, "")),
  Flow        = parse_flow(proplists:get_value("flow", Config)),
  get_config(Rest, [{{Path, Method, ContentType}, Flow} | Acc]).

parse_flow(Flow) ->
  parse_flow(Flow, []).

parse_flow([], Acc) ->
  lists:reverse(Acc);
parse_flow([{M, F} | Rest], Acc) ->
  parse_flow(Rest, [{list_to_atom(M), list_to_atom(F)} | Acc]).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
